{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module EulerHS.KVConnector.Utils where

import           EulerHS.Prelude
import qualified Data.Aeson as A
import           Data.Aeson.Encode.Pretty (encodePretty)
import qualified Database.Beam as B
import qualified Database.Beam.Schema.Tables as B
import qualified Data.ByteString.Lazy as BSL
import           Text.Casing (quietSnake)
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import           EulerHS.KVConnector.DBSync (meshModelTableEntityDescriptor, toPSJSON)
import           EulerHS.KVConnector.Types (MeshMeta(..), MeshResult, MeshError(..), MeshConfig, KVConnector(..), PrimaryKey(..), SecondaryKey(..))
import EulerHS.KVConnector.InMemConfig.Types  (IMCEnabledTables(..),IsIMCEnabled(..))
import qualified EulerHS.Language as L
import           EulerHS.Extra.Language (getOrInitSqlConn)
import           EulerHS.SqlDB.Types (BeamRunner, BeamRuntime, DBConfig, DBError)
-- import           Servant (err500)
import           Sequelize (fromColumnar', columnize, Model, Where, Clause(..), Term(..), Set(..))
import           System.Random (randomRIO)
import           Unsafe.Coerce (unsafeCoerce)
import Data.Time.LocalTime (addLocalTime, LocalTime)
import Data.Time.Clock (secondsToNominalDiffTime)
import           Juspay.Extra.Config (lookupEnvT)
import qualified Data.Fixed as Fixed
import qualified Data.Serialize as Serialize
import qualified Data.Serialize as Cereal
import           Data.Either.Extra (mapRight, mapLeft)
import  EulerHS.KVConnector.Encoding() 


isInMemConfigEnabled :: (L.MonadFlow m) => Text -> m Bool
isInMemConfigEnabled modelName = do
  (mbIMCEnabledTables :: Maybe [Text]) <- L.getOptionLocal IMCEnabledTables
  (mbIsIMCEnabled :: Maybe Bool) <- L.getOptionLocal IsIMCEnabled
  case (mbIsIMCEnabled, mbIMCEnabledTables) of
    (Just isEnabled, Just enabledTables)  -> do
      L.logDebugT "IsIMCEnabled" (show isEnabled)
      L.logDebugT "IMCEnabledTables" (show enabledTables)
      L.logDebugT "modelName" modelName
      L.logDebugT "IsModelNameElem" (show $ elem modelName enabledTables)
      return $ isEnabled && (elem modelName enabledTables)
    (Nothing, Nothing)         -> L.logErrorT "IS_IMC_ENABLED_ERROR" "Error IsIMCEnabled and IMCEnabledTables are not set" $> False
    (Nothing, _)               -> L.logErrorT "IS_KV_ENABLED_ERROR" "Error IsIMCEnabled is not set" $> False
    (_, Nothing)               -> L.logErrorT "IS_KV_ENABLED_ERROR" "Error IMCEnabledTables is not set" $> False

jsonKeyValueUpdates ::
  forall be table. (Model be table, MeshMeta be table)
  => [Set be table] -> [(Text, A.Value)]
jsonKeyValueUpdates = fmap jsonSet

jsonSet ::
  forall be table.
  (Model be table, MeshMeta be table) =>
  Set be table -> (Text, A.Value)
jsonSet (Set column value) = (key, modifiedValue)
  where
    key = B._fieldName . fromColumnar' . column . columnize $
      B.dbTableSettings (meshModelTableEntityDescriptor @table @be)
    modifiedValue = A.toJSON value

jsonSet (SetDefault _) = error "Default values are not supported"

-- | Update the model by setting it's fields according the given
--   key value mapping.
updateModel :: forall be table.
  ( MeshMeta be table,
    ToJSON (table Identity)
  ) =>
  table Identity -> [(Text, A.Value)] -> MeshResult A.Value
updateModel model updVals = do
  let updVals' = map (\(key,v) -> (key, Map.findWithDefault id key (valueMapper @be @table) v)) updVals
  case A.toJSON model of
    A.Object o -> Right (A.Object $ foldr (uncurry HM.insert) o updVals')
    o -> Left $ MUpdateFailed
      ("Failed to update a model. Expected a JSON object but got '" <>
        (decodeUtf8 . BSL.toStrict . encodePretty $ o) <>
        "'.")

runQuery ::
  ( HasCallStack,
    BeamRuntime be beM, BeamRunner beM,
    L.MonadFlow m
  ) =>
  DBConfig beM -> L.SqlDB beM a -> m (Either DBError a)
runQuery dbConf query = do
  conn <- getOrInitSqlConn dbConf
  case conn of
    Right c -> L.runDB c query
    Left  e -> return $ Left e


getDataFromRedisForPKey ::forall table m. (
    KVConnector (table Identity),
    FromJSON (table Identity),
    Serialize.Serialize (table Identity),
    L.MonadFlow m) => MeshConfig -> Text -> m (MeshResult (Maybe (Text, table Identity))) 
getDataFromRedisForPKey meshCfg pKey = do
  res <- L.runKVDB meshCfg.kvRedis $ L.get (fromString $ T.unpack $ pKey)
  case res of
    Right (Just r) ->
      case decodeToField $ BSL.fromChunks [r] of
        Right [decodeRes] -> do
            return . Right . Just $ (pKey, decodeRes)
        Right _ -> return . Right $ Nothing   -- Something went wrong
        Left e -> return $ Left e
    Right Nothing -> do
      let traceMsg = "redis_fetch_noexist: Could not find key: " <> show pKey
      L.logWarningT "getCacheWithHash" traceMsg
      return $ Right Nothing
    Left e -> return $ Left $ MRedisError e

getDataFromPKeysRedis :: forall table m. (
    KVConnector (table Identity),
    FromJSON (table Identity),
    Serialize.Serialize (table Identity),
    L.MonadFlow m) => MeshConfig -> [ByteString] -> m (MeshResult [table Identity])
getDataFromPKeysRedis _ [] = pure $ Right []
getDataFromPKeysRedis meshCfg (pKey : pKeys) = do
  res <- L.runKVDB meshCfg.kvRedis $ L.get (fromString $ T.unpack $ decodeUtf8 pKey)
  case res of
    Right (Just r) ->
      case decodeToField $ BSL.fromChunks [r] of
        Right decodeRes -> do
            remainingPKeysRes <- getDataFromPKeysRedis meshCfg pKeys
            pure $ mapRight (decodeRes ++) remainingPKeysRes
        Left e -> return $ Left e
    Right Nothing -> do
      let traceMsg = "redis_fetch_noexist: Could not find key: " <> show pKey
      L.logWarningT "getCacheWithHash" traceMsg
      getDataFromPKeysRedis meshCfg pKeys
    Left e -> return $ Left $ MRedisError e

------------- KEY UTILS ------------------

keyDelim:: Text
keyDelim = "_"

getPKeyWithShard :: forall table. (KVConnector (table Identity)) => table Identity -> Text
getPKeyWithShard table =
  let pKey = getLookupKeyByPKey table
  in pKey <> getShardedHashTag pKey

getLookupKeyByPKey :: forall table. (KVConnector (table Identity)) => table Identity -> Text
getLookupKeyByPKey table = do
  let tName = tableName @(table Identity)
  let (PKey k) = primaryKey table
  let lookupKey = getSortedKey k
  tName <> keyDelim <> lookupKey

getSecondaryLookupKeys :: forall table. (KVConnector (table Identity)) => table Identity -> [Text]
getSecondaryLookupKeys table = do
  let tName = tableName @(table Identity)
  let skeys = secondaryKeys table
  let tupList = map (\(SKey s) -> s) skeys
  let list = map (\x -> tName <> keyDelim <> getSortedKey x ) tupList
  list

applyFPair :: (t -> b) -> (t, t) -> (b, b)
applyFPair f (x, y) = (f x, f y)

getSortedKey :: [(Text,Text)] -> Text
getSortedKey kvTup = do
  let sortArr = sortBy (compare `on` fst) kvTup
  let (appendedKeys, appendedValues) = applyFPair (T.intercalate "_") $ unzip sortArr
  appendedKeys <> "_" <> appendedValues

getShardedHashTag :: Text -> Text
getShardedHashTag key = do
  let slot = unsafeCoerce @_ @Word16 $ L.keyToSlot $ encodeUtf8 key
      streamShard = slot `mod` 128
  "{shard-" <> show streamShard <> "}"

------------------------------------------

getAutoIncId :: (L.MonadFlow m) => MeshConfig -> Text -> m (MeshResult Integer)
getAutoIncId meshCfg tName = do
  let key = (T.pack . quietSnake . T.unpack) tName <> "_auto_increment_id"
  mId <- L.runKVDB meshCfg.kvRedis $ L.incr $ encodeUtf8 key
  case mId of
    Right id_ -> return $ Right id_
    Left e    -> return $ Left $ MRedisError e

unsafeJSONSet :: forall a b. (ToJSON a, FromJSON b, ToJSON b) => Text -> a -> b -> b
unsafeJSONSet field value obj =
  case A.toJSON obj of
    A.Object o -> do
      if HM.member field o
        then obj
        else do
          let jsonVal = A.toJSON value
              newObj = A.Object (HM.insert field jsonVal o)
          case resultToEither $ A.fromJSON newObj of
            Right r -> r
            Left e  -> error e
    _ -> error "Can't set  value of JSON which isn't a object."

foldEither :: [Either a b] -> Either a [b]
foldEither [] = Right []
foldEither ((Left a) : _) = Left a
foldEither ((Right b) : xs) = mapRight ((:) b) (foldEither xs)

resultToEither :: A.Result a -> Either Text a
resultToEither (A.Success res) = Right res
resultToEither (A.Error e)     = Left $ T.pack e

---------------- Match where clauses -------------
findOneMatching :: B.Beamable table => Where be table -> [table Identity] -> Maybe (table Identity)
findOneMatching whereClause = find (`matchWhereClause` whereClause)

findAllMatching :: B.Beamable table => Where be table -> [table Identity] -> [table Identity]
findAllMatching whereClause = filter (`matchWhereClause` whereClause)

matchWhereClause :: B.Beamable table => table Identity -> [Clause be table] -> Bool
matchWhereClause row = all matchClauseQuery
  where
  matchClauseQuery = \case
    And queries     -> all matchClauseQuery queries
    Or queries      -> any matchClauseQuery queries
    Is column' term ->
      let column = fromColumnar' . column' . columnize
        in termQueryMatch (column row) term

termQueryMatch :: (Ord value) => value -> Term be value -> Bool
termQueryMatch columnVal = \case
  In literals             -> elem columnVal literals
  Null                    -> isNothing columnVal
  Eq literal              -> columnVal == literal
  GreaterThan literal     -> columnVal > literal
  GreaterThanOrEq literal -> columnVal >= literal
  LessThan literal        -> columnVal < literal
  LessThanOrEq literal    -> columnVal <= literal
  Not Null                -> isJust Nothing
  Not (Eq literal)        -> columnVal /= literal
  Not term                -> not (termQueryMatch columnVal term)
  _                       -> error "Term query not supported"

toPico :: Int -> Fixed.Pico
toPico value = Fixed.MkFixed $ ((toInteger value) * 1000000000000)

getStreamName :: String -> Text
getStreamName shard = getConfigStreamBasename <> "-" <> (T.pack shard) <> ""

getRandomStream :: (L.MonadFlow m) => m Text
getRandomStream = do
  streamShard <- L.runIO' "random shard" $ randomRIO (1, getConfigStreamMaxShards)
  return $ getStreamName (show streamShard)

getConfigStreamNames :: [Text]
getConfigStreamNames = fmap (\shardNo -> getStreamName (show shardNo) ) [1..getConfigStreamMaxShards]

getConfigStreamBasename :: Text
getConfigStreamBasename = fromMaybe "ConfigStream" $ lookupEnvT "CONFIG_STREAM_BASE_NAME"

getConfigStreamMaxShards :: Int
getConfigStreamMaxShards = fromMaybe 20 $ readMaybe =<< lookupEnvT "CONFIG_STREAM_MAX_SHARDS"

getConfigStreamLooperDelayInSec :: Int
getConfigStreamLooperDelayInSec = fromMaybe 5 $ readMaybe =<< lookupEnvT "CONFIG_STREAM_LOOPER_DELAY_IN_SEC"

getConfigEntryNewTtl :: (L.MonadFlow m) => m LocalTime
getConfigEntryNewTtl = do
    currentTime <- L.getCurrentTimeUTC
    noise <- L.runIO' "random seconds" $ randomRIO (1, 900)
    return $ addLocalTime (secondsToNominalDiffTime $ toPico (45 * 60 + noise)) currentTime

threadDelayMilisec :: Integer -> IO ()
threadDelayMilisec ms = threadDelay $ fromIntegral ms * 1000

decodeToField :: forall a. (FromJSON a, Serialize.Serialize a) => BSL.ByteString -> MeshResult [a]
decodeToField val =
  let (h, v) = BSL.splitAt 4 val
   in case h of
        "CBOR" -> case Cereal.decodeLazy v of
                    Right r' -> Right [r']
                    Left _ -> case Cereal.decodeLazy v of
                                Right r'' -> Right r''
                                Left _ -> case Cereal.decodeLazy v of
                                            Right r''' -> decodeField @a r'''
                                            Left err' -> Left $ MDecodingError $ T.pack err'
        "JSON" ->
          case A.eitherDecode v of
            Right r' -> decodeField @a r'
            Left e   -> Left $ MDecodingError $ T.pack e
        _      ->
          case A.eitherDecode val of
            Right r' -> decodeField @a r'
            Left e   -> Left $ MDecodingError $ T.pack e

decodeField :: forall a. (FromJSON a, Serialize.Serialize a) => A.Value -> MeshResult [a]
decodeField o@(A.Object _) =
  case A.eitherDecode @a $ A.encode o of
    Right r -> return [r]
    Left e  -> Left $ MDecodingError $ T.pack e
decodeField o@(A.Array _) =
  mapLeft (MDecodingError . T.pack)
    $ A.eitherDecode @[a] $ A.encode o
decodeField o = Left $ MDecodingError
  ("Expected list or object but got '" <> T.pack (show o) <> "'.")

-- getFieldsAndValuesFromClause dt (And [Is DBS.id (Eq (Just 1)), Or [Is DBS.merchantId (Eq (Just "123")), Is DBS.orderId (Eq (Just "oid"))]])
-- [[("id","Just 1"),("merchantId","Just \"123\"")],[("id","Just 1"),("orderId","Just \"oid\"")]]
getFieldsAndValuesFromClause :: forall table be. (Model be table, MeshMeta be table) =>
  B.DatabaseEntityDescriptor be (B.TableEntity table) -> Clause be table -> [[(Text, Text)]]
getFieldsAndValuesFromClause dt = \case
  And cs -> foldl' processAnd [[]] $ map (getFieldsAndValuesFromClause dt) cs
  Or cs -> processOr cs
  Is column (Eq val) -> do
    let key = B._fieldName . fromColumnar' . column . columnize $ B.dbTableSettings dt
    [[(key, showVal . snd $ (toPSJSON @be @table) (key, A.toJSON val))]]
  Is column (In vals) -> do
    let key = B._fieldName . fromColumnar' . column . columnize $ B.dbTableSettings dt
    map (\val -> [(key, showVal . snd $ (toPSJSON @be @table) (key, A.toJSON val))]) vals
  _ -> []

  where
    processAnd xs [] = xs
    processAnd [] ys = ys
    processAnd xs ys = [x ++ y | x <-xs, y <- ys]
    processOr xs = concatMap (getFieldsAndValuesFromClause dt) xs

    showVal res = case res of
      A.String r -> r
      A.Number n -> T.pack $ show n
      A.Array l  -> T.pack $ show l
      A.Object o -> T.pack $ show o
      A.Bool b -> T.pack $ show b
      A.Null -> T.pack "" 

getPrimaryKeyFromFieldsAndValues :: (L.MonadFlow m) => Text -> MeshConfig -> HM.HashMap Text Bool -> [(Text, Text)] -> m (MeshResult [ByteString])
getPrimaryKeyFromFieldsAndValues _ _ _ [] = pure $ Right []
getPrimaryKeyFromFieldsAndValues modelName meshCfg keyHashMap ((k, v) : xs) =
  case HM.lookup k keyHashMap of
    Just True -> pure $ Right [fromString $ T.unpack (contructKey <> getShardedHashTag contructKey)]
    Just False -> do
      let sKey = contructKey
      res <- L.runKVDB meshCfg.kvRedis $ L.smembers (fromString $ T.unpack sKey)
      case res of
        Right r -> pure $ Right r
        Left e -> return $ Left $ MRedisError e
    _ -> getPrimaryKeyFromFieldsAndValues modelName meshCfg keyHashMap xs
  where
    contructKey = modelName <> "_" <> k <> "_" <> v

-- >>> map (T.intercalate "_") (nonEmptySubsequences ["id", "id2", "id3"])
-- ["id","id2","id_id2","id3","id_id3","id2_id3","id_id2_id3"]
nonEmptySubsequences         :: [Text] -> [[Text]]
nonEmptySubsequences []      =  []
nonEmptySubsequences (x:xs)  =  [x]: foldr f [] (nonEmptySubsequences xs)
  where f ys r = ys : (x : ys) : r

isRecachingEnabled :: Bool
isRecachingEnabled = fromMaybe False $ readMaybe =<< lookupEnvT "IS_RECACHING_ENABLED"
