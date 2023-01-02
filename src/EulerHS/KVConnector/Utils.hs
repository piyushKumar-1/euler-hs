{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE NamedFieldPuns #-}

module EulerHS.KVConnector.Utils where

import           EulerHS.Prelude
import qualified Data.Aeson as A
import           Data.Aeson.Encode.Pretty (encodePretty)
import qualified Database.Beam as B
import qualified Database.Beam.Schema.Tables as B
import qualified Data.ByteString.Lazy as BSL
import           Text.Casing (quietSnake)
import           Data.Either.Extra (mapRight)
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import           EulerHS.KVConnector.DBSync (meshModelTableEntityDescriptor)
import           EulerHS.KVConnector.Types (MeshMeta(..), MeshResult, MeshError(..), MeshConfig,
                  KVConnector(..), PrimaryKey(..), SecondaryKey(..))
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

isRecachingEnabled :: Bool
isRecachingEnabled = fromMaybe False $ readMaybe =<< lookupEnvT "IS_RECACHING_ENABLED"
