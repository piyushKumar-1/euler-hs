
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- {-# OPTIONS_GHC -Wno-redundant-constraints #-}

module EulerHS.KVConnector.Flow where

import           EulerHS.Prelude
import           EulerHS.KVConnector.Types (KVConnector, MeshConfig, MeshResult, MeshError(..), MeshMeta(..), tableName, keyMap, getLookupKeyByPKey, getSecondaryLookupKeys, applyFPair)
import           EulerHS.KVConnector.DBSync (getCreateQuery, getUpdateQuery, getDbUpdateCommandJson, meshModelTableEntityDescriptor, DBCommandVersion(..))
import qualified Data.Aeson as A
import           Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified EulerHS.Language as L
import           EulerHS.Extra.Language (getOrInitSqlConn)
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as Map
import           EulerHS.SqlDB.Types (BeamRunner, BeamRuntime, DBConfig, DBError)
import qualified EulerHS.SqlDB.Language as DB
import           Sequelize (fromColumnar', modelTableName, columnize, sqlSelect, Model, Where, Clause(..), Term(..), Set(..))
import qualified Database.Beam as B
import qualified Database.Beam.Schema.Tables as B
import qualified Database.Beam.MySQL as BM
import           Data.Either.Extra (mapRight, mapLeft)
import           Named (defaults, (!))
import           Unsafe.Coerce (unsafeCoerce)



createWithKVConnector ::
  forall (table :: (Type -> Type) -> Type) r.
  ( HasCallStack,
    Model BM.MySQL table,
    FromJSON (table Identity),
    ToJSON (table Identity),
    -- Show (table Identity),
    KVConnector (table Identity)
  ) =>
  MeshConfig ->
  table Identity ->
  Maybe Text ->
  ReaderT r L.Flow (MeshResult (table Identity))
createWithKVConnector meshCfg value _ = do
  autoId <- getAutoIncId meshCfg (tableName @(table Identity))
  L.logWarning @Text "createWithKVConnector" (show autoId)
  case autoId of
    Right _id -> do
      -- TODO: Key - id is hardcoded to replace AutoIncrId. Make it Generic
      let val = unsafeJSONSet @Text "id" (T.pack . show $ _id) value
      let pKeyText = getLookupKeyByPKey value
          pKey = fromString . T.unpack $ pKeyText
      time <- fromIntegral <$> L.getCurrentDateInMillis
      let qCmd = getCreateQuery (modelTableName @table) V1 pKeyText time meshCfg.meshDBName value
      _ <- L.runKVDB meshCfg.kvRedis $ L.setex pKey meshCfg.redisTtl (BSL.toStrict $ A.encode val)
      mapM_ (\secIdx -> do
        let sKey = fromString . T.unpack $ secIdx
        L.runKVDB meshCfg.kvRedis $ L.setex sKey meshCfg.redisTtl pKey) $ getSecondaryLookupKeys val
      _ <- L.runKVDB meshCfg.kvRedis $ L.xadd
            (encodeUtf8 (meshCfg.ecRedisDBStream <> getShardedHashTag pKeyText))
            L.AutoID
            [("command", BSL.toStrict $ A.encode qCmd)]
      return $ Right val
    Left err -> return $ Left err

---------------- Update -----------------

updateWithKVConnector :: forall be table beM r.
  ( HasCallStack,
    BeamRuntime be beM,
    BeamRunner beM,
    Model be table,
    MeshMeta table,
    B.HasQBuilder be,
    KVConnector (table Identity),
    FromJSON (table Identity),
    ToJSON (table Identity),
    Show (table Identity) --debugging purpose
  ) =>
  DBConfig beM ->
  MeshConfig ->
  [Set be table] ->
  Where be table ->
  ReaderT r L.Flow (MeshResult (table Identity))
updateWithKVConnector dbConf meshCfg setClause whereClause = do
  findRes <- findWithKVConnector meshCfg whereClause
  L.logDebugT "updateWithKVConnector" ("findWithKVConnectorRes = " <> show findRes)
  case findRes of
    Right (Just obj) -> setObjectInRedis obj
    Right Nothing -> do
      L.logDebugT "updateWithKVConnector" "Found nothing from findWithKVConnectorRes - Falling back to DB and recaching"
      let findQuery = DB.findRow (sqlSelect ! #where_ whereClause ! defaults)
      dbRes <- runQuery dbConf findQuery
      case dbRes of
        Right (Just obj) -> do
          -- Reconstruct secondary key lookups
          mapM_ (\secIdx -> do
            let pKey = fromString . T.unpack $ getLookupKeyByPKey obj
                sKey = fromString . T.unpack $ secIdx
            L.runKVDB meshCfg.kvRedis $ L.setex sKey meshCfg.redisTtl pKey) (getSecondaryLookupKeys obj)
          setObjectInRedis obj
        Right Nothing -> pure $ Left (MUpdateFailed ("No value found for table " <> tableName @(table Identity)))
        Left err -> pure $ Left (MDBError err)
    Left err -> pure $ Left err

  where
    setObjectInRedis :: table Identity -> ReaderT r L.Flow (MeshResult (table Identity))
    setObjectInRedis obj = do
      let updVals = jsonKeyValueUpdates setClause
      case updateModel obj updVals of
        Left err -> return $ Left err
        Right updatedModel -> do
          time <- fromIntegral <$> L.getCurrentDateInMillis
          let pKeyText = getLookupKeyByPKey obj
              pKey = fromString . T.unpack $ pKeyText
          let updateCmd = getDbUpdateCommandJson (modelTableName @table) updVals whereClause
              qCmd = getUpdateQuery V1 pKeyText time meshCfg.meshDBName updateCmd
          L.logDebugT "Setting in redis stream" (meshCfg.ecRedisDBStream <> getShardedHashTag pKeyText)
          kvdbRes <- L.runKVDB meshCfg.kvRedis $ L.setex pKey meshCfg.redisTtl (BSL.toStrict $ A.encode updatedModel)
          _ <- L.runKVDB meshCfg.kvRedis $ L.xadd
            (encodeUtf8 (meshCfg.ecRedisDBStream <> getShardedHashTag pKeyText))
            L.AutoID
            [("command", BSL.toStrict $ A.encode qCmd)]
          L.logDebug @Text "RedisUpdAnswer" . T.pack . show $ kvdbRes
          return $ mapLeft MDecodingError
            $ resultToEither $ A.fromJSON updatedModel

-- | Update the model by setting it's fields according the given
--   key value mapping.
updateModel :: forall table.
  ( MeshMeta table,
    ToJSON (table Identity)
  ) =>
  table Identity -> [(Text, A.Value)] -> MeshResult A.Value
updateModel model updVals = do
  let updVals' = map (\(key,v) -> (key, Map.findWithDefault id key (valueMapper @table) v)) updVals
  case A.toJSON model of
    A.Object o -> Right (A.Object $ foldr (uncurry HM.insert) o updVals')
    o -> Left $ MUpdateFailed
      ("Failed to update a model. Expected a JSON object but got '" <>
        (decodeUtf8 . BSL.toStrict . encodePretty $ o) <>
        "'.")

jsonKeyValueUpdates ::
  forall be table. (Model be table, MeshMeta table)
  => [Set be table] -> [(Text, A.Value)]
jsonKeyValueUpdates = fmap jsonSet

jsonSet ::
  forall be table.
  (Model be table, MeshMeta table) =>
  Set be table -> (Text, A.Value)
jsonSet (Set column value) = (key, modifiedValue)
  where
    key = B._fieldName . fromColumnar' . column . columnize $
      B.dbTableSettings (meshModelTableEntityDescriptor @table @be)
    modifiedValue = A.toJSON value

jsonSet (SetDefault _) = error "Default values are not supported"

runQuery ::
  ( HasCallStack,
    BeamRuntime be beM, BeamRunner beM,
    L.MonadFlow m
  ) =>
  DBConfig beM -> DB.SqlDB beM a -> m (Either DBError a)
runQuery dbConf query = do
  conn <- getOrInitSqlConn dbConf
  case conn of
    Right c -> L.runDB c query
    Left  e -> return $ Left e

---------------- Find -----------------------
findWithKVConnector :: forall be table beM r.
  ( HasCallStack,
    BeamRuntime be beM,
    Model be table,
    MeshMeta table,
    KVConnector (table Identity),
    FromJSON (table Identity)
  ) =>
  MeshConfig ->
  Where be table ->
  ReaderT r L.Flow (MeshResult (Maybe (table Identity)))
findWithKVConnector meshCfg whereClause = do
  let fieldsAndValues = sort $ getFieldsAndValuesFromClause meshModelTableEntityDescriptor [] (And whereClause)
      modelName = tableName @(table Identity)
      keyAndValueCombinations = uncurry zip $ applyFPair (map (T.intercalate "_") . nonEmptySubsequences) $ unzip fieldsAndValues
      keyHashMap = keyMap @(table Identity)
  L.logDebugT "findWithKVConnector" (show keyAndValueCombinations)
  keyRes <- getPrimaryKeyFromFieldsAndValues modelName keyAndValueCombinations meshCfg keyHashMap
  L.logDebugT "findWithKVConnector" (show keyRes)

  case keyRes of
    Right (Just key) -> do
      res <- L.runKVDB meshCfg.kvRedis $ L.get (fromString $ T.unpack key)
      case res of
        Right (Just r) ->
          case A.eitherDecode $ BSL.fromChunks [r] of
            Right r' -> return $ mapRight (findOneMatching whereClause) $ decodeField @(table Identity) r'
            -- Decoding Error
            Left e   -> return $ Left $ MDecodingError $ T.pack e
        -- Not found
        Right Nothing -> do
          let traceMsg = "redis_fetch_noexist: Could not find key: " <> key
          L.logWarningT "getCacheWithHash" traceMsg
          pure $ Right Nothing
        -- Redis error
        Left e -> return $ Left $ MRedisError e
    Right Nothing -> return $ Right Nothing
    Left err -> pure $ Left err

decodeField :: forall a. (FromJSON a) => A.Value -> MeshResult [a]
decodeField o@(A.Object _) =
  case A.eitherDecode @a $ A.encode o of
    Right r -> return [r]
    Left e  -> Left $ MDecodingError $ T.pack e
decodeField o@(A.Array _) =
  mapLeft (MDecodingError . T.pack)
    $ A.eitherDecode @[a] $ A.encode o
decodeField o = Left $ MDecodingError
  ("Expected list or object but got '" <> T.pack (show o) <> "'.")

getFieldsAndValuesFromClause :: forall table be. (Model be table, MeshMeta table) =>
  B.DatabaseEntityDescriptor be (B.TableEntity table) -> [(Text, Text)] -> Clause be table -> [(Text, Text)]
getFieldsAndValuesFromClause dt res = \case
  And cs -> concatMap (getFieldsAndValuesFromClause dt res) cs
  Or cs -> concatMap (getFieldsAndValuesFromClause dt res) cs
  Is column (Eq val) -> do
    let key = B._fieldName . fromColumnar' . column . columnize $ B.dbTableSettings dt
    (key, show val) : res
  _ -> res

getPrimaryKeyFromFieldsAndValues :: Text -> [(Text, Text)] -> MeshConfig -> HM.HashMap Text Bool -> ReaderT r L.Flow (MeshResult (Maybe Text))
getPrimaryKeyFromFieldsAndValues _ [] _ _ = return $ Right Nothing
getPrimaryKeyFromFieldsAndValues modelName ((k, v) : xs) meshCfg keyHashMap =
  case HM.lookup k keyHashMap of
    Just True -> pure $ Right $ Just contructKey
    Just False -> do
      let sKey = contructKey
      res <- L.runKVDB meshCfg.kvRedis $ L.get (fromString $ T.unpack sKey)
      case res of
        Right (Just r) -> pure $ Right (Just $ decodeUtf8 r)
        Right Nothing -> do
          let traceMsg = "redis_fetch_noexist: Could not find " <> sKey
          L.logWarningT "getPKeyFromSkeys" traceMsg
          getPrimaryKeyFromFieldsAndValues modelName xs meshCfg keyHashMap
        Left e -> return $ Left $ MRedisError e
    _ -> getPrimaryKeyFromFieldsAndValues modelName xs meshCfg keyHashMap
  where
    contructKey = modelName <> "_" <> k <> "_" <> v

-- >>> map (T.intercalate "_") (nonEmptySubsequences ["id", "id2", "id3"])
-- ["id","id2","id_id2","id3","id_id3","id2_id3","id_id2_id3"]
nonEmptySubsequences         :: [Text] -> [[Text]]
nonEmptySubsequences []      =  []
nonEmptySubsequences (x:xs)  =  [x]: foldr f [] (nonEmptySubsequences xs)
  where f ys r = ys : (x : ys) : r

---------------- UTILS -----------------------

getShardedHashTag :: Text -> Text
getShardedHashTag key = do
  let slot = unsafeCoerce @_ @Word16 $ L.keyToSlot $ encodeUtf8 key
      streamShard = slot `mod` 128
  "{shard-" <> show streamShard <> "}"

getAutoIncId :: MeshConfig -> Text -> ReaderT r L.Flow (MeshResult Integer)
getAutoIncId meshCfg tName = do
  let key = tName <> "_auto_increment_id"
  mId <- L.runKVDB meshCfg.kvRedis $ L.incr $ encodeUtf8 key
  case mId of
    Right id_ -> return $ Right id_
    Left e    -> return $ Left $ MRedisError e

unsafeJSONSet :: forall a b. (ToJSON a, FromJSON b, ToJSON b) => Text -> a -> b -> b
unsafeJSONSet field value obj =
  case A.toJSON obj of
    A.Object o ->
      let jsonVal = A.toJSON value
          newObj =  A.Object (HM.insert field jsonVal o)
      in case resultToEither $ A.fromJSON newObj of
        Right r -> r
        Left e  -> error e
    _ -> error "Can't set  value of JSON which isn't a object."

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