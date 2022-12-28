{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module EulerHS.KVConnector.Flow
  (
    createWoReturingKVConnector,
    createWithKVConnector,
    findWithKVConnector,
    updateWoReturningWithKVConnector,
    updateWithKVConnector,
    findAllWithKVConnector,
    updateAllWithKVConnector,
    getFieldsAndValuesFromClause,
    updateAllReturningWithKVConnector,
    findAllWithOptionsKVConnector
  )
 where

import           Control.Monad.Extra (notM)
import           EulerHS.Extra.Time (getCurrentDateInMillis)
import           EulerHS.Prelude hiding (maximum)
import           EulerHS.KVConnector.Types (KVConnector(..), MeshConfig, MeshResult, MeshError(..), MeshMeta(..), SecondaryKey(..), tableName, keyMap, DBLogEntry(..), MerchantID(..))
import           EulerHS.KVConnector.DBSync (getCreateQuery, getUpdateQuery, getDbUpdateCommandJson, meshModelTableEntityDescriptor, toPSJSON, DBCommandVersion(..))
import           EulerHS.KVConnector.InMemConfig.Types (InMemCacheResult (..))
import           EulerHS.KVConnector.InMemConfig.Flow (checkAndStartLooper)
import           EulerHS.KVConnector.Utils
import           EulerHS.Runtime (mkConfigEntry)
import           Unsafe.Coerce (unsafeCoerce)
import qualified Data.Aeson as A
import qualified Data.Serialize as Cereal
import qualified Data.ByteString.Lazy as BSL
import           Data.List (span, maximum, findIndices)
import qualified Data.Text as T
import qualified EulerHS.Language as L
import qualified Data.HashMap.Strict as HM
import           EulerHS.SqlDB.Types (BeamRunner, BeamRuntime, DBConfig, DBError)
import qualified EulerHS.SqlDB.Language as DB
import           Sequelize (fromColumnar', columnize, sqlSelect, sqlSelect', sqlUpdate, Model, Where, Clause(..), Term(..), Set(..), OrderBy(..))
import           EulerHS.CachedSqlDBQuery (createReturning, createSqlWoReturing, updateOneSqlWoReturning, SqlReturning)
import qualified Database.Beam as B
import qualified Database.Beam.Schema.Tables as B
import qualified Database.Beam.Postgres as BP
import           Data.Either.Extra (mapRight, mapLeft)
import           Named (defaults, (!))
import qualified Data.Serialize as Serialize
import qualified EulerHS.KVConnector.Encoding as Encoding
import           Safe (atMay)
import           System.CPUTime (getCPUTime)
import           EulerHS.Types(ApiTag (..))
import           EulerHS.KVConnector.Metrics (incrementMetric, KVMetric(..))

createWoReturingKVConnector :: forall (table :: (Type -> Type) -> Type) be m beM.
  ( HasCallStack,
    SqlReturning beM be,
    Model be table,
    BeamRuntime be beM,
    BeamRunner beM,
    B.HasQBuilder be,
    FromJSON (table Identity),
    ToJSON (table Identity),
    Serialize.Serialize (table Identity),
    Show (table Identity),
    KVConnector (table Identity),
    L.MonadFlow m) =>
  DBConfig beM ->
  MeshConfig ->
  table Identity ->
  m (MeshResult ())
createWoReturingKVConnector dbConf meshCfg value = do
  isEnabled <- isKVEnabled (tableName @(table Identity))
  t1        <- getCurrentDateInMillis
  cpuT1     <- L.runIO getCPUTime
  res <- if isEnabled
    then do
      L.logDebugT "createWoReturingKVConnector" ("Taking KV Path for " <> tableName @(table Identity))
      mapRight (const ()) <$> createKV meshCfg value
    else do
      L.logDebugT "createWoReturingKVConnector" ("Taking SQLDB Path for " <> tableName @(table Identity))
      res <- createSqlWoReturing dbConf value
      case res of
        Right _ -> return $ Right ()
        Left e -> return $ Left $ MDBError e
  t2        <- getCurrentDateInMillis
  cpuT2     <- L.runIO getCPUTime
  apiTag <- L.getOptionLocal ApiTag
  mid <- L.getOptionLocal MerchantID 
  let dblog =DBLogEntry {
      _log_type     = "DB"
    , _action       = "CREATE"
    , _data        = case res of
                        Left err -> A.String (T.pack $ show err)
                        Right _  -> A.Null
    , _latency      = t2 - t1
    , _model        = tableName @(table Identity)
    , _cpuLatency   = getLatencyInMicroSeconds (cpuT2 - cpuT1)
    , _source       = if isEnabled then "KV" else "DB"
    , _apiTag       = apiTag
    , _merchant_id = mid
    }
  L.logInfoV ("DB" :: Text) dblog
  incrementMetric KVAction dblog
  pure res


createWithKVConnector ::
  forall (table :: (Type -> Type) -> Type) be m beM.
  ( HasCallStack,
    SqlReturning beM be,
    Model be table,
    BeamRuntime be beM,
    BeamRunner beM,
    B.HasQBuilder be,
    FromJSON (table Identity),
    ToJSON (table Identity),
    Serialize.Serialize (table Identity),
    Show (table Identity),
    KVConnector (table Identity),
    L.MonadFlow m) =>
  DBConfig beM ->
  MeshConfig ->
  table Identity ->
  m (MeshResult (table Identity))
createWithKVConnector dbConf meshCfg value = do
  isEnabled <- isKVEnabled (tableName @(table Identity))
  t1        <- getCurrentDateInMillis
  cpuT1     <- L.runIO getCPUTime
  res <- if isEnabled
    then do
      L.logDebugT "createWithKVConnector" ("Taking KV Path for " <> tableName @(table Identity))
      createKV meshCfg value
    else do
      L.logDebugT "createWithKVConnector" ("Taking SQLDB Path for " <> tableName @(table Identity))
      res <- createReturning dbConf value Nothing
      case res of
        Right val -> return $ Right val
        Left e -> return $ Left $ MDBError e
  t2        <- getCurrentDateInMillis
  cpuT2     <- L.runIO getCPUTime
  apiTag <- L.getOptionLocal ApiTag
  mid <- L.getOptionLocal MerchantID 
  let dblog =DBLogEntry {
      _log_type     = "DB"
    , _action       = "CREATE_RETURNING"
    , _data        = case res of
                        Left err -> A.String (T.pack $ show err)
                        Right _  -> A.Null
    , _latency      = t2 - t1
    , _model        = tableName @(table Identity)
    , _cpuLatency   = getLatencyInMicroSeconds (cpuT2 - cpuT1)
    , _source       = if isEnabled then "KV" else "DB"
    , _apiTag       = apiTag
    , _merchant_id = mid
    }
  L.logInfoV ("DB" :: Text) dblog
  incrementMetric KVAction dblog
  pure res

createKV :: forall (table :: (Type -> Type) -> Type) m.
  ( FromJSON (table Identity),
    ToJSON (table Identity),
    Serialize.Serialize (table Identity),
    Show (table Identity),
    KVConnector (table Identity),
    L.MonadFlow m) =>
  MeshConfig ->
  table Identity ->
  m (MeshResult (table Identity))
createKV meshCfg value = do
  autoId <- getAutoIncId meshCfg (tableName @(table Identity))
  case autoId of
    Right _id -> do
      -- TODO: Key - id is hardcoded to replace AutoIncrId. Make it Generic
      let val = unsafeJSONSet @Text "id" (T.pack . show $ _id) value
      let pKeyText = getLookupKeyByPKey val
          shard = getShardedHashTag pKeyText
          pKey = fromString . T.unpack $ pKeyText <> shard
      time <- fromIntegral <$> L.getCurrentDateInMillis
      let qCmd = getCreateQuery (tableName @(table Identity)) V1 (pKeyText <> shard) time meshCfg.meshDBName val
      L.logDebugT "createKV length" (show $ length $ getSecondaryLookupKeys val)
      revMappingRes <- mapM (\secIdx -> do
        let sKey = fromString . T.unpack $ secIdx
        _ <- L.runKVDB meshCfg.kvRedis $ L.sadd sKey [pKey]
        L.runKVDB meshCfg.kvRedis $ L.expire sKey meshCfg.redisTtl
        ) $ getSecondaryLookupKeys val
      case foldEither revMappingRes of
        Left err -> pure $ Left $ MRedisError err
        Right _ -> do
          kvRes <- L.runKVDB meshCfg.kvRedis $ L.multiExecWithHash (encodeUtf8 shard) $ do
            _ <- L.xaddTx
                  (encodeUtf8 (meshCfg.ecRedisDBStream <> shard))
                  L.AutoID
                  [("command", BSL.toStrict $ A.encode qCmd)]
            L.setexTx pKey meshCfg.redisTtl (BSL.toStrict $ Encoding.encode meshCfg.cerealEnabled val)
          case kvRes of
            Right _ -> pure $ Right val
            Left err -> pure $ Left (MRedisError err)
    Left err -> pure $ Left err

---------------- Update -----------------

updateWoReturningWithKVConnector :: forall be table beM m.
  ( HasCallStack,
    BeamRuntime be beM,
    BeamRunner beM,
    Model be table,
    MeshMeta be table,
    B.HasQBuilder be,
    KVConnector (table Identity),
    FromJSON (table Identity),
    ToJSON (table Identity),
    Serialize.Serialize (table Identity),
    Show (table Identity), --debugging purpose
    L.MonadFlow m
  ) =>
  DBConfig beM ->
  MeshConfig ->
  [Set be table] ->
  Where be table ->
  m (MeshResult ())
updateWoReturningWithKVConnector dbConf meshCfg setClause whereClause = do
  isEnabled <- isKVEnabled (tableName @(table Identity))
  t1        <- getCurrentDateInMillis
  cpuT1     <- L.runIO getCPUTime
  res <- if isEnabled
    then do
      L.logDebugT "updateWoReturningWithKVConnector" ("Taking KV Path for " <> tableName @(table Identity) )
      mapRight (const ()) <$> updateKV dbConf meshCfg setClause whereClause
    else do
      L.logDebugT "updateWoReturningWithKVConnector" ("Taking SQLDB Path for " <> tableName @(table Identity))
      whereClauseDiffCheck whereClause
      res <- updateOneSqlWoReturning dbConf setClause whereClause
      case res of
        Right val -> return $ Right val
        Left e -> return $ Left $ MDBError e
  t2        <- getCurrentDateInMillis
  cpuT2     <- L.runIO getCPUTime
  apiTag <- L.getOptionLocal ApiTag
  mid <- L.getOptionLocal MerchantID 
  let dblog =DBLogEntry {
      _log_type     = "DB"
    , _action       = "UPDATE"
    , _data        = case res of
                        Left err -> A.String (T.pack $ show err)
                        Right _  -> A.Null
    , _latency      = t2 - t1
    , _model        = tableName @(table Identity)
    , _cpuLatency   = getLatencyInMicroSeconds (cpuT2 - cpuT1)
    , _source       = if isEnabled then "KV" else "DB"
    , _apiTag       = apiTag
    , _merchant_id = mid
    }
  L.logInfoV ("DB" :: Text) dblog
  incrementMetric KVAction dblog
  pure res

updateWithKVConnector :: forall table m.
  ( HasCallStack,
    Model BP.Postgres table,
    MeshMeta BP.Postgres table,
    B.HasQBuilder BP.Postgres,
    KVConnector (table Identity),
    FromJSON (table Identity),
    ToJSON (table Identity),
    Serialize.Serialize (table Identity),
    Show (table Identity), --debugging purpose
    L.MonadFlow m
  ) =>
  DBConfig BP.Pg ->
  MeshConfig ->
  [Set BP.Postgres table] ->
  Where BP.Postgres table ->
  m (MeshResult (Maybe (table Identity)))
updateWithKVConnector dbConf meshCfg setClause whereClause = do
  isEnabled <- isKVEnabled (tableName @(table Identity))
  t1        <- getCurrentDateInMillis
  cpuT1     <- L.runIO getCPUTime
  res <- if isEnabled
    then do
      L.logDebugT "updateWithKVConnector" ("Taking KV Path for " <> tableName @(table Identity))
      updateKV dbConf meshCfg setClause whereClause
    else do
      L.logDebugT "updateWithKVConnector" ("Taking SQLDB Path for " <> tableName @(table Identity))
      whereClauseDiffCheck whereClause
      let updateQuery = DB.updateRowsReturningList $ sqlUpdate ! #set setClause ! #where_ whereClause
      res <- runQuery dbConf updateQuery
      case res of
        Right [x] -> return $ Right (Just x)
        Right [] -> return $ Right Nothing
        Right xs -> do
          let message = "DB returned \"" <> show (length xs) <> "\" rows after update for table: " <> show (tableName @(table Identity))
          L.logError @Text "updateWithKVConnector" message
          return $ Left $ UnexpectedError message
        Left e -> return $ Left $ MDBError e
  t2        <- getCurrentDateInMillis
  cpuT2     <- L.runIO getCPUTime
  apiTag <- L.getOptionLocal ApiTag
  mid <- L.getOptionLocal MerchantID 

  let dblog =DBLogEntry {
      _log_type     = "DB"
    , _action       = "UPDATE_RETURNING"
    , _data        = case res of
                        Left err -> A.String (T.pack $ show err)
                        Right _  -> A.Null
    , _latency      = t2 - t1
    , _model        = tableName @(table Identity)
    , _cpuLatency   = getLatencyInMicroSeconds (cpuT2 - cpuT1)
    , _source       = if isEnabled then "KV" else "DB"
    , _apiTag       = apiTag
    , _merchant_id = mid
    }
  L.logInfoV ("DB" :: Text) dblog
  incrementMetric KVAction dblog
  pure res

updateKV :: forall be table beM m.
  ( HasCallStack,
    BeamRuntime be beM,
    BeamRunner beM,
    Model be table,
    MeshMeta be table,
    B.HasQBuilder be,
    KVConnector (table Identity),
    FromJSON (table Identity),
    ToJSON (table Identity),
    Serialize.Serialize (table Identity),
    Show (table Identity),
    L.MonadFlow m
  ) =>
  DBConfig beM ->
  MeshConfig ->
  [Set be table] ->
  Where be table ->
  m (MeshResult (Maybe (table Identity)))
updateKV dbConf meshCfg setClause whereClause = do
  eitherFindRes <- findOneFromRedis meshCfg whereClause
  let updVals = jsonKeyValueUpdates setClause
  case eitherFindRes of
    Right [] -> do
      L.logDebugT "updateKV" "Found nothing from findWithKVConnectorRes - Falling back to DB and recaching"
      let findQuery = DB.findRows (sqlSelect ! #where_ whereClause ! defaults)
      dbRes <- runQuery dbConf findQuery
      case dbRes of
        Right [obj] -> mapRight Just <$> updateDBRowInRedis meshCfg updVals whereClause obj
        Right [] -> pure $ Right Nothing
        Right _ -> pure $ Left $ MUpdateFailed "Found more than one record in DB"
        Left err -> pure $ Left (MDBError err)
    Right r -> case findAllMatching whereClause r of
      [obj] -> mapRight Just <$> updateKVRowInRedis meshCfg whereClause updVals obj
      [] -> pure $ Right Nothing
      _ -> do
        L.logDebugT "updateKV" "Found more than one record in redis - Update failed"
        pure $ Left $ MUpdateFailed "Found more than one record in redis"
    Left err -> pure $ Left err

updateObjectInMemConfig :: forall beM be table m.
  ( HasCallStack,
    BeamRuntime be beM,
    BeamRunner beM,
    Model be table,
    MeshMeta be table,
    B.HasQBuilder be,
    KVConnector (table Identity),
    FromJSON (table Identity),
    ToJSON (table Identity),
    L.MonadFlow m
  ) => MeshConfig -> Where be table -> [(Text, A.Value)] -> table Identity ->  m (MeshResult ())
updateObjectInMemConfig meshCfg _ updVals obj = do
  shouldSearchInMemoryCache <- isInMemConfigEnabled $ tableName @(table Identity)
  if not shouldSearchInMemoryCache
    then pure . Right $ ()
    else
      case (updateModel @be @table) obj updVals of
        Left err -> return $ Left err
        Right updatedModel -> do
          let 
            pKeyText = getLookupKeyByPKey obj
            shard = getShardedHashTag pKeyText
            pKey =  pKeyText <> shard
            updatedModelT :: Text = decodeUtf8 . A.encode $ updatedModel
          case A.fromJSON updatedModel of
            A.Error decodeErr -> return . Left . MDecodingError . T.pack $ decodeErr
            A.Success (updatedModel' :: table Identity)  -> do
              L.logInfoT "updateObjectInMemConfig" $ "Found key <" <> pKey <> "> in redis. Now setting in in-mem-config"
              newTtl <- getConfigEntryNewTtl
              L.setConfig pKey (mkConfigEntry newTtl updatedModel') 
              mapM_ (pushToConfigStream pKey updatedModelT) getConfigStreamNames
              pure . Right $ ()
  where
    pushToConfigStream :: (L.MonadFlow m ) => Text -> Text -> Text -> m ()
    pushToConfigStream k v streamName = 
      void $ L.runKVDB meshCfg.kvRedis $ L.xadd (encodeUtf8 streamName) L.AutoID [applyFPair encodeUtf8 (k,v)]


updateObjectRedis :: forall beM be table m.
  ( HasCallStack,
    BeamRuntime be beM,
    BeamRunner beM,
    Model be table,
    MeshMeta be table,
    B.HasQBuilder be,
    KVConnector (table Identity),
    FromJSON (table Identity),
    ToJSON (table Identity),
    Serialize.Serialize (table Identity),
    -- Show (table Identity), --debugging purpose
    L.MonadFlow m
  ) =>
  MeshConfig -> [(Text, A.Value)] -> Where be table -> table Identity -> m (MeshResult (table Identity))
updateObjectRedis meshCfg updVals whereClause obj = do
  case (updateModel @be @table) obj updVals of
    Left err -> return $ Left err
    Right updatedModel -> do
      time <- fromIntegral <$> L.getCurrentDateInMillis
      let pKeyText = getLookupKeyByPKey obj
          shard = getShardedHashTag pKeyText
          pKey = fromString . T.unpack $ pKeyText <> shard
      let updateCmd = getDbUpdateCommandJson (tableName @(table Identity)) updVals whereClause
          qCmd = getUpdateQuery V1 (pKeyText <> shard) time meshCfg.meshDBName updateCmd
      case resultToEither $ A.fromJSON updatedModel of
        Right value -> do
          let olderSkeys = map (\(SKey s) -> s) (secondaryKeys obj)
          skeysUpdationRes <- modifySKeysRedis olderSkeys value
          case skeysUpdationRes of
            Right _ -> do
              kvdbRes <- L.runKVDB meshCfg.kvRedis $ L.multiExecWithHash (encodeUtf8 shard) $ do
                _ <- L.xaddTx
                      (encodeUtf8 (meshCfg.ecRedisDBStream <> shard))
                      L.AutoID
                      [("command", BSL.toStrict $ A.encode qCmd)]
                L.setexTx pKey meshCfg.redisTtl (BSL.toStrict $ Encoding.encode meshCfg.cerealEnabled value)
              L.logDebugT "RedisUpdAnswer returned: " (show kvdbRes)
              case kvdbRes of
                Right _ -> pure $ Right value
                Left err -> pure $ Left $ MRedisError err
            Left err -> pure $ Left err
        Left err -> pure $ Left $ MDecodingError err

  where
    modifySKeysRedis :: [[(Text, Text)]] -> table Identity -> m (MeshResult (table Identity)) -- TODO: Optimise this logic
    modifySKeysRedis olderSkeys table = do
      let pKeyText = getLookupKeyByPKey table
          shard = getShardedHashTag pKeyText
          pKey = fromString . T.unpack $ pKeyText <> shard
      let tName = tableName @(table Identity)
          updValsMap = HM.fromList (map (\p -> (fst p, True)) updVals)
          (modifiedSkeysValues, unModifiedSkeysValues) = applyFPair (map getSortedKeyAndValue) $
                                                span (`isKeyModified` updValsMap) olderSkeys
          newSkeysValues = map (\(SKey s) -> getSortedKeyAndValue s) (secondaryKeys table)
      let unModifiedSkeys = map (\x -> tName <> "_" <> fst x <> "_" <> snd x) unModifiedSkeysValues
      let modifiedSkeysValuesMap = HM.fromList modifiedSkeysValues
      L.logDebugT ("modifySKeysRedis " <> tName) (show (map fst modifiedSkeysValues))
      mapRight (const table) <$> runExceptT (do
                                    mapM_ ((ExceptT . resetTTL) . (fromString . T.unpack)) unModifiedSkeys
                                    mapM_ (ExceptT . addNewSkey pKey tName) (foldSkeysFunc modifiedSkeysValuesMap newSkeysValues))

    resetTTL key= do
      x <- L.runKVDB meshCfg.kvRedis $ L.expire key meshCfg.redisTtl
      pure $ mapLeft MRedisError x

    foldSkeysFunc :: HashMap Text Text -> [(Text, Text)] -> [(Text, Text, Text)]
    foldSkeysFunc _ [] = []
    foldSkeysFunc hm (x : xs) = do
      case HM.lookup (fst x) hm of
        Just val -> (fst x, snd x, val) : foldSkeysFunc hm xs
        Nothing -> foldSkeysFunc hm xs


    addNewSkey :: ByteString -> Text -> (Text, Text, Text) -> m (MeshResult ())
    addNewSkey pKey tName (k, v1, v2) = do
      let newSKey = fromString . T.unpack $ tName <> "_" <> k <> "_" <> v1
          oldSKey = fromString . T.unpack $ tName <> "_" <> k <> "_" <> v2
      res <- runExceptT $ do
        _ <- ExceptT $ L.runKVDB meshCfg.kvRedis $ L.srem oldSKey [pKey]
        _ <- ExceptT $ L.runKVDB meshCfg.kvRedis $ L.sadd newSKey [pKey]
        ExceptT $ L.runKVDB meshCfg.kvRedis $ L.expire newSKey meshCfg.redisTtl
      case res of
        Right _ -> pure $ Right ()
        Left err -> pure $ Left (MRedisError err)

    getSortedKeyAndValue :: [(Text,Text)] -> (Text, Text)
    getSortedKeyAndValue kvTup = do
      let sortArr = sortBy (compare `on` fst) kvTup
      let (appendedKeys, appendedValues) = applyFPair (T.intercalate "_") $ unzip sortArr
      (appendedKeys, appendedValues)

    isKeyModified :: [(Text, Text)] -> HM.HashMap Text Bool -> Bool
    isKeyModified sKey updValsMap = foldl' (\r k -> HM.member (fst k) updValsMap || r) False sKey

updateKVRowInRedis :: forall be table beM m.
  ( HasCallStack,
    BeamRuntime be beM,
    BeamRunner beM,
    Model be table,
    MeshMeta be table,
    B.HasQBuilder be,
    KVConnector (table Identity),
    FromJSON (table Identity),
    ToJSON (table Identity),
    Serialize.Serialize (table Identity),
    -- Show (table Identity), --debugging purpose
    L.MonadFlow m
  ) =>
  MeshConfig ->
  Where be table ->
  [(Text, A.Value)] ->
  table Identity ->
  m (MeshResult (table Identity))
updateKVRowInRedis meshCfg whereClause updVals obj = do
  mapM_ (\secIdx -> do -- Resetting TTls of Skeys
    let sKey = fromString . T.unpack $ secIdx
    L.runKVDB meshCfg.kvRedis $  L.expire sKey meshCfg.redisTtl
    ) $ getSecondaryLookupKeys obj
  configUpdateResult <- updateObjectInMemConfig meshCfg whereClause updVals obj
  case configUpdateResult of
    Left err -> return $ Left err
    Right _ -> updateObjectRedis meshCfg updVals whereClause obj

updateDBRowInRedis :: forall beM be table m.
  (
    BeamRuntime be beM,
    BeamRunner beM,
    Model be table,
    MeshMeta be table,
    B.HasQBuilder be,
    KVConnector (table Identity),
    ToJSON (table Identity),
    FromJSON (table Identity),
    Serialize.Serialize (table Identity),
    L.MonadFlow m
  ) =>
  MeshConfig -> [(Text, A.Value)] -> Where be table -> table Identity -> m (MeshResult (table Identity))
updateDBRowInRedis meshCfg updVals whereClause obj = do
  let pKeyText = getLookupKeyByPKey obj
      shard = getShardedHashTag pKeyText
      pKey = fromString . T.unpack $ pKeyText <> shard
  mapM_ (\secIdx -> do -- Recaching Skeys in redis
    let sKey = fromString . T.unpack $ secIdx
    _ <- L.runKVDB meshCfg.kvRedis $ L.sadd sKey [pKey]
    L.runKVDB meshCfg.kvRedis $  L.expire sKey meshCfg.redisTtl
    ) $ getSecondaryLookupKeys obj
  configUpdateResult <- updateObjectInMemConfig meshCfg whereClause updVals obj
  case configUpdateResult of
    Left err -> return $ Left err
    Right _ -> updateObjectRedis meshCfg updVals whereClause obj

updateAllReturningWithKVConnector :: forall table m.
  ( HasCallStack,
    Model BP.Postgres table,
    MeshMeta BP.Postgres table,
    KVConnector (table Identity),
    FromJSON (table Identity),
    ToJSON (table Identity),
    Serialize.Serialize (table Identity),
    -- Show (table Identity), --debugging purpose
    L.MonadFlow m
  ) =>
  DBConfig BP.Pg ->
  MeshConfig ->
  [Set BP.Postgres table] ->
  Where BP.Postgres table ->
  m (MeshResult [table Identity])
updateAllReturningWithKVConnector dbConf meshCfg setClause whereClause = do
  isEnabled <- isKVEnabled (tableName @(table Identity))
  t1        <- getCurrentDateInMillis
  cpuT1     <- L.runIO getCPUTime
  res <- if isEnabled
    then do
      L.logDebugT "updateAllReturningWithKVConnector" ("Taking KV Path for " <> tableName @(table Identity))
      let findAllQuery = DB.findRows (sqlSelect ! #where_ whereClause ! defaults)
          updVals = jsonKeyValueUpdates setClause
      kvRows <- redisFindAll meshCfg whereClause
      dbRows <- runQuery dbConf findAllQuery
      updateKVAndDBResults meshCfg whereClause dbRows kvRows updVals
    else do
      L.logDebugT "updateAllReturningWithKVConnector" ("Taking SQLDB Path for " <> tableName @(table Identity))
      whereClauseDiffCheck whereClause
      let updateQuery = DB.updateRowsReturningList $ sqlUpdate ! #set setClause ! #where_ whereClause
      res <- runQuery dbConf updateQuery
      case res of
        Right x -> return $ Right x
        Left e -> return $ Left $ MDBError e
  t2        <- getCurrentDateInMillis
  cpuT2     <- L.runIO getCPUTime
  apiTag <- L.getOptionLocal ApiTag
  mid <- L.getOptionLocal MerchantID 
  let dblog =DBLogEntry {
      _log_type     = "DB"
    , _action       = "FIND_ALL"
    , _data        = case res of
                        Left err -> A.String (T.pack $ show err)
                        Right _  -> A.Null
    , _latency      = t2 - t1
    , _model        = tableName @(table Identity)
    , _cpuLatency   = getLatencyInMicroSeconds (cpuT2 - cpuT1)
    , _source       = if isEnabled then "KV" else "DB"
    , _apiTag       = apiTag
    , _merchant_id = mid
    }
  L.logInfoV ("DB" :: Text) dblog
  incrementMetric KVAction dblog
  pure res

updateAllWithKVConnector :: forall be table beM m.
  ( HasCallStack,
    BeamRuntime be beM,
    BeamRunner beM,
    Model be table,
    MeshMeta be table,
    B.HasQBuilder be,
    KVConnector (table Identity),
    FromJSON (table Identity),
    ToJSON (table Identity),
    Serialize.Serialize (table Identity),
    -- Show (table Identity), --debugging purpose
    L.MonadFlow m
  ) =>
  DBConfig beM ->
  MeshConfig ->
  [Set be table] ->
  Where be table ->
  m (MeshResult ())
updateAllWithKVConnector dbConf meshCfg setClause whereClause = do
  isEnabled <- isKVEnabled (tableName @(table Identity))
  t1        <- getCurrentDateInMillis
  cpuT1     <- L.runIO getCPUTime
  res <- if isEnabled
    then do
      L.logDebugT "updateAllWithKVConnector" ("Taking KV Path for " <> tableName @(table Identity))
      let findAllQuery = DB.findRows (sqlSelect ! #where_ whereClause ! defaults)
          updVals = jsonKeyValueUpdates setClause
      kvRows <- redisFindAll meshCfg whereClause
      dbRows <- runQuery dbConf findAllQuery
      mapRight (const ()) <$> updateKVAndDBResults meshCfg whereClause dbRows kvRows updVals
    else do
      L.logDebugT "updateAllWithKVConnector" ("Taking SQLDB Path for " <> tableName @(table Identity))
      whereClauseDiffCheck whereClause
      let updateQuery = DB.updateRows $ sqlUpdate ! #set setClause ! #where_ whereClause
      res <- runQuery dbConf updateQuery
      case res of
        Right x -> return $ Right x
        Left e -> return $ Left $ MDBError e
  t2        <- getCurrentDateInMillis
  cpuT2     <- L.runIO getCPUTime
  apiTag <- L.getOptionLocal ApiTag
  mid <- L.getOptionLocal MerchantID 
  let dblog = DBLogEntry {
      _log_type     = "DB"
    , _action       = "UPDATE_ALL"
    , _data        = case res of
                        Left err -> A.String (T.pack $ show err)
                        Right _  -> A.Null
    , _latency      = t2 - t1
    , _model        = tableName @(table Identity)
    , _cpuLatency   = getLatencyInMicroSeconds (cpuT2 - cpuT1)
    , _source       = if isEnabled then "KV" else "DB"
    , _apiTag       = apiTag
    , _merchant_id = mid
    }
  L.logInfoV ("DB" :: Text) dblog
  incrementMetric KVAction dblog
  pure res

updateKVAndDBResults :: forall be table beM m.
  ( HasCallStack,
    BeamRuntime be beM,
    BeamRunner beM,
    Model be table,
    MeshMeta be table,
    B.HasQBuilder be,
    KVConnector (table Identity),
    FromJSON (table Identity),
    ToJSON (table Identity),
    Serialize.Serialize (table Identity),
    -- Show (table Identity), --debugging purpose
    L.MonadFlow m
  ) => MeshConfig -> Where be table -> Either DBError [table Identity] -> MeshResult [table Identity] -> [(Text, A.Value)] -> m (MeshResult [table Identity])
updateKVAndDBResults meshCfg whereClause eitherDbRows eitherKvRows updVals = do
  case (eitherDbRows, eitherKvRows) of
    (Right allDBRows, Right allKVRows) -> do
      let kvRows = findAllMatching whereClause allKVRows
          dbRows = findAllMatching whereClause allDBRows
      let kvPkeys = map getLookupKeyByPKey kvRows
          uniqueDbRes = filter (\r -> getLookupKeyByPKey r `notElem` kvPkeys) dbRows
      updateKVRowRes <- mapM (updateObjectRedis meshCfg updVals whereClause) kvRows
      updateDBRowRes <- mapM (updateDBRowInRedis meshCfg updVals whereClause) uniqueDbRes
      pure $ foldEither (updateKVRowRes ++ updateDBRowRes)
    (_, Left err) -> pure $ Left err
    (Left err, _) -> pure $ Left (MDBError err)

---------------- Find -----------------------
findWithKVConnector :: forall be table beM m.
  ( HasCallStack,
    BeamRuntime be beM,
    BeamRunner beM,
    B.HasQBuilder be,
    Model be table,
    MeshMeta be table,
    KVConnector (table Identity),
    FromJSON (table Identity),
    ToJSON (table Identity),
    Serialize.Serialize (table Identity),
    L.MonadFlow m,
    Show (table Identity)
  ) =>
  DBConfig beM ->
  MeshConfig ->
  Where be table ->
  m (MeshResult (Maybe (table Identity)))
findWithKVConnector dbConf meshCfg whereClause = do --This function fetches all possible rows and apply where clause on it.
  L.logDebugT "findWithKVConnector: " $ "whereClause : " <> (tname <> ":" <> (show . length $ whereClause))
  inMemResult <- searchInMemoryCache
  L.logDebugT "findWithKVConnector" $ tname <> ":" <> "In Mem result : " <> (show inMemResult)
  case inMemResult of
    EntryValid valEntry -> entryToTable valEntry
    TableIneligible -> kvFetch 
    EntryExpired valEntry Nothing -> entryToTable valEntry
    EntryExpired valEntry (Just keyForInMemConfig) -> useExpiredAndForkKvFetchAndSave keyForInMemConfig valEntry
    EntryNotFound keyForInMemConfig ->  kvFetchAndSave keyForInMemConfig 
    UnknownError err -> pure . Left $ err

  where

    tname :: Text = (tableName @(table Identity))
    
    useExpiredAndForkKvFetchAndSave :: forall a. Text -> a -> m (MeshResult (Maybe (table Identity)))
    useExpiredAndForkKvFetchAndSave key val = do
      L.logInfoT "findWithKVConnector" $ "Starting Timeout for redis-fetch for in-mem-config"
      L.fork $ 
        do
          void $ L.runIO $ threadDelayMilisec 5
          void $ L.releaseConfigLock key      -- TODO  Check if release fails
      L.logInfoT "findWithKVConnector" $ "Initiating updation of key <" <> key <>"> in-mem-config"
      L.fork $ (kvFetchAndSave key) 
      entryToTable val

    searchInMemoryCache :: m (InMemCacheResult)
    searchInMemoryCache = do
      shouldSearchInMemoryCache <- isInMemConfigEnabled $ tableName @(table Identity)
      L.logDebugT "findWithKVConnector: " $ "shouldSearchInMemoryCache: " <> (tname <> ":" <> show shouldSearchInMemoryCache)
      if shouldSearchInMemoryCache && length whereClause == 1 
      then do
        eitherPKeys <- getPrimaryKeys 
        L.logDebugT "findWithKVConnector: " $ "eitherPKeys: " <> (tname <> ":" <> show eitherPKeys)
        let
          decodeTable :: ByteString -> Maybe (table Identity)
          decodeTable = A.decode . BSL.fromStrict
        checkAndStartLooper meshCfg decodeTable
        case eitherPKeys of
          Right [] -> return TableIneligible
          Right [[pKey]] -> do
            let k = decodeUtf8 pKey
            mbVal <- L.getConfig k
            case mbVal of
              Just val -> do
                L.logInfoT "findWithKVConnector" $ "Found key: <" <> k <> "> in in-mem-config"
                L.logDebugT "findWithKVConnector" $ "key val: " <> show val
                currentTime <- L.getCurrentTimeUTC
                if val.ttl > currentTime
                  then do
                    L.logInfoT "findWithKVConnector" $ "Key TTL Valid"
                    return $ EntryValid val.entry
                  else do
                    L.logInfoT "findWithKVConnector" $ "Key TTL Not Valid"
                    ifM (notM $ L.acquireConfigLock k)
                      (return $ EntryExpired val.entry Nothing)    --then
                      (return $ EntryExpired val.entry $ Just k)   --else
              Nothing -> return . EntryNotFound $ k
          Right _ -> return TableIneligible
          Left err -> return . UnknownError $ err
      else
        return TableIneligible

    entryToTable :: forall a. a -> m (MeshResult (Maybe (table Identity)))
    entryToTable x = return . Right . Just $ ((unsafeCoerce @_ @(table Identity)) x)

    getPrimaryKeys :: m (MeshResult [[ByteString]])
    getPrimaryKeys = do
      let 
        keyAndValueCombinations = getFieldsAndValuesFromClause meshModelTableEntityDescriptor (And whereClause)
        andCombinations = map (uncurry zip . applyFPair (map (T.intercalate "_") . sortOn (Down . length) . nonEmptySubsequences) . unzip . sort) keyAndValueCombinations
        modelName = tableName @(table Identity)
        keyHashMap = keyMap @(table Identity)
      L.logDebugT "findWithKVConnector" (show keyAndValueCombinations)
      eitherKeyRes <- mapM (getPrimaryKeyFromFieldsAndValues modelName meshCfg keyHashMap) andCombinations
      L.logDebugT "findWithKVConnector" (show eitherKeyRes)
      pure $ foldEither eitherKeyRes

    kvFetchAndSave :: Text -> m (MeshResult (Maybe (table Identity)))
    kvFetchAndSave key = do
      val <- kvFetch
      case val of
        Right (Just val') -> do
          void $ setInConfig key val'
        _ -> return ()
      pure val

    kvFetch :: m (MeshResult (Maybe (table Identity)))
    kvFetch = do
      isEnabled <- isKVEnabled (tableName @(table Identity))
      t1        <- getCurrentDateInMillis
      cpuT1     <- L.runIO getCPUTime
      res <- if isEnabled
        then do
          L.logDebugT "findWithKVConnector" ("Taking KV Path for " <> tableName @(table Identity))
          eitherKvRows <- findOneFromRedis meshCfg whereClause
          case eitherKvRows of
            Right [] -> do
              L.logDebugT "findWithKVConnector" "Falling back to SQL - Nothing found in KV"
              findOneFromDB dbConf whereClause
            Right rows -> do
              L.logDebugT "findWithKVConnector" ("findOneFromRedis = " <> show (length rows) <> "rows")
              pure $ Right $ findOneMatching whereClause rows
            Left err -> pure $ Left err
        else do
          L.logDebugT "findWithKVConnector" ("Taking SQLDB Path for " <> tableName @(table Identity))
          whereClauseDiffCheck whereClause
          findOneFromDB dbConf whereClause
      t2        <- getCurrentDateInMillis
      cpuT2     <- L.runIO getCPUTime
      apiTag <- L.getOptionLocal ApiTag
      mid <- L.getOptionLocal MerchantID 
      let dblog =  DBLogEntry {
          _log_type     = "DB"
        , _action       = "FIND"
        , _data        = case res of
                            Left err -> A.String (T.pack $ show err)
                            Right _  -> A.Null
        , _latency      = t2 - t1
        , _model        = tableName @(table Identity)
        , _cpuLatency   = getLatencyInMicroSeconds (cpuT2 - cpuT1)
        , _source       = if isEnabled then "KV" else "DB"
        , _apiTag       = apiTag
        , _merchant_id = mid
        }
      L.logInfoV ("DB" :: Text) dblog
      incrementMetric KVAction dblog
      pure res

    setInConfig :: Text -> table Identity -> m ()
    setInConfig k model = do
      newTtl <- getConfigEntryNewTtl
      L.logInfoT "findWithKVConnector" $ "Found key <" <> k <> "> in redis. Now setting in in-mem-config"
      void $ L.setConfig k (mkConfigEntry  newTtl model) 


-- TODO: Once record matched in redis stop and return it
findOneFromRedis :: forall be table beM m.
  ( HasCallStack,
    BeamRuntime be beM,
    BeamRunner beM,
    B.HasQBuilder be,
    Model be table,
    MeshMeta be table,
    KVConnector (table Identity),
    Serialize.Serialize (table Identity),
    FromJSON (table Identity),
    L.MonadFlow m
  ) =>
  MeshConfig -> Where be table -> m (MeshResult [table Identity])
findOneFromRedis meshCfg whereClause = do
  let keyAndValueCombinations = getFieldsAndValuesFromClause meshModelTableEntityDescriptor (And whereClause)
      andCombinations = map (uncurry zip . applyFPair (map (T.intercalate "_") . sortOn (Down . length) . nonEmptySubsequences) . unzip . sort) keyAndValueCombinations
      modelName = tableName @(table Identity)
      keyHashMap = keyMap @(table Identity)
  L.logDebugT "findWithKVConnector" (show $ length keyAndValueCombinations)
  eitherKeyRes <- mapM (getPrimaryKeyFromFieldsAndValues modelName meshCfg keyHashMap) andCombinations
  L.logDebugT "findWithKVConnector" (show $ length eitherKeyRes)
  case foldEither eitherKeyRes of
    Right keyRes -> do
      allRowsRes <- mapM (getDataFromPKeysRedis meshCfg) keyRes
      pure $ mapRight concat (foldEither allRowsRes)
    Left err -> pure $ Left err

findOneFromDB :: forall be table beM m.
  ( HasCallStack,
    BeamRuntime be beM,
    BeamRunner beM,
    B.HasQBuilder be,
    Model be table,
    MeshMeta be table,
    KVConnector (table Identity),
    FromJSON (table Identity),
    L.MonadFlow m
  ) =>
  DBConfig beM -> Where be table -> m (MeshResult (Maybe (table Identity)))
findOneFromDB dbConf whereClause = do
  let findQuery = DB.findRow (sqlSelect ! #where_ whereClause ! defaults)
  mapLeft MDBError <$> runQuery dbConf findQuery


-- Need to recheck offset implementation
findAllWithOptionsKVConnector :: forall be table beM m.
  ( HasCallStack,
    BeamRuntime be beM,
    Model be table,
    MeshMeta be table,
    KVConnector (table Identity),
    Serialize.Serialize (table Identity),
    Show (table Identity),
    FromJSON (table Identity),
    L.MonadFlow m, B.HasQBuilder be, BeamRunner beM) =>
  DBConfig beM ->
  MeshConfig ->
  Where be table ->
  OrderBy table ->
  Maybe Int ->
  Maybe Int ->
  m (MeshResult [table Identity])
findAllWithOptionsKVConnector dbConf meshCfg whereClause orderBy mbLimit mbOffset = do
  isEnabled <- isKVEnabled (tableName @(table Identity))
  t1        <- getCurrentDateInMillis
  cpuT1     <- L.runIO getCPUTime
  res <- if isEnabled
    then do
      L.logDebugT "findAllWithOptionsKVConnector" ("Taking KV Path for " <> tableName @(table Identity))
      kvRes <- redisFindAll meshCfg whereClause
      case kvRes of
        Right kvRows -> do
          let matchedKVRows = findAllMatching whereClause kvRows
              offset = fromMaybe 0 mbOffset
              shift = length matchedKVRows
              updatedOffset = if offset - shift >= 0 then offset - shift else 0
              findAllQueryUpdated = DB.findRows (sqlSelect'
                ! #where_ whereClause
                ! #orderBy (Just [orderBy])
                ! #limit ((shift +) <$> mbLimit)
                ! #offset (Just updatedOffset) -- Offset is 0 in case mbOffset Nothing
                ! defaults)
          dbRes <- runQuery dbConf findAllQueryUpdated
          case dbRes of
            Left err -> pure $ Left $ MDBError err
            Right [] -> pure $ Right $ applyOptions offset matchedKVRows
            Right dbRows -> do
              let mergedRows = mergeKVAndDBResults dbRows matchedKVRows
              if isJust mbOffset
                then do
                  let noOfRowsFelledLeftSide = calculateLeftFelledRedisEntries kvRows dbRows
                  pure $ Right $ applyOptions ((if updatedOffset == 0 then offset else shift) - noOfRowsFelledLeftSide) mergedRows
                else pure $ Right $ applyOptions 0 mergedRows
        Left err -> pure $ Left err
    else do
      let findAllQuery = DB.findRows (sqlSelect'
            ! #where_ whereClause
            ! #orderBy (Just [orderBy])
            ! #limit mbLimit
            ! #offset mbOffset
            ! defaults)
      L.logDebugT "findAllWithOptionsKVConnector" ("Taking SQLDB Path for " <> tableName @(table Identity))
      whereClauseDiffCheck whereClause
      mapLeft MDBError <$> runQuery dbConf findAllQuery
  t2        <- getCurrentDateInMillis
  cpuT2     <- L.runIO getCPUTime
  apiTag <- L.getOptionLocal ApiTag
  mid <- L.getOptionLocal MerchantID 
  let dblog = DBLogEntry {
      _log_type     = "DB"
    , _action       = "FIND_ALL"
    , _data        = case res of
                        Left err -> A.String (T.pack $ show err)
                        Right _  -> A.Null
    , _latency      = t2 - t1
    , _model        = tableName @(table Identity)
    , _cpuLatency   = getLatencyInMicroSeconds (cpuT2 - cpuT1)
    , _source       = if isEnabled then "KV" else "DB"
    , _apiTag       = apiTag
    , _merchant_id = mid
   }
  L.logInfoV ("DB" :: Text) dblog
  incrementMetric KVAction dblog
  pure res

    where
      applyOptions :: Int -> [table Identity] -> [table Identity]
      applyOptions shift rows = do
        let cmp = case orderBy of
              (Asc col) -> compareCols (fromColumnar' . col . columnize) True
              (Desc col) -> compareCols (fromColumnar' . col . columnize) False
        let resWithoutLimit = (drop shift . sortBy cmp) rows
        maybe resWithoutLimit (`take` resWithoutLimit) mbLimit

      compareCols :: (Ord value) => (table Identity -> value) -> Bool -> table Identity -> table Identity -> Ordering
      compareCols col isAsc r1 r2 = if isAsc then compare (col r1) (col r2) else compare (col r2) (col r1)

      calculateLeftFelledRedisEntries :: [table Identity] -> [table Identity] -> Int
      calculateLeftFelledRedisEntries kvRows dbRows = do
        case orderBy of
          (Asc col) -> do
            let dbMn = maximum $ map (fromColumnar' . col . columnize) dbRows
            length $ filter (\r -> dbMn > fromColumnar' (col $ columnize r)) kvRows
          (Desc col) -> do
            let dbMx = maximum $ map (fromColumnar' . col . columnize) dbRows
            length $ filter (\r -> dbMx < fromColumnar' (col $ columnize r)) kvRows

findAllWithKVConnector :: forall be table beM m.
  ( HasCallStack,
    BeamRuntime be beM,
    Model be table,
    MeshMeta be table,
    KVConnector (table Identity),
    FromJSON (table Identity),
    Serialize.Serialize (table Identity),
    L.MonadFlow m, B.HasQBuilder be, BeamRunner beM) =>
  DBConfig beM ->
  MeshConfig ->
  Where be table ->
  m (MeshResult [table Identity])
findAllWithKVConnector dbConf meshCfg whereClause = do
  let findAllQuery = DB.findRows (sqlSelect ! #where_ whereClause ! defaults)
  isEnabled <- isKVEnabled (tableName @(table Identity))
  t1        <- getCurrentDateInMillis
  cpuT1     <- L.runIO getCPUTime
  res <- if isEnabled
    then do
      L.logDebugT "findAllWithKVConnector" ("Taking KV Path for " <> tableName @(table Identity))
      kvRes <- redisFindAll meshCfg whereClause
      case kvRes of
        Right kvRows -> do
          dbRes <- runQuery dbConf findAllQuery
          case dbRes of
            Left err -> pure $ Left $ MDBError err
            Right dbRows -> pure $ Right $ (mergeKVAndDBResults dbRows . findAllMatching whereClause) kvRows
        Left err -> pure $ Left err
    else do
      L.logDebugT "findAllWithKVConnector" ("Taking SQLDB Path for " <> tableName @(table Identity))
      whereClauseDiffCheck whereClause
      mapLeft MDBError <$> runQuery dbConf findAllQuery
  t2        <- getCurrentDateInMillis
  cpuT2     <- L.runIO getCPUTime
  apiTag <- L.getOptionLocal ApiTag
  mid <- L.getOptionLocal MerchantID 
  let dblog =DBLogEntry {
      _log_type     = "DB"
    , _action       = "FIND_ALL"
    , _data        = case res of
                        Left err -> A.String (T.pack $ show err)
                        Right _  -> A.Null
    , _latency      = t2 - t1
    , _model        = tableName @(table Identity)
    , _cpuLatency   = getLatencyInMicroSeconds (cpuT2 - cpuT1)
    , _source       = if isEnabled then "KV" else "DB"
    , _apiTag       = apiTag
    , _merchant_id = mid
    }
  L.logInfoV ("DB" :: Text) dblog
  incrementMetric KVAction dblog
  pure res

redisFindAll :: forall be table beM m.
  ( HasCallStack,
    BeamRuntime be beM,
    Model be table,
    MeshMeta be table,
    KVConnector (table Identity),
    FromJSON (table Identity),
    Serialize.Serialize (table Identity),
    L.MonadFlow m, B.HasQBuilder be, BeamRunner beM) =>
  MeshConfig ->
  Where be table ->
  m (MeshResult [table Identity])
redisFindAll meshCfg whereClause = do
  let keyAndValueCombinations = getFieldsAndValuesFromClause meshModelTableEntityDescriptor (And whereClause)
      andCombinations = map (uncurry zip . applyFPair (map (T.intercalate "_") . sortOn (Down . length) . nonEmptySubsequences) . unzip . sort) keyAndValueCombinations
      modelName = tableName @(table Identity)
      keyHashMap = keyMap @(table Identity)
  eitherKeyRes <- mapM (getPrimaryKeyFromFieldsAndValues modelName meshCfg keyHashMap) andCombinations
  case foldEither eitherKeyRes of
    Right keyRes -> do
      L.logDebugT "redisFindAll" ("Found " <> show (length eitherKeyRes) <> " pKeys for " <> modelName)
      allRowsRes <- mapM (getDataFromPKeysRedis meshCfg) keyRes
      pure $ mapRight concat (foldEither allRowsRes)
    Left err -> pure $ Left err

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

mergeKVAndDBResults :: KVConnector (table Identity) => [table Identity] -> [table Identity] -> [table Identity]
mergeKVAndDBResults dbRows kvRows = do
  let kvPkeys = map getLookupKeyByPKey kvRows
      uniqueDbRes = filter (\r -> getLookupKeyByPKey r `notElem` kvPkeys) dbRows
  kvRows ++ uniqueDbRes

--
--
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

whereClauseDiffCheck :: forall be table m. 
  ( L.MonadFlow m
  , Model be table
  , MeshMeta be table
  , KVConnector (table Identity)
  ) =>
  Where be table -> m ()
whereClauseDiffCheck whereClause = do
  let keyAndValueCombinations = getFieldsAndValuesFromClause meshModelTableEntityDescriptor (And whereClause)
      andCombinations = map (uncurry zip . applyFPair (map (T.intercalate "_") . sortOn (Down . length) . nonEmptySubsequences) . unzip . sort) keyAndValueCombinations
      keyHashMap = keyMap @(table Identity)
      failedKeys = catMaybes $ map (atMay keyAndValueCombinations) $ findIndices (checkForPrimaryOrSecondary keyHashMap) andCombinations
  when (not $ null failedKeys) $ L.logInfoT "WHERE_DIFF_CHECK" (tableName @(table Identity) <> ": " <> show (map (map fst) failedKeys))
  where
    checkForPrimaryOrSecondary _ [] = True
    checkForPrimaryOrSecondary keyHashMap ((k, _) : xs) =
      case HM.member k keyHashMap of
        True -> False
        _ -> checkForPrimaryOrSecondary keyHashMap xs

getLatencyInMicroSeconds :: Integer -> Integer
getLatencyInMicroSeconds execTime = execTime `div` 1000000
