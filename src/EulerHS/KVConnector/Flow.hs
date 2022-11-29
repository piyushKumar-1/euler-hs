{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
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

import           EulerHS.Prelude hiding (maximum)
import           EulerHS.KVConnector.Types (KVConnector(..), MeshConfig, MeshResult, MeshError(..), MeshMeta(..), SecondaryKey(..), tableName, keyMap, getLookupKeyByPKey, getSecondaryLookupKeys, applyFPair)
import           EulerHS.KVConnector.DBSync (getCreateQuery, getUpdateQuery, getDbUpdateCommandJson, meshModelTableEntityDescriptor, toPSJSON, DBCommandVersion(..))
import           EulerHS.KVConnector.Utils
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
  if isEnabled
    then do
      L.logDebug @Text "createWoReturingKVConnector" ("Taking KV Path for" <> show (tableName @(table Identity)))
      mapRight (const ()) <$> createKV meshCfg value
    else do
      L.logDebug @Text "createWoReturingKVConnector" ("Taking SQLDB Path for" <> show (tableName @(table Identity)))
      res <- createSqlWoReturing dbConf value
      case res of
        Right _ -> return $ Right ()
        Left e -> return $ Left $ MDBError e


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
  if isEnabled
    then do
      L.logDebug @Text "createWithKVConnector" ("Taking KV Path for" <> show (tableName @(table Identity)))
      createKV meshCfg value
    else do
      L.logDebug @Text "createWithKVConnector" ("Taking SQLDB Path for" <> show (tableName @(table Identity)))
      res <- createReturning dbConf value Nothing
      case res of
        Right val -> return $ Right val
        Left e -> return $ Left $ MDBError e

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
  if isEnabled
    then do
      L.logDebug @Text "updateWoReturningWithKVConnector" ("Taking KV Path for" <> show (tableName @(table Identity)) )
      mapRight (const ()) <$> updateKV dbConf meshCfg setClause whereClause
    else do
      L.logDebug @Text "updateWoReturningWithKVConnector" ("Taking SQLDB Path for" <> show (tableName @(table Identity)))
      whereClauseDiffCheck whereClause
      res <- updateOneSqlWoReturning dbConf setClause whereClause
      case res of
        Right val -> return $ Right val
        Left e -> return $ Left $ MDBError e

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
  if isEnabled
    then do
      L.logDebug @Text "updateWithKVConnector" ("Taking KV Path for" <> show (tableName @(table Identity)))
      updateKV dbConf meshCfg setClause whereClause
    else do
      L.logDebug @Text "updateWithKVConnector" ("Taking SQLDB Path for" <> show (tableName @(table Identity)))
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
              L.logDebug @Text "RedisUpdAnswer returned: " (show kvdbRes)
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
  updateObjectRedis meshCfg updVals whereClause obj

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
  updateObjectRedis meshCfg updVals whereClause obj

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
  if isEnabled
    then do
      L.logDebug @Text "updateAllReturningWithKVConnector" ("Taking KV Path for" <> show (tableName @(table Identity)))
      let findAllQuery = DB.findRows (sqlSelect ! #where_ whereClause ! defaults)
          updVals = jsonKeyValueUpdates setClause
      kvRows <- redisFindAll meshCfg whereClause
      dbRows <- runQuery dbConf findAllQuery
      updateKVAndDBResults meshCfg whereClause dbRows kvRows updVals
    else do
      L.logDebug @Text "updateAllReturningWithKVConnector" ("Taking SQLDB Path for" <> show (tableName @(table Identity)))
      whereClauseDiffCheck whereClause
      let updateQuery = DB.updateRowsReturningList $ sqlUpdate ! #set setClause ! #where_ whereClause
      res <- runQuery dbConf updateQuery
      case res of
        Right x -> return $ Right x
        Left e -> return $ Left $ MDBError e

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
  if isEnabled
    then do
      L.logDebug @Text "updateAllWithKVConnector" ("Taking KV Path for" <> show (tableName @(table Identity)))
      let findAllQuery = DB.findRows (sqlSelect ! #where_ whereClause ! defaults)
          updVals = jsonKeyValueUpdates setClause
      kvRows <- redisFindAll meshCfg whereClause
      dbRows <- runQuery dbConf findAllQuery
      mapRight (const ()) <$> updateKVAndDBResults meshCfg whereClause dbRows kvRows updVals
    else do
      L.logDebug @Text "updateAllWithKVConnector" ("Taking SQLDB Path for" <> show (tableName @(table Identity)))
      whereClauseDiffCheck whereClause
      let updateQuery = DB.updateRows $ sqlUpdate ! #set setClause ! #where_ whereClause
      res <- runQuery dbConf updateQuery
      case res of
        Right x -> return $ Right x
        Left e -> return $ Left $ MDBError e

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
    Serialize.Serialize (table Identity),
    L.MonadFlow m,
    Show (table Identity)
  ) =>
  DBConfig beM ->
  MeshConfig ->
  Where be table ->
  m (MeshResult (Maybe (table Identity)))
findWithKVConnector dbConf meshCfg whereClause = do --This function fetches all possible rows and apply where clause on it.
  isEnabled <- isKVEnabled (tableName @(table Identity))
  if isEnabled
    then do
      L.logDebug @Text "findWithKVConnector" ("Taking KV Path for" <> show (tableName @(table Identity)))
      eitherKvRows <- findOneFromRedis meshCfg whereClause
      case eitherKvRows of
        Right [] -> do
          L.logDebug @Text "findWithKVConnector" "Falling back to SQL - Nothing found in KV"
          findOneFromDB dbConf whereClause
        Right rows -> do
          L.logDebugT "findWithKVConnector" ("findOneFromRedis = " <> show (length rows) <> "rows")
          pure $ Right $ findOneMatching whereClause rows
        Left err -> pure $ Left err
    else do
      L.logDebug @Text "findWithKVConnector" ("Taking SQLDB Path for" <> show (tableName @(table Identity)))
      whereClauseDiffCheck whereClause
      findOneFromDB dbConf whereClause

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
  if isEnabled
    then do
      L.logDebug @Text "findAllWithOptionsKVConnector" ("Taking KV Path for" <> show (tableName @(table Identity)))
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
      L.logDebug @Text "findAllWithOptionsKVConnector" ("Taking SQLDB Path for" <> show (tableName @(table Identity)))
      whereClauseDiffCheck whereClause
      mapLeft MDBError <$> runQuery dbConf findAllQuery

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
  if isEnabled
    then do
      L.logDebug @Text "findAllWithKVConnector" ("Taking KV Path for" <> show (tableName @(table Identity)))
      kvRes <- redisFindAll meshCfg whereClause
      case kvRes of
        Right kvRows -> do
          dbRes <- runQuery dbConf findAllQuery
          case dbRes of
            Left err -> pure $ Left $ MDBError err
            Right dbRows -> pure $ Right $ (mergeKVAndDBResults dbRows . findAllMatching whereClause) kvRows
        Left err -> pure $ Left err
    else do
      L.logDebug @Text "findAllWithKVConnector" ("Taking SQLDB Path for" <> show (tableName @(table Identity)))
      whereClauseDiffCheck whereClause
      mapLeft MDBError <$> runQuery dbConf findAllQuery

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
  L.logDebugT "redisFindAll" (show (length eitherKeyRes) <> " rows" )
  case foldEither eitherKeyRes of
    Right keyRes -> do
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
    [[(key, show . snd $ (toPSJSON @be @table) (key, A.toJSON val))]]
  _ -> []

  where
    processAnd xs [] = xs
    processAnd [] ys = ys
    processAnd xs ys = [x ++ y | x <-xs, y <- ys]
    processOr xs = concatMap (getFieldsAndValuesFromClause dt) xs

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