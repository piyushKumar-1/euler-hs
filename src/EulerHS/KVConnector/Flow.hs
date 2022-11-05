
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module EulerHS.KVConnector.Flow
  (
    createWithKVConnector,
    findWithKVConnector,
    updateWithKVConnector,
    findAllWithKVConnector,
    updateAllWithKVConnector
  )
 where

import           EulerHS.Prelude
import           EulerHS.KVConnector.Types (KVConnector(..), MeshConfig, MeshResult, MeshError(..), MeshMeta(..), SecondaryKey(..), tableName, keyMap, getLookupKeyByPKey, getSecondaryLookupKeys, applyFPair)
import           EulerHS.KVConnector.DBSync (getCreateQuery, getUpdateQuery, getDbUpdateCommandJson, meshModelTableEntityDescriptor, DBCommandVersion(..))
import qualified Data.Aeson as A
import           Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy as BSL
import           Data.List (span)
import qualified Data.Text as T
import qualified EulerHS.Language as L
import           EulerHS.Extra.Language (getOrInitSqlConn)
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as Map
import           EulerHS.SqlDB.Types (BeamRunner, BeamRuntime, DBConfig, DBError)
import qualified EulerHS.SqlDB.Language as DB
import           Sequelize (fromColumnar', modelTableName, columnize, sqlSelect, sqlUpdate, Model, Where, Clause(..), Term(..), Set(..))
import           EulerHS.CachedSqlDBQuery (createReturning, SqlReturning)
import qualified Database.Beam as B
import qualified Database.Beam.Schema.Tables as B
import           Data.Either.Extra (mapRight, mapLeft)
import           Named (defaults, (!))
import           Text.Casing (quietSnake)
import           Unsafe.Coerce (unsafeCoerce)



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
    Show (table Identity),
    KVConnector (table Identity),
    L.MonadFlow m) =>
  DBConfig beM ->
  MeshConfig ->
  table Identity ->
  m (MeshResult (table Identity))
createWithKVConnector dbConf meshCfg value = do
  if meshCfg.meshEnabled
    then do
      L.logDebug @Text "createWithKVConnector" "Taking KV Path"
      autoId <- getAutoIncId meshCfg (tableName @(table Identity))
      L.logWarning @Text "createWithKVConnector" (show autoId)
      case autoId of
        Right _id -> do
          -- TODO: Key - id is hardcoded to replace AutoIncrId. Make it Generic
          let val = unsafeJSONSet @Text "id" (T.pack . show $ _id) value
          let pKeyText = getLookupKeyByPKey val
              pKey = fromString . T.unpack $ pKeyText
          time <- fromIntegral <$> L.getCurrentDateInMillis
          let qCmd = getCreateQuery (modelTableName @table) V1 pKeyText time meshCfg.meshDBName val
          L.logDebugT "createWithKVConnector" (show $ getSecondaryLookupKeys val)
          mapM_ (\secIdx -> do
            let sKey = fromString . T.unpack $ secIdx
            _ <- L.runKVDB meshCfg.kvRedis $ L.sadd sKey [pKey]
            L.runKVDB meshCfg.kvRedis $ L.expire sKey meshCfg.redisTtl
            ) $ getSecondaryLookupKeys val
          _ <- L.runKVDB meshCfg.kvRedis $ L.xadd
                (encodeUtf8 (meshCfg.ecRedisDBStream <> getShardedHashTag pKeyText))
                L.AutoID
                [("command", BSL.toStrict $ A.encode qCmd)]
          kvRes <- L.runKVDB meshCfg.kvRedis $ L.setex pKey meshCfg.redisTtl (BSL.toStrict $ A.encode val)
          case kvRes of
            Right _ -> pure $ Right val
            Left err -> pure $ Left (MRedisError err)
        Left err -> return $ Left err
    else do
      L.logDebug @Text "createWithKVConnector" "Taking SQLDB Path"
      res <- createReturning dbConf value Nothing
      case res of
        Right val -> return $ Right val
        Left e -> return $ Left $ MDBError e

---------------- Update -----------------

updateWithKVConnector :: forall be table beM m.
  ( HasCallStack,
    BeamRuntime be beM,
    BeamRunner beM,
    Model be table,
    MeshMeta table,
    B.HasQBuilder be,
    KVConnector (table Identity),
    FromJSON (table Identity),
    ToJSON (table Identity),
    Show (table Identity), --debugging purpose
    L.MonadFlow m
  ) =>
  DBConfig beM ->
  MeshConfig ->
  [Set be table] ->
  Where be table ->
  m (MeshResult (table Identity))
updateWithKVConnector dbConf meshCfg setClause whereClause = do
  if meshCfg.meshEnabled
    then do
      L.logDebug @Text "updateWithKVConnector" "Taking KV Path"
      eitherFindRes <- findOneFromRedis meshCfg whereClause
      L.logDebugT "updateWithKVConnector" ("findOneFromRedis = " <> show eitherFindRes)
      let updVals = jsonKeyValueUpdates setClause
      case eitherFindRes of
        Right r -> case findAllMatching whereClause r of
          [obj] -> updateKVRowInRedis meshCfg whereClause updVals obj
          [] -> do
            L.logDebugT "updateWithKVConnector" "Found nothing from findWithKVConnectorRes - Falling back to DB and recaching"
            let findQuery = DB.findRow (sqlSelect ! #where_ whereClause ! defaults)
            dbRes <- runQuery dbConf findQuery
            case dbRes of
              Right (Just obj) -> do
                updateDBRowInRedis meshCfg updVals whereClause obj
              Right Nothing -> pure $ Left (MUpdateFailed ("No value found for table " <> tableName @(table Identity)))
              Left err -> pure $ Left (MDBError err)
          _ -> do
            L.logDebugT "updateWithKVConnector" "Found more than one record in redis - Update failed"
            pure $ Left $ MUpdateFailed "Found more than one record in redis"
        Left err -> pure $ Left err
    else do
      L.logDebug @Text "updateWithKVConnector" "Taking SQLDB Path"
      let updateQuery = DB.updateRowsReturningList $ sqlUpdate ! #set setClause ! #where_ whereClause
      res <- runQuery dbConf updateQuery
      case res of
        Right [x] -> return $ Right x
        Right xs -> do
          let message = "DB returned \"" <> show xs <> "\" after update"
          L.logError @Text "create" message
          return $ Left $ UnexpectedError message
        Left e -> return $ Left $ MDBError e

updateObjectRedis :: forall be table m.
  (
    Model be table,
    MeshMeta table,
    KVConnector (table Identity),
    ToJSON (table Identity),
    FromJSON (table Identity),
    L.MonadFlow m
  ) =>
  MeshConfig -> [(Text, A.Value)] -> Where be table -> table Identity -> m (MeshResult (table Identity))
updateObjectRedis meshCfg updVals whereClause obj = do
  if isAnySkeyModified
    then pure $ Left (MUpdateFailed "Update failed - A part of skey is present in set clause")
    else do
    case updateModel obj updVals of
      Left err -> return $ Left err
      Right updatedModel -> do
        time <- fromIntegral <$> L.getCurrentDateInMillis
        let pKeyText = getLookupKeyByPKey obj
            pKey = fromString . T.unpack $ pKeyText
        let updateCmd = getDbUpdateCommandJson (modelTableName @table) updVals whereClause
            qCmd = getUpdateQuery V1 pKeyText time meshCfg.meshDBName updateCmd
        _ <- L.runKVDB meshCfg.kvRedis $ L.xadd
          (encodeUtf8 (meshCfg.ecRedisDBStream <> getShardedHashTag pKeyText))
          L.AutoID
          [("command", BSL.toStrict $ A.encode qCmd)]
        kvdbRes <- L.runKVDB meshCfg.kvRedis $ L.setex pKey meshCfg.redisTtl (BSL.toStrict $ A.encode updatedModel)
        L.logDebug @Text "RedisUpdAnswer" . T.pack . show $ kvdbRes
        pure $ mapLeft MDecodingError (resultToEither $ A.fromJSON updatedModel)

  where
    isAnySkeyModified :: Bool -- TODO: Optimise this
    isAnySkeyModified = do
      let olderSkeys = map (\(SKey s) -> s) (secondaryKeys obj)
          updValsMap = HM.fromList (map (\p -> (fst p, True)) updVals)
          (modifiedSkeysValues, _) = applyFPair (map getSortedKeyAndValue) $
                                                span (`isKeyModified` updValsMap) olderSkeys
      not (null modifiedSkeysValues)

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
    MeshMeta table,
    B.HasQBuilder be,
    KVConnector (table Identity),
    FromJSON (table Identity),
    ToJSON (table Identity),
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

updateDBRowInRedis :: forall be table m.
  (
    Model be table,
    MeshMeta table,
    KVConnector (table Identity),
    ToJSON (table Identity),
    FromJSON (table Identity),
    L.MonadFlow m
  ) =>
  MeshConfig -> [(Text, A.Value)] -> Where be table -> table Identity -> m (MeshResult (table Identity))
updateDBRowInRedis meshCfg updVals whereClause obj = do
  let pKey = fromString . T.unpack $ getLookupKeyByPKey obj
  mapM_ (\secIdx -> do -- Recaching Skeys in redis
    let sKey = fromString . T.unpack $ secIdx
    _ <- L.runKVDB meshCfg.kvRedis $ L.sadd sKey [pKey]
    L.runKVDB meshCfg.kvRedis $  L.expire sKey meshCfg.redisTtl
    ) $ getSecondaryLookupKeys obj
  updateObjectRedis meshCfg updVals whereClause obj


updateAllWithKVConnector :: forall be table beM m.
  ( HasCallStack,
    BeamRuntime be beM,
    BeamRunner beM,
    Model be table,
    MeshMeta table,
    B.HasQBuilder be,
    KVConnector (table Identity),
    FromJSON (table Identity),
    ToJSON (table Identity),
    -- Show (table Identity), --debugging purpose
    L.MonadFlow m
  ) =>
  DBConfig beM ->
  MeshConfig ->
  [Set be table] ->
  Where be table ->
  m (MeshResult [table Identity])
updateAllWithKVConnector dbConf meshCfg setClause whereClause = do
  if meshCfg.meshEnabled
    then do
      L.logDebug @Text "updateAllWithKVConnector" "Taking KV Path"
      let findAllQuery = DB.findRows (sqlSelect ! #where_ whereClause ! defaults)
          updVals = jsonKeyValueUpdates setClause
      kvRows <- redisFindAll meshCfg whereClause
      dbRows <- runQuery dbConf findAllQuery
      updateKVAndDBResults dbRows kvRows updVals
    else do
      L.logDebug @Text "updateAllWithKVConnector" "Taking SQLDB Path"
      let updateQuery = DB.updateRowsReturningList $ sqlUpdate ! #set setClause ! #where_ whereClause
      res <- runQuery dbConf updateQuery
      case res of
        Right x -> return $ Right x
        Left e -> return $ Left $ MDBError e

  where
    updateKVAndDBResults :: (L.MonadFlow m) => Either DBError [table Identity] -> MeshResult [table Identity] -> [(Text, A.Value)] -> m (MeshResult [table Identity])
    updateKVAndDBResults eitherDbRows eitherKvRows updVals = do
      case (eitherDbRows, eitherKvRows) of
        (Right allDBRows, Right allKVRows) -> do
          let kvRows = findAllMatching whereClause allKVRows
              dbRows = findAllMatching whereClause allDBRows
          let kvPkeys = map getLookupKeyByPKey kvRows
              uniqueDbRes = filter (\r -> getLookupKeyByPKey r `notElem` kvPkeys) dbRows
          updateKVRowRes <- mapM (updateObjectRedis meshCfg updVals whereClause) kvRows
          updateDBRowRes <- mapM (updateDBRowInRedis meshCfg updVals whereClause) uniqueDbRes
          pure $ concatMeshResult updateKVRowRes updateDBRowRes
        (_, Left err) -> pure $ Left err
        (Left err, _) -> pure $ Left (MDBError err)


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
findWithKVConnector :: forall be table beM m.
  ( HasCallStack,
    BeamRuntime be beM,
    BeamRunner beM,
    B.HasQBuilder be,
    Model be table,
    MeshMeta table,
    KVConnector (table Identity),
    FromJSON (table Identity),
    L.MonadFlow m
  ) =>
  DBConfig beM ->
  MeshConfig ->
  Where be table ->
  m (MeshResult (Maybe (table Identity)))
findWithKVConnector dbConf meshCfg whereClause = do --This function fetches all possible rows and apply where clause on it.
  if meshCfg.meshEnabled
    then do
      eitherKvRows <- findOneFromRedis meshCfg whereClause
      case eitherKvRows of
        Right [] -> do
          L.logDebug @Text "findWithKVConnector" "Falling back to SQL - Nothing found in KV"
          findOneFromDB dbConf whereClause
        Right rows -> pure $ Right $ findOneMatching whereClause rows
        Left err -> pure $ Left err
    else do
      L.logDebug @Text "findWithKVConnector" "Taking SQLDB Path"
      findOneFromDB dbConf whereClause

-- TODO: Once record matched in redis stop and return it
findOneFromRedis :: forall be table beM m. 
  ( HasCallStack,
    BeamRuntime be beM,
    BeamRunner beM,
    B.HasQBuilder be,
    Model be table,
    MeshMeta table,
    KVConnector (table Identity),
    FromJSON (table Identity),
    L.MonadFlow m
  ) =>
  MeshConfig -> Where be table -> m (MeshResult [table Identity])
findOneFromRedis meshCfg whereClause = do
  L.logDebug @Text "findWithKVConnector" "Taking KV Path"
  let keyAndValueCombinations = getFieldsAndValuesFromClause meshModelTableEntityDescriptor (And whereClause)
      andCombinations = map (uncurry zip . applyFPair (map (T.intercalate "_") . sortOn (Down . length) . nonEmptySubsequences) . unzip . sort) keyAndValueCombinations
      modelName = tableName @(table Identity)
      keyHashMap = keyMap @(table Identity)
  L.logDebugT "findWithKVConnector" (show keyAndValueCombinations)
  keyRes <- mapM (getPrimaryKeyFromFieldsAndValues modelName meshCfg keyHashMap) andCombinations
  allRowsRes <- mapM getDataFromPKeysRedis (rights keyRes) --don't ignore left
  L.logDebugT "findWithKVConnector" (show keyRes)
  pure $ getRowsFromKVResult allRowsRes []

  where
    getDataFromPKeysRedis [] = pure $ Right []
    getDataFromPKeysRedis (pKey : pKeys) = do
      res <- L.runKVDB meshCfg.kvRedis $ L.get (fromString $ T.unpack $ decodeUtf8 pKey)
      case res of
        Right (Just r) ->
          case A.eitherDecode $ BSL.fromChunks [r] of
            Right r' -> do
              case decodeField @(table Identity) r' of
                Right decodeRes -> do
                  remainingPKeysRes <- getDataFromPKeysRedis pKeys
                  pure $ mapRight (decodeRes ++) remainingPKeysRes
                Left e -> return $ Left e
            Left e   -> return $ Left $ MDecodingError $ T.pack e
        Right Nothing -> do
          let traceMsg = "redis_fetch_noexist: Could not find key: " <> show pKey
          L.logWarningT "getCacheWithHash" traceMsg
          getDataFromPKeysRedis pKeys
        Left e -> return $ Left $ MRedisError e

findOneFromDB :: forall be table beM m.
  ( HasCallStack,
    BeamRuntime be beM,
    BeamRunner beM,
    B.HasQBuilder be,
    Model be table,
    MeshMeta table,
    KVConnector (table Identity),
    FromJSON (table Identity),
    L.MonadFlow m
  ) =>
  DBConfig beM -> Where be table -> m (MeshResult (Maybe (table Identity)))
findOneFromDB dbConf whereClause = do
  let findQuery = DB.findRow (sqlSelect ! #where_ whereClause ! defaults)
  mapLeft MDBError <$> runQuery dbConf findQuery

concatMeshResult :: [MeshResult a] -> [MeshResult a] -> MeshResult [a]
concatMeshResult [] []              = Right []
concatMeshResult (Left err : _) _   = Left err
concatMeshResult _ (Left err : _)   = Left err
concatMeshResult (Right m1 : xs) ys = mapRight (m1 :) (concatMeshResult xs ys)
concatMeshResult xs (Right m2 : ys) = mapRight (m2 :) (concatMeshResult xs ys)


findAllWithKVConnector :: forall be table beM m.
  ( HasCallStack,
    BeamRuntime be beM,
    Model be table,
    MeshMeta table,
    KVConnector (table Identity),
    FromJSON (table Identity),
    L.MonadFlow m, B.HasQBuilder be, BeamRunner beM) =>
  DBConfig beM ->
  MeshConfig ->
  Where be table ->
  m (MeshResult [table Identity])
findAllWithKVConnector dbConf meshCfg whereClause = do
  let findAllQuery = DB.findRows (sqlSelect ! #where_ whereClause ! defaults)
  if meshCfg.meshEnabled
    then do
      L.logDebug @Text "findAllWithKVConnector" "Taking KV Path"
      kvRes <- redisFindAll meshCfg whereClause
      dbRes <- runQuery dbConf findAllQuery
      case dbRes of
        Left err -> L.logDebug @Text "findAllWithKVConnector" ("dbRes = " <> show err)
        Right _ -> L.logDebug @Text "findAllWithKVConnector" "Right"
      pure $ mapRight (mergeKVAndDBResults dbRes . findAllMatching whereClause) kvRes
    else do
      L.logDebug @Text "findAllWithKVConnector" "Taking SQLDB Path"
      mapLeft MDBError <$> runQuery dbConf findAllQuery

redisFindAll :: forall be table beM m.
  ( HasCallStack,
    BeamRuntime be beM,
    Model be table,
    MeshMeta table,
    KVConnector (table Identity),
    FromJSON (table Identity),
    L.MonadFlow m, B.HasQBuilder be, BeamRunner beM) =>
  MeshConfig ->
  Where be table ->
  m (MeshResult [table Identity])
redisFindAll meshCfg whereClause = do
  let keyAndValueCombinations = getFieldsAndValuesFromClause meshModelTableEntityDescriptor (And whereClause)
      andCombinations = map (uncurry zip . applyFPair (map (T.intercalate "_") . sortOn (Down . length) . nonEmptySubsequences) . unzip . sort) keyAndValueCombinations
      modelName = tableName @(table Identity)
      keyHashMap = keyMap @(table Identity)
  L.logDebugT "redisFindAll" (show keyAndValueCombinations)
  keyRes <- mapM (getPrimaryKeyFromFieldsAndValues modelName meshCfg keyHashMap) andCombinations
  allRowsRes <- mapM getDataFromPKeysRedis (rights keyRes) --don't ignore left
  L.logDebugT "redisFindAll" (show keyRes)
  pure $ getRowsFromKVResult allRowsRes []

  where
    getDataFromPKeysRedis [] = pure $ Right []
    getDataFromPKeysRedis (pKey : pKeys) = do
      res <- L.runKVDB meshCfg.kvRedis $ L.get (fromString $ T.unpack $ decodeUtf8 pKey)
      case res of
        Right (Just r) ->
          case A.eitherDecode $ BSL.fromChunks [r] of
            Right r' -> do
              case decodeField @(table Identity) r' of
                Right decodeRes -> do
                  remainingPKeysRes <- getDataFromPKeysRedis pKeys
                  pure $ mapRight (decodeRes ++) remainingPKeysRes
                Left e -> return $ Left e
            Left e   -> return $ Left $ MDecodingError $ T.pack e
        Right Nothing -> do
          let traceMsg = "redis_fetch_noexist: Could not find key: " <> show pKey
          L.logWarningT "getCacheWithHash" traceMsg
          getDataFromPKeysRedis pKeys
        Left e -> return $ Left $ MRedisError e

mergeKVAndDBResults :: (KVConnector (table Identity)) => Either DBError [table Identity] -> [table Identity] -> [table Identity]
mergeKVAndDBResults eitherDbRows kvRows = do
  case eitherDbRows of
    Right dbRows -> do
      let kvPkeys = map getLookupKeyByPKey kvRows
          uniqueDbRes = filter (\r -> getLookupKeyByPKey r `notElem` kvPkeys) dbRows
      kvRows ++ uniqueDbRes
    Left _ -> kvRows

getRowsFromKVResult :: [Either a [b]] -> [b] -> Either a [b]
getRowsFromKVResult [] res = Right res
getRowsFromKVResult (x : xs) res = case x of
  Right rows -> getRowsFromKVResult xs (rows ++ res)
  Left e -> Left e

--
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

-- getFieldsAndValuesFromClause dt (And [Is DBS.id (Eq (Just 1)), Or [Is DBS.merchantId (Eq (Just "123")), Is DBS.orderId (Eq (Just "oid"))]])
-- [[("id","Just 1"),("merchantId","Just \"123\"")],[("id","Just 1"),("orderId","Just \"oid\"")]]
getFieldsAndValuesFromClause :: forall table be. (Model be table, MeshMeta table) =>
  B.DatabaseEntityDescriptor be (B.TableEntity table) -> Clause be table -> [[(Text, Text)]]
getFieldsAndValuesFromClause dt = \case
  And cs -> foldl' processAnd [[]] $ map (getFieldsAndValuesFromClause dt) cs
  Or cs -> processOr cs
  Is column (Eq val) -> do
    let key = B._fieldName . fromColumnar' . column . columnize $ B.dbTableSettings dt
    [[(key, show val)]]
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
    Just True -> pure $ Right [fromString $ T.unpack contructKey]
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

---------------- UTILS -----------------------

getShardedHashTag :: Text -> Text
getShardedHashTag key = do
  let slot = unsafeCoerce @_ @Word16 $ L.keyToSlot $ encodeUtf8 key
      streamShard = slot `mod` 128
  "{shard-" <> show streamShard <> "}"

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