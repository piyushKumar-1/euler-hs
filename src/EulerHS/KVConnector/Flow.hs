
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- {-# OPTIONS_GHC -Wno-redundant-constraints #-}

module EulerHS.KVConnector.Flow
  (
    createWithKVConnector,
    findWithKVConnector,
    updateWithKVConnector,
    findAllWithKVConnector
  )
 where

import           EulerHS.Prelude
import           EulerHS.KVConnector.Types (KVConnector(..), MeshConfig, MeshResult, MeshError(..), MeshMeta(..), SecondaryKey(..), tableName, keyMap, getLookupKeyByPKey, getSecondaryLookupKeys, applyFPair)
import           EulerHS.KVConnector.DBSync (getCreateQuery, getUpdateQuery, getDbUpdateCommandJson, meshModelTableEntityDescriptor, DBCommandVersion(..))
import qualified Data.Aeson as A
import           Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy as BSL
import           Data.List (span, delete)
import qualified Data.Text as T
import qualified EulerHS.Language as L
import           EulerHS.Extra.Language (getOrInitSqlConn)
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as Map
import           EulerHS.SqlDB.Types (BeamRunner, BeamRuntime, DBConfig, DBError)
import qualified EulerHS.SqlDB.Language as DB
import           Sequelize (fromColumnar', modelTableName, columnize, sqlSelect, modelTableEntity, mkExprWithDefault, sqlUpdate, Model, Where, Clause(..), Term(..), Set(..))
import qualified Database.Beam as B
import qualified Database.Beam.Schema.Tables as B
import           Data.Either.Extra (mapRight, mapLeft)
import           Named (defaults, (!))
import           Text.Casing (quietSnake)
import           Unsafe.Coerce (unsafeCoerce)



createWithKVConnector ::
  forall (table :: (Type -> Type) -> Type) be m beM.
  ( HasCallStack,
    Model be table,
    BeamRuntime be beM,
    BeamRunner beM,
    B.HasQBuilder be,
    FromJSON (table Identity),
    ToJSON (table Identity),
    Show (table Identity),
    KVConnector (table Identity),
    L.MonadFlow m
  ) =>
  DBConfig beM ->
  MeshConfig ->
  table Identity ->
  Maybe Text ->
  m (MeshResult (table Identity))
createWithKVConnector dbConf meshCfg value _ = do
  case meshCfg.meshEnabled of
    True -> do
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
          _ <- L.runKVDB meshCfg.kvRedis $ L.setex pKey meshCfg.redisTtl (BSL.toStrict $ A.encode val)
          mapM_ (\secIdx -> do
            let sKey = fromString . T.unpack $ secIdx
            L.runKVDB meshCfg.kvRedis $ L.multiExec $ do
              _ <- L.lpushTx sKey [pKey]
              L.expireTx sKey meshCfg.redisTtl
            ) $ getSecondaryLookupKeys val
          _ <- L.runKVDB meshCfg.kvRedis $ L.xadd
                (encodeUtf8 (meshCfg.ecRedisDBStream <> getShardedHashTag pKeyText))
                L.AutoID
                [("command", BSL.toStrict $ A.encode qCmd)]
          return $ Right val
        Left err -> return $ Left err
    False -> do
      L.logDebug @Text "createWithKVConnector" "Taking SQLDB Path"
      res <- runQuery dbConf $ DB.insertRowsReturningList $ sqlCreate value
      case res of
        Right [val] -> return $ Right val
        Right xs -> do
          let message = "DB returned \"" <> show xs <> "\" after inserting \"" <> show value <> "\""
          L.logError @Text "create" message
          return $ Left $ UnexpectedError message
        Left e -> return $ Left $ MDBError e
  where
    sqlCreate :: forall _be _table.
      (B.HasQBuilder _be, Model _be _table) =>
      _table Identity ->
      B.SqlInsert _be _table
    sqlCreate _value = B.insert modelTableEntity (mkExprWithDefault _value)

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
  case meshCfg.meshEnabled of
    True -> do
      L.logDebug @Text "updateWithKVConnector" "Taking KV Path"
      findRes <- findWithKVConnector dbConf meshCfg whereClause
      L.logDebugT "updateWithKVConnector" ("findWithKVConnectorRes = " <> show findRes)
      let updVals = jsonKeyValueUpdates setClause
      case findRes of
        Right (Just obj) -> do
          let olderSkeys = map (\(SKey s) -> s) (secondaryKeys obj)
          _ <- modifySKeysRedis updVals olderSkeys obj
          updateObjectRedis obj updVals
        Right Nothing -> do
          L.logDebugT "updateWithKVConnector" "Found nothing from findWithKVConnectorRes - Falling back to DB and recaching"
          let findQuery = DB.findRow (sqlSelect ! #where_ whereClause ! defaults)
          dbRes <- runQuery dbConf findQuery
          case dbRes of
            Right (Just obj) -> do
              let pKey = fromString . T.unpack $ getLookupKeyByPKey obj
              mapM_ (\secIdx -> do
                let sKey = fromString . T.unpack $ secIdx
                L.runKVDB meshCfg.kvRedis $ L.multiExec $ do
                  _ <- L.lpushTx sKey [pKey]
                  L.expireTx sKey meshCfg.redisTtl
                ) $ getSecondaryLookupKeys obj
              updateObjectRedis obj updVals
            Right Nothing -> pure $ Left (MUpdateFailed ("No value found for table " <> tableName @(table Identity)))
            Left err -> pure $ Left (MDBError err)
        Left err -> pure $ Left err
    False -> do
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

  where
    updateObjectRedis :: table Identity ->[(Text, A.Value)] -> m (MeshResult (table Identity))
    updateObjectRedis obj updVals = do
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
          kvdbRes <- L.runKVDB meshCfg.kvRedis $ L.setex pKey meshCfg.redisTtl (BSL.toStrict $ A.encode obj)
          L.logDebug @Text "RedisUpdAnswer" . T.pack . show $ kvdbRes
          pure $ mapLeft MDecodingError (resultToEither $ A.fromJSON updatedModel)

    modifySKeysRedis :: [(Text, A.Value)] -> [[(Text, Text)]] -> table Identity -> m (MeshResult (table Identity))
    modifySKeysRedis updVals olderSkeys table = do
      let pKeyText = getLookupKeyByPKey table
          pKey = fromString . T.unpack $ pKeyText
      let tName = tableName @(table Identity)
          updValsMap = HM.fromList (map (\p -> (fst p, True)) updVals)
          (modifiedSkeysValues, unModifiedSkeysValues) = applyFPair (map getSortedKeyAndValue) $
                                                span (`isKeyModified` updValsMap) olderSkeys
          newSkeysValues = map (\(SKey s) -> getSortedKeyAndValue s) (secondaryKeys table)
      let unModifiedSkeys = map (\x -> tName <> "_" <> fst x <> "_" <> snd x) unModifiedSkeysValues
      mapM_ ((\key -> L.runKVDB meshCfg.kvRedis $ L.expire key meshCfg.redisTtl) . (fromString . T.unpack)) unModifiedSkeys
      mapM_ (addNewSkey pKey tName) (filter (\x -> fst x `elem` map fst modifiedSkeysValues) newSkeysValues)
      mapM_ (deleteOldSKeys pKey . (\x -> fromString . T.unpack $ tName <> "_" <> fst x <> "_" <> snd x)) modifiedSkeysValues
      pure $ Right table

    isKeyModified :: [(Text, Text)] -> HM.HashMap Text Bool -> Bool
    isKeyModified sKey updValsMap = foldl' (\r k -> HM.member (fst k) updValsMap || r) False sKey

    addNewSkey :: ByteString -> Text -> (Text, Text) -> m (MeshResult ())
    addNewSkey pKey tName (k, v) = do
      let sKey = fromString . T.unpack $ tName <> "_" <> k <> "_" <> v
      _ <- L.runKVDB meshCfg.kvRedis $ L.multiExec $ do
          _ <- L.lpushTx sKey [pKey]
          L.expireTx sKey meshCfg.redisTtl
      pure $ Right ()

    deleteOldSKeys :: ByteString -> ByteString -> m (MeshResult ())
    deleteOldSKeys pKey sKey = do
      res <- L.runKVDB meshCfg.kvRedis $ L.lrange sKey 0 (-1)
      case res of
        Right pKeys -> do
          _ <- L.runKVDB meshCfg.kvRedis $ L.lpush sKey (delete pKey pKeys)
          pure $ Right ()
        Left err -> pure $ Left (MRedisError err)

    getSortedKeyAndValue :: [(Text,Text)] -> (Text, Text)
    getSortedKeyAndValue kvTup = do
      let sortArr = sortBy (compare `on` fst) kvTup
      let (appendedKeys, appendedValues) = applyFPair (T.intercalate "_") $ unzip sortArr
      (appendedKeys, appendedValues)


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
  case meshCfg.meshEnabled of
    True -> do
      L.logDebug @Text "findWithKVConnector" "Taking KV Path"
      let keyAndValueCombinations = map (applyFPair (T.intercalate "_") . (unzip . sort)) (getFieldsAndValuesFromClause meshModelTableEntityDescriptor (And whereClause))
          modelName = tableName @(table Identity)
          keyHashMap = keyMap @(table Identity)
      L.logDebugT "findWithKVConnector" (show keyAndValueCombinations)
      keyRes <- mapM (getPrimaryKeyFromFieldsAndValues modelName meshCfg keyHashMap) keyAndValueCombinations
      allRowsRes <- mapM getDataFromPKeysRedis (rights keyRes) --don't ignore left
      L.logDebugT "findWithKVConnector" (show keyRes)
      pure $ mapRight (findOneMatching whereClause) (getRowsFromKVResult allRowsRes [])
    False -> do
      L.logDebug @Text "findWithKVConnector" "Taking SQLDB Path"
      mapLeft MDBError <$> runQuery dbConf findQuery
  where
    findQuery = DB.findRow (sqlSelect ! #where_ whereClause ! defaults)

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

    getRowsFromKVResult [] res = Right res
    getRowsFromKVResult (x : xs) res = case x of
      Right rows -> getRowsFromKVResult xs (rows ++ res)
      Left e -> Left e

-- concatMeshResult :: MeshResult [a] -> MeshResult [a] -> MeshResult [a]
-- concatMeshResult (Right m1) (Right m2) = Right (m1 ++ m2)
-- concatMeshResult (Left _) _ = Left $ MKeyNotFound "No key"
-- concatMeshResult _ (Left _) = Left $ MKeyNotFound "No key"

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
  case meshCfg.meshEnabled of
    True -> do
      L.logDebug @Text "findAllWithKVConnector" "Taking KV Path"
      let keyAndValueCombinations = map (applyFPair (T.intercalate "_") . (unzip . sort)) (getFieldsAndValuesFromClause meshModelTableEntityDescriptor (And whereClause))
          modelName = tableName @(table Identity)
          keyHashMap = keyMap @(table Identity)
      L.logDebugT "findAllWithKVConnector" (show keyAndValueCombinations)
      keyRes <- mapM (getPrimaryKeyFromFieldsAndValues modelName meshCfg keyHashMap) keyAndValueCombinations
      allRowsRes <- mapM getDataFromPKeysRedis (rights keyRes) --don't ignore left
      L.logDebugT "findAllWithKVConnector" (show keyRes)
      let findAllQuery = DB.findRows (sqlSelect ! #where_ whereClause ! defaults)
      _ <- runQuery dbConf findAllQuery
      pure $ mapRight (findAllMatching whereClause) (getRowsFromKVResult allRowsRes [])
    False -> do
      L.logDebug @Text "findAllWithKVConnector" "Taking SQLDB Path"
      let findAllQuery = DB.findRows (sqlSelect ! #where_ whereClause ! defaults)
      mapLeft MDBError <$> runQuery dbConf findAllQuery

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
    processAnd xs ys = [x ++ y | x <-xs, y <- ys]
    processOr xs = concatMap (getFieldsAndValuesFromClause dt) xs

getPrimaryKeyFromFieldsAndValues :: (L.MonadFlow m) => Text -> MeshConfig -> HM.HashMap Text Bool -> (Text, Text) -> m (MeshResult [ByteString])
getPrimaryKeyFromFieldsAndValues modelName meshCfg keyHashMap (k, v) =
  case HM.lookup k keyHashMap of
    Just True -> pure $ Right [fromString $ T.unpack contructKey]
    Just False -> do
      let sKey = contructKey
      res <- L.runKVDB meshCfg.kvRedis $ L.lrange (fromString $ T.unpack sKey) 0 (-1)
      case res of
        Right r -> pure $ Right r
        Left e -> return $ Left $ MRedisError e
    _ -> pure $ Right []
  where
    contructKey = modelName <> "_" <> k <> "_" <> v

-- >>> map (T.intercalate "_") (nonEmptySubsequences ["id", "id2", "id3"])
-- ["id","id2","id_id2","id3","id_id3","id2_id3","id_id2_id3"]
-- nonEmptySubsequences         :: [Text] -> [[Text]]
-- nonEmptySubsequences []      =  []
-- nonEmptySubsequences (x:xs)  =  [x]: foldr f [] (nonEmptySubsequences xs)
--   where f ys r = ys : (x : ys) : r

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