{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes        #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module EulerHS.KVConnector.InMemConfig.Flow

    where

import qualified Data.HashMap.Strict as HM
import           EulerHS.Prelude 
import           EulerHS.SqlDB.Types (BeamRunner, BeamRuntime, DBConfig)
import qualified EulerHS.SqlDB.Language as DB
import qualified Data.Aeson as A
import qualified Database.Beam as B
-- import           Control.Monad.Extra (notM)
import qualified Data.Set as Set
import qualified Data.List as DL
import qualified Data.ByteString.Lazy as BSL
import           Data.Time (LocalTime)
import qualified Data.Text as T
import qualified EulerHS.Language as L
import           EulerHS.KVConnector.InMemConfig.Types
import           EulerHS.KVConnector.Types (KVConnector(..), MeshConfig, tableName, MeshResult, MeshMeta(..), MeshError(MDBError))
import           Unsafe.Coerce (unsafeCoerce)
import           Data.Either.Extra (mapLeft, mapRight)
import           EulerHS.CachedSqlDBQuery (runQuery)
import           EulerHS.Runtime (mkConfigEntry)
import           EulerHS.KVConnector.Utils
import           Sequelize (Model, Where, Clause(..), sqlSelect)
import           Named (defaults, (!))
import qualified Data.Serialize as Serialize

checkAndStartLooper :: forall table m.
    (
    HasCallStack,
    KVConnector (table Identity),
    L.MonadFlow m) => MeshConfig -> (ByteString -> Maybe (table Identity)) -> m ()
checkAndStartLooper meshCfg decodeTable = do
    hasLooperStarted <- L.getOption $ (LooperStarted (tableName @(table Identity)))
    case hasLooperStarted of
        _hasLooperStarted
            | _hasLooperStarted == Just True ->  pure ()
            | otherwise ->  do
                streamName <- getRandomStream 
                L.logDebug @Text "checkAndStartLooper" $ "Connecting with Stream <" <> streamName <> ">"
                L.fork $ looperForRedisStream  decodeTable meshCfg.kvRedis streamName
                L.setOption (LooperStarted (tableName @(table Identity))) True

looperForRedisStream :: forall table m.(
    HasCallStack,
    KVConnector (table Identity),
    L.MonadFlow m
    ) => 
    (ByteString -> Maybe (table Identity)) -> Text -> Text -> m ()
looperForRedisStream decodeTable redisName streamName = forever $ do
    maybeRId <- L.getOption RecordId
    let tName = tableName @(table Identity)
    case maybeRId of
        Nothing -> do
            rId <- T.pack . show <$> L.getCurrentDateInMillis
            initRecords <- getRecordsFromStream redisName streamName rId
            case initRecords of 
                Nothing -> do
                    L.setOption RecordId rId
                    return ()
                Just (latestId, rs) -> do
                    L.setOption RecordId latestId
                    mapM_ (setInMemCache tName decodeTable) rs
        Just rId -> do
            newRecords <- getRecordsFromStream redisName streamName rId
            case newRecords of
                Nothing -> return ()
                Just (latestId, rs) -> do
                    L.setOption RecordId latestId
                    mapM_ (setInMemCache tName decodeTable) rs
    void $ looperDelayInSec


looperDelayInSec :: (L.MonadFlow m) => m ()
looperDelayInSec = L.runIO $ threadDelay $ getConfigStreamLooperDelayInSec * 1000000

setInMemCache :: forall table m.(
    HasCallStack,
    KVConnector(table Identity),
    L.MonadFlow m
    ) => 
    Text ->
    (ByteString -> Maybe (table Identity)) ->
    RecordKeyValues -> m ()
setInMemCache tName decodeTable (key,value) = do
  when (tName == key) $                             -- decode only when entry is for the looper's table
    case decodeTable value of
        Nothing -> return ()
        Just x -> do
            let
              pKeyText = getLookupKeyByPKey x
              pKey = pKeyText  <> getShardedHashTag pKeyText
            updateAllKeysInIMC pKey x

extractRecordsFromStreamResponse :: [L.KVDBStreamReadResponseRecord] -> [RecordKeyValues]
extractRecordsFromStreamResponse  = foldMap (fmap (bimap decodeUtf8 id) . L.records) 

getRecordsFromStream :: Text -> Text -> LatestRecordId -> (L.MonadFlow m) => m (Maybe (LatestRecordId, [RecordKeyValues]))
getRecordsFromStream redisName streamName lastRecordId = do
    eitherReadResponse <- L.rXreadT redisName streamName lastRecordId
    case eitherReadResponse of
        Left err -> do
            L.delOption RecordId    -- TODO Necessary?
            L.logErrorT "getRecordsFromStream" $ "Error getting initial records from stream <" <> streamName <> ">" <> show err
            return Nothing
        Right maybeRs -> case maybeRs of
            Nothing -> do
            -- L.delOption RecordId    -- Maybe stream doesn't exist or Maybe no new records
                logEmptyResponseAndReturn
            Just [] -> do             -- Never seems to occur
                logEmptyResponseAndReturn
            Just rs -> case filter (\rec-> (decodeUtf8 rec.streamName) == streamName) rs of
                [] -> do
                    logEmptyResponseAndReturn
                (rss : _) -> do
                    case uncons . reverse . L.response $ rss of
                        Nothing -> logEmptyResponseAndReturn
                        Just (latestRecord, _) -> do
                                L.logInfoT "getRecordsFromStream" $ (show . length . L.response $ rss) <> " new records in stream <" <> streamName <> ">"
                                return . Just . bimap (decodeUtf8 . L.recordId) (extractRecordsFromStreamResponse . L.response ) $ (latestRecord, rss)
    where
        logEmptyResponseAndReturn :: (L.MonadFlow m) => m (Maybe (LatestRecordId, [RecordKeyValues]))
        logEmptyResponseAndReturn = do
            L.logDebugT "getRecordsFromStream" $ "No new records in stream <" <> streamName <> ">"
            return Nothing


getDataFromPKeysIMC :: forall table m. (
    KVConnector (table Identity),
    FromJSON (table Identity),
    Show (table Identity),
    L.MonadFlow m) => MeshConfig -> [ByteString] -> m (MeshResult [InMemCacheResult (table Identity)])
getDataFromPKeysIMC _ [] = pure $ Right []
getDataFromPKeysIMC meshCfg (pKey : pKeys) = do
  let k = decodeUtf8 pKey
  mbVal <- L.getConfig k
  record <- case mbVal of
    Nothing -> pure . EntryNotFound $ k
    Just val -> do
      currentTime <- L.getCurrentTimeUTC
      if val.ttl > currentTime
        then pure $ EntryValid (unsafeCoerce @_ @(table Identity) val.entry)
        else pure $ EntryExpired (unsafeCoerce @_ @(table Identity) val.entry) k
  mapRight (record :) <$> getDataFromPKeysIMC meshCfg pKeys

searchInMemoryCache :: forall be beM table m.
  (
    BeamRuntime be beM,
    BeamRunner beM,
    B.HasQBuilder be,
    HasCallStack,
    KVConnector (table Identity),
    Show (table Identity),
    Serialize.Serialize (table Identity),
    FromJSON (table Identity),
    Model be table,
    MeshMeta be table,
    L.MonadFlow m
  ) =>  MeshConfig -> 
        DBConfig beM ->
        Where be table ->
        m (MeshResult [table Identity])
searchInMemoryCache meshCfg dbConf whereClause = do
  eitherPKeys <- getPrimaryKeys 
  L.logDebugT "findWithKVConnector: " $ "eitherPKeys: " <> (tname <> ":" <> show eitherPKeys)
  let
    decodeTable :: ByteString -> Maybe (table Identity)
    decodeTable = A.decode . BSL.fromStrict
  checkAndStartLooper meshCfg decodeTable
  case eitherPKeys of
    Right pKeys -> do
        L.logDebugT "searchInMemoryCache: pKeys : " $ (show pKeys)
        allRowsRes <- mapM (getDataFromPKeysIMC meshCfg) pKeys
        case mapRight concat (foldEither allRowsRes) of
          (Left e) -> return . Left $ e
          Right results -> do
              let
                validResult = catMaybes $ results <&> getValidResult
                keysRequiringRedisFetch = catMaybes $ results <&> getKeysRequiringRedisFetch
              L.logDebugT "searchInMemoryCache: validResult : " $ (show validResult)
              L.logDebugT "searchInMemoryCache: keysRequiringRedisFetch : " $ (show keysRequiringRedisFetch)
              if length validResult == 0
                then kvFetch keysRequiringRedisFetch
                else if length keysRequiringRedisFetch == 0
                  then return . Right $ validResult
                  else do
                    let
                      lockKey :: Text
                      lockKey = T.pack . DL.intercalate "_" $ T.unpack <$> keysRequiringRedisFetch
                    whenM (L.acquireConfigLock lockKey) (forkKvFetchAndSave keysRequiringRedisFetch lockKey)
                    return . Right $ validResult
    
    Left e -> return . Left $ e
  where

    tname :: Text = (tableName @(table Identity))

    getValidResult :: InMemCacheResult (table Identity) -> Maybe (table Identity)
    getValidResult r = case r of
      EntryValid v -> Just $ v
      EntryExpired v _ -> Just $ v
      _ -> Nothing

    getKeysRequiringRedisFetch :: InMemCacheResult (table Identity) -> Maybe KeysRequiringRedisFetch
    getKeysRequiringRedisFetch r = case r of
      EntryExpired _ k -> Just $ k
      EntryNotFound k -> Just $ k
      _ -> Nothing

    forkKvFetchAndSave :: [Text] -> Text -> m ()
    forkKvFetchAndSave pKeys lockKey = do
      L.logDebugT "forkKvFetchAndSave" $ "Starting Timeout for redis-fetch for in-mem-config"
      L.fork $ 
        do
          void $ L.runIO $ threadDelayMilisec 5
          void $ L.releaseConfigLock lockKey      -- TODO  Check if release fails
      L.logDebugT "forkKvFetchAndSave" $ "Initiating updation of key <" <> (show pKeys) <>"> in-mem-config"
      L.fork $ void $ kvFetch pKeys

    kvFetch :: [Text] -> m (MeshResult [table Identity])
    kvFetch pKeys = 
      if meshCfg.kvHardKilled       
        then doDbFetchAndUpdateIMC
        else useKvcAndUpdateIMC pKeys

    useKvcAndUpdateIMC :: [Text] -> m (MeshResult [table Identity])
    useKvcAndUpdateIMC pKeys = do
      (eTuples :: MeshResult [Maybe (Text, table Identity)]) <- foldEither <$> mapM (getDataFromRedisForPKey meshCfg)  pKeys
      case eTuples of
        Left err -> do
          L.logErrorT "kvFetch: " (show err)
          return . Left $ err
        Right mtups -> do
          L.logDebugT "kvFetch" $ "mtups: " <> (show mtups)
          let tups = catMaybes mtups
          if length tups == 0
            then doDbFetchAndUpdateIMC
            else do
              mapM_ (uncurry updateAllKeysInIMC) tups
              return . Right $ snd <$> tups

    doDbFetchAndUpdateIMC :: m (MeshResult [(table Identity)])
    doDbFetchAndUpdateIMC = do
      eDbTups <- dbFetch
      L.logDebugT "kvFetch" $ "eDbTups: " <> (show eDbTups)
      case eDbTups of
        (Left e) -> pure .Left $ e
        Right dbTups -> do
          mapM_ (uncurry updateAllKeysInIMC) dbTups
          return . Right $ snd <$> dbTups

    dbFetch :: m (MeshResult [(Text, table Identity)])
    dbFetch = do
      let findAllQuery = DB.findRows (sqlSelect ! #where_ whereClause ! defaults)
      res <- mapLeft MDBError <$> runQuery dbConf findAllQuery
      case res of
        Left err -> pure $ Left err
        Right rows -> pure . Right $ (\row -> (getPKeyFromPKeyText row, row)) <$> rows

    getPKeyFromPKeyText :: table Identity -> Text
    getPKeyFromPKeyText row = 
      let
        pKeyText = getLookupKeyByPKey row
      in pKeyText  <> getShardedHashTag pKeyText
    getPrimaryKeys :: m (MeshResult [[ByteString]])
    getPrimaryKeys = do
      let 
        keyAndValueCombinations = getFieldsAndValuesFromClause meshModelTableEntityDescriptor (And whereClause)
        andCombinations = map (uncurry zip . applyFPair (map (T.intercalate "_") . sortOn (Down . length) . nonEmptySubsequences) . unzip . sort) keyAndValueCombinations
        modelName = tableName @(table Identity)
        keyHashMap = keyMap @(table Identity)
      L.logDebugT "findWithKVConnector: kvCombos" (show keyAndValueCombinations)
      eitherKeyRes <- mapM (getPrimaryKeyInIMCFromFieldsAndValues modelName keyHashMap) andCombinations
      L.logDebugT "findWithKVConnector: eitherKeyRes" (show eitherKeyRes)
      pure $ foldEither eitherKeyRes

    getPrimaryKeyInIMCFromFieldsAndValues :: (L.MonadFlow m) => Text -> HM.HashMap Text Bool -> [(Text, Text)] -> m (MeshResult [ByteString])
    getPrimaryKeyInIMCFromFieldsAndValues _ _ [] = pure $ Right []
    getPrimaryKeyInIMCFromFieldsAndValues modelName keyHashMap ((k, v) : xs) =
      case HM.lookup k keyHashMap of
        Just True -> pure $ Right [fromString $ T.unpack (contructKey <> getShardedHashTag contructKey)]
        Just False -> do
          let sKey = contructKey
          L.getConfig sKey >>= \case
            Nothing -> getPrimaryKeyInIMCFromFieldsAndValues modelName keyHashMap xs
            Just pKeyEntries -> pure . Right $ encodeUtf8 <$> unsafeCoerce @_ @[Text] pKeyEntries.entry
        _ -> getPrimaryKeyInIMCFromFieldsAndValues modelName keyHashMap xs
      where
        contructKey = modelName <> "_" <> k <> "_" <> v

updateAllKeysInIMC :: forall table m. (KVConnector (table Identity), L.MonadFlow m) => Text -> table Identity -> m ()
updateAllKeysInIMC pKey val = do
  newTtl <- getConfigEntryNewTtl
  L.setConfig pKey $ mkConfigEntry newTtl val
  let
    sKeys = getSecondaryLookupKeys val
  mapM_ (updateSecondaryKeyInIMC pKey newTtl) sKeys

updateSecondaryKeyInIMC :: L.MonadFlow m => Text -> LocalTime -> Text -> m ()
updateSecondaryKeyInIMC pKey ttl sKey = do
  pkeyList <- L.getConfig sKey >>= \case 
    Nothing -> return []
    Just ls -> return $ unsafeCoerce ls.entry
  L.setConfig sKey $ mkConfigEntry ttl (Set.toList . Set.fromList $ pKey:pkeyList)