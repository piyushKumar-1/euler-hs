{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes        #-}
module EulerHS.KVConnector.InMemConfig.Flow

    where

import           EulerHS.Prelude 
import qualified Data.Text as T
import qualified EulerHS.Language as L
import           EulerHS.KVConnector.InMemConfig.Types
import           EulerHS.KVConnector.Types (MeshConfig)
import           EulerHS.Runtime (ConfigEntry(..))
import           EulerHS.KVConnector.Utils

checkAndStartLooper :: (L.MonadFlow m) => MeshConfig -> m ()
checkAndStartLooper meshCfg = do
    hasLooperStarted <- L.getOption LooperStarted
    case hasLooperStarted of
        _hasLooperStarted
            | _hasLooperStarted == Just True ->  pure ()
            | otherwise ->  do
                streamName <- getRandomStream meshCfg
                L.logDebug @Text "checkAndStartLooper" $ "Connecting with Stream <" <> streamName <> ">"
                L.fork $ looperForRedisStream meshCfg.kvRedis streamName
                L.setOption LooperStarted True

looperForRedisStream :: (L.MonadFlow m) => Text -> Text -> m ()
looperForRedisStream redisName streamName = forever $ do
    maybeRId <- L.getOption RecordId
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
                    mapM_ setInMemCache rs
        Just rId -> do
            newRecords <- getRecordsFromStream redisName streamName rId
            case newRecords of
                Nothing -> return ()
                Just (latestId, rs) -> do
                    L.setOption RecordId latestId
                    mapM_ setInMemCache rs
    void $ looperDelayInSec


looperDelayInSec :: (L.MonadFlow m) => m ()
looperDelayInSec = L.runIO $ threadDelay $ getConfigStreamLooperDelayInSec * 1000000

setInMemCache :: RecordKeyValues -> (L.MonadFlow m) => m ()
setInMemCache (key,value) = do
    newTtl <- getConfigEntryNewTtl
    void $ L.setConfig key $ ConfigEntry newTtl value
    return ()

extractRecordsFromStreamResponse :: [L.KVDBStreamReadResponseRecord] -> [RecordKeyValues]
extractRecordsFromStreamResponse  = foldMap (fmap (bimap decodeUtf8 decodeUtf8) . L.records) 

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

