module Euler.Playback.Service
  ( initPlayerParams
  , initRecorderParams
  , runSinglePlayerMode
  , runBulkPlayerMode
  , writeMethodRecordingDescription
  ) where

import EulerHS.Prelude

import           Euler.Playback.MethodPlayer
import           Euler.Playback.Types
import           EulerHS.Types

import           Data.Time
import           System.Directory
import           System.Environment

import qualified Control.Exception        as E   (IOException)
import qualified Data.Aeson               as A   (eitherDecodeStrict)
import qualified Data.Aeson.Encode.Pretty as A   (encodePretty)
import qualified Data.ByteString          as BS  (readFile, writeFile)
import qualified Data.ByteString.Lazy     as BSL (toStrict)


tryReadParams :: Read a => a -> String -> IO a
tryReadParams def name = do
  mEnvVar <- lookupEnv name
  pure $ maybe def id $ readMaybe =<< mEnvVar



initPlayerParams :: IO PlayerParams
initPlayerParams = do
  ppDisableVerify       <- tryReadParams [] "PLAYER_DISABLE_VERIFY"
  ppDisableMocking      <- tryReadParams [] "PLAYER_DISABLE_MOCKING"
  ppSkipEntries         <- tryReadParams [] "PLAYER_SKIP_ENTRIES"
 -- forceRealDBConns'   <- tryReadParams [] "PLAYER_FORCE_REAL_DB_CONNS_USAGE"
 -- forceRealKVDBConns' <- tryReadParams [] "PLAYER_FORCE_REAL_KVDB_CONNS_USAGE"
  ppResponseCheckMode   <- tryReadParams VerifyResponse "PLAYER_RESPONSE_CHECK_MODE"

  ppForcedRealDBConns   <- pure mempty -- StrMap.fromFoldable <$> traverse initForcedConn forceRealDBConns'
  ppForcedRealKVDBConns <- pure mempty -- StrMap.fromFoldable <$> traverse initForcedConn forceRealKVDBConns'

 -- makeDBModels          forcedRealDBConns
 -- makeEcReportingDBConn forcedRealDBConns
 -- makeEulerDBModels     forcedRealDBConns
 -- makeECRDBModels       forcedRealDBConns

  pure $ PlayerParams
    { ppDisableVerify
    , ppDisableMocking
    , ppSkipEntries
    , ppResponseCheckMode
    , ppForcedRealDBConns
    , ppForcedRealKVDBConns
    }

initRecorderParams :: IO (Maybe RecorderParams)
initRecorderParams = do
  mbRecordingsDir <- do
        enabled <- lookupEnv "RECORDER_ENABLED"
        dir <- lookupEnv "RECORDER_RECORDINGS_DIR"
        if enabled == (Just "TRUE") then pure dir else pure Nothing

  case mbRecordingsDir of
    Nothing -> pure Nothing
    Just recordingsDir -> do
      disableEntries <- tryReadParams [] "RECORDER_DISABLED_ENTRIES"
      putStrLn @String "Recorder mode."
      putStrLn @String $ "Recordings dir: " <> recordingsDir
      putStrLn @String $ "Disabled entries: " <> show disableEntries
 -- check that dir exists, if not - create     void $ attempt $ liftEff $ NFSync.mkdir recordingsDir
      void $ (either (print @SomeException) pure) =<< (try $ createDirectoryIfMissing True recordingsDir)
      pure $ Just $ RecorderParams { disableEntries
                                   , recordingsStorage = FSStorage recordingsDir
                                   }


showSinglePlayerResult :: MethodPlayerResult -> [String]
showSinglePlayerResult eResult =
  case eResult of
    Left (RequestDecodingError    msg)   -> ["[FAIL] Failed to decode request: ", msg]
    Left (RecordingsDecodingError msg)   -> ["[FAIL] Failed to decode recordings: ", msg]
    Left (RecordingsReadError     msg)   -> ["[FAIL] Failed to read recordings source: ", msg]
    Left (MethodNotSupported      msg)   -> ["[FAIL] Method not supported: ", msg]
    Left (ExceptionInPlayer       msg)   -> ["[FAIL] Exception in player: ", msg]
    Left (ForkedFlowsFailed       m)     -> (:) "Forked flows failed: " $ map (\pbErr -> "Flow: " <> (toString $ errorFlowGUID pbErr) <> (show $ errorType pbErr) <> errorMessage pbErr) m
    Right (PlaybackFailed pbErr)         -> ["[FAIL] Playback failed: ", "Flow: " <> (toString $ errorFlowGUID pbErr), show $ errorType pbErr, errorMessage pbErr]
    Right (PlaybackSucceeded respCheck)  ->
      case respCheck of
        ResponseOk                -> ["[SUCCESS] Successful replaying."]
        ResponseSkipped           -> ["[SUCCESS] Successful replaying, response check skipped."]
        ResponseMismatch r1 r2    -> ["[WARNING] Failed response check, response mismatch: ", r1, r2]
        ResponseDecodingError msg -> ["[WARNING] Failed response check, decode failed: ", msg]

runSinglePlayer' :: PlayerParams -> MethodRecordingSource -> IO ()
runSinglePlayer' pp src = do
  eResult <- runSinglePlayer pp src
  void $ traverse putStringLn $ showSinglePlayerResult eResult
  exitSuccess

runSinglePlayer :: PlayerParams -> MethodRecordingSource -> IO MethodPlayerResult
runSinglePlayer pp (FileSource fileName) = do
  recordingStr <- try $ BS.readFile fileName
  case recordingStr of
    Left err -> pure $ Left $ RecordingsReadError $ "Can't read file: " <> fileName <> " because of " <> (displayException @E.IOException err)
    Right recStr -> runPlayer'' pp recStr fileName

runPlayer'' :: PlayerParams -> ByteString -> String -> IO MethodPlayerResult
runPlayer'' pp recordingsStr source = case A.eitherDecodeStrict recordingsStr of
    Left err -> pure $ Left $ RecordingsDecodingError
      $ "Failed to decode method recording description from: " <> source <> "\nError: \n" <> show err
    Right mrd -> do
      eMPR <- runMethodPlayer' pp mrd
      case eMPR of
        Left err  -> pure $ Left $ ExceptionInPlayer $ show err
        Right mpr -> pure $ Right mpr

runMethodPlayer' ::PlayerParams
  -> MethodRecordingDescription
  -> IO MethodPlayerResult
runMethodPlayer' pp MethodRecordingDescription{..} =
  runMethodPlayer methodName methodRecording pp

runBulkPlayer' :: PlayerParams -> String -> IO ()
runBulkPlayer' pp dir = do
  eResult <- runBulkPlayer pp dir
  case eResult of
    Left (DirectoryReadError msg) -> putStringLn $ "Player failed to read directory: " <> msg
    Right results                 -> do
      putStringLn "Player finished replaying recordings. Results:"
      void $ traverse (\(n, pr) -> do
        putStringLn $ "Method: " <> n
        traverse putStringLn $ showSinglePlayerResult pr
        ) results
      exitSuccess

runBulkPlayer :: PlayerParams -> String -> IO PlayerResult
runBulkPlayer pp dirName = do
  eDirContents <- try $ listDirectory dirName
  case eDirContents of
    Left (err :: SomeException) -> pure $ Left $ DirectoryReadError $ show err
    Right dirContents -> do
      prs <- sequence $ map (runSinglePlayer pp . FileSource . (\n -> dirName <> "/" <> n)) dirContents
      pure $ Right $ zip dirContents prs

putStringLn :: String -> IO ()
putStringLn = putStrLn @String

runSinglePlayerMode :: IO ()
runSinglePlayerMode = do
  putStringLn "Single player mode."
  playerRecFile <- lookupEnv "PLAYER_RECORDING_FILE"
  case playerRecFile of
    Nothing   -> putStringLn "No recording file passed. Consider to setup PLAYER_RECORDING_FILE env variable."
    Just file -> do
      pp@PlayerParams{..} <- initPlayerParams
      putStringLn $ "Disabled verify: "       <> show ppDisableVerify
      putStringLn $ "Disabled mocking: "      <> show ppDisableMocking
      putStringLn $ "Skipping entries: "      <> show ppSkipEntries
      putStringLn $ "Force real DB conns: "   <> (show $ keys ppForcedRealDBConns)
      putStringLn $ "Force real KVDB conns: " <> (show $ keys ppForcedRealKVDBConns)
      putStringLn $ "Response check mode: "   <> show ppResponseCheckMode
      putStringLn $ "Recording file: "        <> file
      runSinglePlayer' pp  (FileSource file)

runBulkPlayerMode :: IO ()
runBulkPlayerMode = do
  putStringLn "Bulk player mode."
  recsDir <- lookupEnv "PLAYER_RECORDINGS_DIR"
  case recsDir  of
    Nothing   -> putStringLn "No recording dir passed. Consider to setup PLAYER_RECORDINGS_DIR env variable."
    Just dir -> do
      pp@PlayerParams{..} <- initPlayerParams
      putStringLn $ "Disabled verify: " <> show ppDisableVerify
      putStringLn $ "Disabled mocking: " <> show ppDisableMocking
      putStringLn $ "Skipping entries: " <> show ppSkipEntries
      putStringLn $ "Force real DB conns: "   <> (show $ keys ppForcedRealDBConns)
      putStringLn $ "Force real KVDB conns: " <> (show $ keys ppForcedRealKVDBConns)
      putStringLn $ "Response check mode: " <> show ppResponseCheckMode
      putStringLn $ "Recordings directory: " <> dir
      runBulkPlayer' pp dir

writeMethodRecordingDescription
  :: RecordingsStorage
  -> MethodRecordingDescription
  -> String
  -> String
  -> IO ()
writeMethodRecordingDescription (FSStorage dirPath) mrd methodName sessionid = do
  currentTime <- (formatTime defaultTimeLocale "%Y-%m-%d_%H-%M-%S")  <$> getCurrentTime
  -- FIXME: fileName is fragile!
  let fileName = dirPath
        <> "/"
        <> currentTime
        <> "_"
        <> methodName
        <> "_"
        <> sessionid <> ".txt"
  let prettyMrd = A.encodePretty mrd
  res <- try $ BS.writeFile fileName (BSL.toStrict prettyMrd)
  case res of
    Right _ -> pure ()
    Left (err :: SomeException) -> putStringLn $ show err
writeMethodRecordingDescription _ _ _ _ = error $ "writeMethodRecordingDescription not implemented."
