{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}

module EulerHS.Tests.Framework.ArtSpec where

import           EulerHS.Prelude
import           Test.Hspec
import           EulerHS.Types               as T
import           EulerHS.Runtime
import           EulerHS.Language            as L
import           EulerHS.Interpreters
import qualified Data.Map                    as Map
import qualified Data.Vector                 as V
import Data.Aeson.Encode.Pretty
import           EulerHS.TestData.Types
import           Servant.Server
import           Servant.Client
import           EulerHS.TestData.API.Client
import           Network.Wai.Handler.Warp
import           Data.Aeson                  as A

import           Database.Redis (checkedConnect, defaultConnectInfo, TxResult(TxSuccess), Status(..))
-- import qualified EulerHS.Language as L

spec :: Spec
spec = do
  kvdbSpec
  describe "ART Test" $ do
    it "Regular mode" $ do
      rt <- initRegularRT
      res <- runFlow rt $ mainScript
      res `shouldBe` "hello\n"

    it "Recorder mode" $ do
      flowRuntime <- initRecorderRT
      result      <- runFlow flowRuntime mainScript
      case _runMode flowRuntime of
        T.RecordingMode T.RecorderRuntime{recording} -> do
          T.ResultRecording{..} <- awaitRecording recording
          V.length recording        `shouldBe` 9
          Map.size forkedRecordings `shouldBe` 2
          result                    `shouldBe` "hello\n"
        _ -> fail "wrong mode"

    it "Player mode: replaying incorrect flow returns error (main flow)" $ do
      flowRuntime <- initRecorderRT
      _           <- runFlow flowRuntime mainScript
      case _runMode flowRuntime of
        T.RecordingMode T.RecorderRuntime{recording} -> do
          entries              <- awaitRecording recording
          playerRuntime        <- initPlayerRT entries
          -- TODO runFlow shoul catch all exceptions internally
          _ <- try @_ @SomeException $ runFlow playerRuntime mainScriptWrong
          case _runMode playerRuntime of
            T.ReplayingMode T.PlayerRuntime{rerror} -> do
              errors <- awaitErrors rerror
              flattenErrors errors `shouldNotBe` []
            _ -> fail "wrong mode"
        _ -> fail "wrong mode"

    it "Player mode: replaying incorrect flow returns error (fork flow)" $ do
      flowRuntime <- initRecorderRT
      _           <- runFlow flowRuntime mainScript
      case _runMode flowRuntime of
        T.RecordingMode T.RecorderRuntime{recording} -> do
          entries              <- awaitRecording recording
          playerRuntime        <- initPlayerRT entries
          -- TODO runFlow shoul catch all exceptions internally
          _ <- try @_ @SomeException $ runFlow playerRuntime mainScriptWrongFork
          case _runMode playerRuntime of
            T.ReplayingMode T.PlayerRuntime{rerror} -> do
              errors <- awaitErrors rerror
              flattenErrors errors `shouldNotBe` []
            _ -> fail "wrong mode"
        _ -> fail "wrong mode"

    it "Player mode: missing fork recording returns error (fork flow)" $ do
      flowRuntime <- initRecorderRT
      _           <- runFlow flowRuntime mainScript
      case _runMode flowRuntime of
        T.RecordingMode T.RecorderRuntime{recording} -> do
          entries              <- awaitRecording recording
          playerRuntime        <- initPlayerRT $ entries {forkedRecordings = Map.empty}
          -- TODO runFlow shoul catch all exceptions internally
          _ <- try @_ @SomeException $ runFlow playerRuntime mainScript
          case _runMode playerRuntime of
            T.ReplayingMode T.PlayerRuntime{rerror} -> do
              errors <- awaitErrors rerror
              flattenErrors errors `shouldNotBe` []
            _ -> fail "wrong mode"
        _ -> fail "wrong mode"

----------------------------------------------------------------------

    it "Set/Get Option" $ do
      let testOptionKey   = TestStringKey
      let testOptionValue = "testOptionValue" :: String
      mopt <- runFlowWithArt $ do
        L.setOption testOptionKey testOptionValue
        L.getOption testOptionKey
      mopt `shouldBe` Just testOptionValue

    it "Generate distinct GUID" $ do
      (guid1, guid2) <- runFlowWithArt $ do
        guid1 <- L.generateGUID
        guid2 <- L.generateGUID
        pure (guid1, guid2)
      guid1 `shouldNotBe` guid2

    it "RunIO" $ do
      res <- runFlowWithArt $ do
        L.runIO $ pure ()
      res `shouldBe` ()

    it "RunIO" $ do
      res <- runFlowWithArt $ do
        L.runIO $ pure ()
      res `shouldBe` ()

    it "RunIO also works with Serializable types" $ do
      let bs :: ByteString = "Hello"
      res <- runFlowWithArt $ do
        L.runIO $ pure bs
      res `shouldBe` bs

    it "RunSysCmd" $ do
      let value = "hello"
      res <- runFlowWithArt $ do
        L.runSysCmd $ "echo " <> value
      res `shouldBe` "hello\n"

    it "Logging" $ runFlowWithArt $ do
      L.logInfo    @String "Info"    "L.logInfo"
      L.logError   @String "Error"   "L.logError"
      L.logDebug   @String "Debug"   "L.logDebug"
      L.logWarning @String "Warning" "L.logWarning"

    it "Fork" $ runFlowWithArt $ do
      L.forkFlow "Fork" $
        L.logInfo @String "Fork" "Hello"

    it "Fork from Fork" $ runFlowWithArt $ do
      L.forkFlow "ForkOne" $ do
        L.logInfo @String "ForkOne" "Hello"
        L.forkFlow "ForkTwo" $
          L.forkFlow "ForkThree" $ do
            L.forkFlow "ForkFour" $
              L.logInfo @String "ForkFour" "Hello"

    around_ withServer $ do
      describe "CallServantAPI tests" $ do
        it "Simple request (book)" $ do
          let url = BaseUrl Http "localhost" port ""
          bookEither <- runFlowWithArt $ callServantAPI url getBook
          bookEither `shouldSatisfy` isRight

        it "Simple request (user)" $ do
          let url = BaseUrl Http "localhost" port ""
          userEither <- runFlowWithArt $ callServantAPI url getUser
          userEither `shouldSatisfy` isRight



runFlowWithArt :: (Show b, Eq b) => Flow b -> IO b
runFlowWithArt flow = do
  (recording, recResult) <- runFlowRecording pure flow
  (errors   , repResult) <- runFlowReplaying pure recording flow
  flattenErrors errors `shouldBe` []
  recResult `shouldBe` repResult
  pure recResult


runFlowRecording :: (FlowRuntime -> IO FlowRuntime) -> Flow a -> IO (ResultRecording, a)
runFlowRecording mod flow = do
  flowRuntime <- mod =<< initRecorderRT
  result <- runFlow flowRuntime flow
  case _runMode flowRuntime of
    T.RecordingMode T.RecorderRuntime{recording} -> do
      entries <- awaitRecording recording
      pure (entries, result)
    _ -> fail "wrong mode"

runFlowReplaying :: (FlowRuntime -> IO FlowRuntime) -> ResultRecording -> Flow a -> IO (ResultReplayError, a)
runFlowReplaying mod recording flow  = do
  playerRuntime <- mod =<< initPlayerRT recording
  result <- runFlow playerRuntime flow
  case _runMode playerRuntime of
    T.ReplayingMode T.PlayerRuntime{rerror} -> do
      errors <- awaitErrors rerror
      pure (errors, result)
    _ -> fail "wrong mode"

withServer :: IO () -> IO ()
withServer action = do
  serverStartupLock <- newEmptyMVar

  let
    settings = setBeforeMainLoop (putMVar serverStartupLock ()) $
      setPort port defaultSettings

  threadId <- forkIO $ runSettings settings $ serve api server
  readMVar serverStartupLock
  action
  killThread threadId

initRegularRT :: IO FlowRuntime
initRegularRT = do
  flowRt <- withFlowRuntime Nothing pure
  pure $ flowRt { _runMode = T.RegularMode }

initRecorderRT :: IO FlowRuntime
initRecorderRT = do
  recMVar       <- newMVar V.empty
  forkedRecMVar <- newMVar Map.empty
  let
    recorderRuntime = T.RecorderRuntime
      { flowGUID = "testFlow"
      , recording = T.Recording recMVar forkedRecMVar
      , disableEntries = []
      }
  flowRuntime <- withFlowRuntime Nothing pure
  pure $ flowRuntime { _runMode = T.RecordingMode recorderRuntime }


initPlayerRT :: ResultRecording -> IO FlowRuntime
initPlayerRT recEntries = do
  step              <- newMVar 0
  freshReplayErrors <- T.ReplayErrors <$> newMVar Nothing <*> newMVar Map.empty

  let
    playerRuntime = T.PlayerRuntime
      { resRecording    = recEntries
      , stepMVar        = step
      , rerror          = freshReplayErrors
      , disableVerify   = []
      , disableMocking  = []
      , skipEntries     = []
      , entriesFiltered = False
      , flowGUID        = "MainFlow"
      }

  flowRuntime <- withFlowRuntime Nothing pure
  pure $ flowRuntime { _runMode = T.ReplayingMode playerRuntime }

mainScript :: Flow String
mainScript = do
  guid1 <- generateGUID
  guid2 <- generateGUID
  forkFlow guid1 forkScript
  forkFlow guid2 forkScript
  runSysCmd "echo hello"

mainScriptWrong :: Flow String
mainScriptWrong = do
  guid1 <- generateGUID
  forkFlow guid1 forkScript
  runSysCmd "echo hello"

mainScriptWrongFork :: Flow String
mainScriptWrongFork = do
  guid1 <- generateGUID
  guid2 <- generateGUID
  forkFlow guid1 forkScript
  forkFlow guid2 forkScriptWrong
  runSysCmd "echo hello"

forkScript :: Flow String
forkScript = do
  _ <- generateGUID
  runSysCmd "echo hello"

forkScriptWrong :: Flow String
forkScriptWrong = do
  runSysCmd "echo hello"


----------------------------------------------------------------------

replayRecording :: ResultRecording -> Flow a -> IO a
replayRecording rec flow = do
  (errors, result) <- runFlowReplaying pure rec flow
  flattenErrors errors `shouldBe` []
  pure result

-- prints replay in JSON format to console
runWithRedisConn :: (Show b, Eq b) => a -> Flow b -> IO b
runWithRedisConn _ flow = do
  (recording, recResult) <- runFlowRecording initRedis flow
  print $ encode $ recording
  pure recResult
  where
    initRedis = \rt -> do
      realRedisConnection <- Redis <$> checkedConnect defaultConnectInfo
      connections         <- takeMVar $ _kvdbConnections rt
      putMVar (_kvdbConnections rt) $
        Map.insert "redis" realRedisConnection $ connections
      pure rt


kvdbSpec :: Spec
kvdbSpec = do
  describe "RunKVDB tests with ART" $ do
    it "get a correct key" $ do
      result <- replayRecording getKey $ L.runKVDB $ do
        L.set "aaa" "bbb"
        res <- L.get "aaa"
        L.del ["aaa"]
        pure res
      result `shouldBe` Right (Just "bbb")

    it "get a wrong key" $ do
      result <- replayRecording getWrongKey $ L.runKVDB $ do
        L.set "aaa" "bbb"
        res <- L.get "aaac"
        L.del ["aaa"]
        pure res
      result `shouldBe` Right Nothing

    it "delete existing keys" $ do
      result <- replayRecording deleteExisting $ L.runKVDB $ do
        L.set "aaa" "bbb"
        L.set "ccc" "ddd"
        L.del ["aaa", "ccc"]
      result `shouldBe` Right 2

    it "delete keys (w/ no keys)" $ do
      result <- replayRecording deleteKeysNoKeys $ L.runKVDB $ do
        L.del []
      result `shouldBe` Right 0

    it "delete missing keys" $ do
      result <- replayRecording deleteMissing $ L.runKVDB $ do
        L.del ["zzz", "yyy"]
      result `shouldBe` Right 0

    it "get a correct key from transaction" $ do
      result <- replayRecording getCorrectFromTx $ L.runKVDB $ L.multiExec $ do
        L.setTx "aaa" "bbb"
        res <- L.getTx "aaa"
        L.delTx ["aaa"]
        pure res
      result `shouldBe` Right (T.TxSuccess (Just "bbb"))

    it "get incorrect key from transaction" $ do
      result <- replayRecording getIncorrectFromTx $ L.runKVDB $ L.multiExec $ do
        res <- L.getTx "aaababababa"
        pure res
      result `shouldBe` Right (T.TxSuccess Nothing)

    it "setex sets value" $ do
      let hour = 60 * 60
      result <- replayRecording setExGetKey $ L.runKVDB $ do
        L.setex "aaaex" hour "bbbex"
        res <- L.get "aaaex"
        L.del ["aaaex"]
        pure res
      result `shouldBe` Right (Just "bbbex")

    it "setex ttl works" $ do
      result <- replayRecording setExTtl $ do
        L.runKVDB $ L.setex "aaaex" 1 "bbbex"
        L.runIO $ threadDelay (2 * 10 ^ 6)
        L.runKVDB $ do
          res <- L.get "aaaex"
          L.del ["aaaex"]
          pure res
      result `shouldBe` Right Nothing



getKey :: ResultRecording
getKey = fromJust $ decode "{\"recording\":[{\"_entryName\":\"SetEntry\",\"_entryIndex\":0,\"_entryPayload\":{\"jsonValue\":[98,98,98],\"jsonResult\":{\"Right\":{\"tag\":\"Ok\"}},\"jsonKey\":[97,97,97]},\"_entryReplayMode\":\"Normal\"},{\"_entryName\":\"GetEntry\",\"_entryIndex\":1,\"_entryPayload\":{\"jsonResult\":{\"Right\":[98,98,98]},\"jsonKey\":[97,97,97]},\"_entryReplayMode\":\"Normal\"},{\"_entryName\":\"DelEntry\",\"_entryIndex\":2,\"_entryPayload\":{\"jsonResult\":{\"Right\":1},\"jsonKeys\":[[97,97,97]]},\"_entryReplayMode\":\"Normal\"}],\"forkedRecordings\":{}}"

getWrongKey :: ResultRecording
getWrongKey = fromJust $ decode "{\"recording\":[{\"_entryName\":\"SetEntry\",\"_entryIndex\":0,\"_entryPayload\":{\"jsonValue\":[98,98,98],\"jsonResult\":{\"Right\":{\"tag\":\"Ok\"}},\"jsonKey\":[97,97,97]},\"_entryReplayMode\":\"Normal\"},{\"_entryName\":\"GetEntry\",\"_entryIndex\":1,\"_entryPayload\":{\"jsonResult\":{\"Right\":null},\"jsonKey\":[97,97,97,99]},\"_entryReplayMode\":\"Normal\"},{\"_entryName\":\"DelEntry\",\"_entryIndex\":2,\"_entryPayload\":{\"jsonResult\":{\"Right\":1},\"jsonKeys\":[[97,97,97]]},\"_entryReplayMode\":\"Normal\"}],\"forkedRecordings\":{}}"

deleteExisting :: ResultRecording
deleteExisting = fromJust $ decode "{\"recording\":[{\"_entryName\":\"SetEntry\",\"_entryIndex\":0,\"_entryPayload\":{\"jsonValue\":[98,98,98],\"jsonResult\":{\"Right\":{\"tag\":\"Ok\"}},\"jsonKey\":[97,97,97]},\"_entryReplayMode\":\"Normal\"},{\"_entryName\":\"SetEntry\",\"_entryIndex\":1,\"_entryPayload\":{\"jsonValue\":[100,100,100],\"jsonResult\":{\"Right\":{\"tag\":\"Ok\"}},\"jsonKey\":[99,99,99]},\"_entryReplayMode\":\"Normal\"},{\"_entryName\":\"DelEntry\",\"_entryIndex\":2,\"_entryPayload\":{\"jsonResult\":{\"Right\":2},\"jsonKeys\":[[97,97,97],[99,99,99]]},\"_entryReplayMode\":\"Normal\"}],\"forkedRecordings\":{}}"

deleteKeysNoKeys :: ResultRecording
deleteKeysNoKeys = fromJust $ decode "{\"recording\":[{\"_entryName\":\"DelEntry\",\"_entryIndex\":0,\"_entryPayload\":{\"jsonResult\":{\"Right\":0},\"jsonKeys\":[]},\"_entryReplayMode\":\"Normal\"}],\"forkedRecordings\":{}}"

deleteMissing :: ResultRecording
deleteMissing = fromJust $ decode "{\"recording\":[{\"_entryName\":\"DelEntry\",\"_entryIndex\":0,\"_entryPayload\":{\"jsonResult\":{\"Right\":0},\"jsonKeys\":[[122,122,122],[121,121,121]]},\"_entryReplayMode\":\"Normal\"}],\"forkedRecordings\":{}}"

getCorrectFromTx :: ResultRecording
getCorrectFromTx = fromJust $ decode "{\"recording\":[{\"_entryName\":\"MultiExecEntry\",\"_entryIndex\":0,\"_entryPayload\":{\"jsonResult\":{\"Right\":{\"tag\":\"TxSuccess\",\"contents\":[98,98,98]}}},\"_entryReplayMode\":\"Normal\"}],\"forkedRecordings\":{}}"

getIncorrectFromTx :: ResultRecording
getIncorrectFromTx = fromJust $ decode "{\"recording\":[{\"_entryName\":\"MultiExecEntry\",\"_entryIndex\":0,\"_entryPayload\":{\"jsonResult\":{\"Right\":{\"tag\":\"TxSuccess\",\"contents\":null}}},\"_entryReplayMode\":\"Normal\"}],\"forkedRecordings\":{}}"

setExGetKey :: ResultRecording
setExGetKey = fromJust $ decode "{\"recording\":[{\"_entryName\":\"SetExEntry\",\"_entryIndex\":0,\"_entryPayload\":{\"jsonTtl\":3600,\"jsonValue\":[98,98,98,101,120],\"jsonResult\":{\"Right\":{\"tag\":\"Ok\"}},\"jsonKey\":[97,97,97,101,120]},\"_entryReplayMode\":\"Normal\"},{\"_entryName\":\"GetEntry\",\"_entryIndex\":1,\"_entryPayload\":{\"jsonResult\":{\"Right\":[98,98,98,101,120]},\"jsonKey\":[97,97,97,101,120]},\"_entryReplayMode\":\"Normal\"},{\"_entryName\":\"DelEntry\",\"_entryIndex\":2,\"_entryPayload\":{\"jsonResult\":{\"Right\":1},\"jsonKeys\":[[97,97,97,101,120]]},\"_entryReplayMode\":\"Normal\"}],\"forkedRecordings\":{}}"

setExTtl :: ResultRecording
setExTtl = fromJust $ decode "{\"recording\":[{\"_entryName\":\"SetExEntry\",\"_entryIndex\":0,\"_entryPayload\":{\"jsonTtl\":1,\"jsonValue\":[98,98,98,101,120],\"jsonResult\":{\"Right\":{\"tag\":\"Ok\"}},\"jsonKey\":[97,97,97,101,120]},\"_entryReplayMode\":\"Normal\"},{\"_entryName\":\"RunIOEntry\",\"_entryIndex\":1,\"_entryPayload\":{\"jsonResult\":[]},\"_entryReplayMode\":\"Normal\"},{\"_entryName\":\"GetEntry\",\"_entryIndex\":2,\"_entryPayload\":{\"jsonResult\":{\"Right\":null},\"jsonKey\":[97,97,97,101,120]},\"_entryReplayMode\":\"Normal\"},{\"_entryName\":\"DelEntry\",\"_entryIndex\":3,\"_entryPayload\":{\"jsonResult\":{\"Right\":0},\"jsonKeys\":[[97,97,97,101,120]]},\"_entryReplayMode\":\"Normal\"}],\"forkedRecordings\":{}}"
