module EulerHS.Tests.Framework.Common where

import qualified Data.Map as Map
import qualified Data.Vector as V
import           Network.Wai.Handler.Warp
import           Servant.Client
import           Servant.Server
import           Test.Hspec

import           EulerHS.Interpreters
import           EulerHS.Language as L
import           EulerHS.Prelude
import           EulerHS.Runtime
import           EulerHS.TestData.API.Client
import           EulerHS.Types as T


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
      , flowGUID        = "testFlow"
      }

  flowRuntime <- withFlowRuntime Nothing pure
  pure $ flowRuntime { _runMode = T.ReplayingMode playerRuntime }

replayRecording :: ResultRecording -> Flow a -> IO a
replayRecording rec flow = do
  (errors, result) <- runFlowReplaying pure rec flow
  flattenErrors errors `shouldBe` []
  pure result
