{-# LANGUAGE DuplicateRecordFields #-}

module EulerHS.Tests.Framework.ArtSpec where

import EulerHS.Prelude
import Test.Hspec
-- import qualified EulerHS.Framework.Playback.Machine as P
import qualified EulerHS.Framework.Playback.Types   as P
import qualified EulerHS.Framework.Playback.Entries as P
import           EulerHS.Runtime
import           EulerHS.Language as L
import           EulerHS.Interpreters
import qualified Data.Map as Map
import qualified Data.Vector as V

initRegularRT = do
  opts <- newMVar Map.empty
  flowRt <- withFlowRuntime Nothing pure
  pure $ flowRt
   { _runMode = P.RegularMode
   }

initRecorderRT = do
  recMVar <- newMVar V.empty
  forkedRecMvar <- newMVar Map.empty
  opts <- newMVar Map.empty
  let recRt = P.RecorderRuntime
        { flowGUID = "testFlow"
        , recordingMVar = recMVar
        , forkedRecordingsVar = forkedRecMvar
        , disableEntries = []
        }
  flowRt <- withFlowRuntime Nothing pure
  pure $ flowRt
    { _runMode = P.RecordingMode recRt
    }


initPlayerRT recEntries = do
  opts <- newMVar Map.empty
  step <- newMVar 0
  errMVar <- newMVar Nothing
  ffEV <- newMVar Map.empty
  let pRt = P.PlayerRuntime
        { recording = recEntries
        , stepMVar = step
        , errorMVar = errMVar
        , disableVerify = []
        , disableMocking = []
        , skipEntries = []
        , entriesFiltered = False
        , flowGUID = "MainFlow"
        , forkedFlowRecordings = Map.empty
        , forkedFlowErrorsVar = ffEV
        }
  flowRt <- withFlowRuntime Nothing pure
  pure $ flowRt
    { _runMode = P.ReplayingMode pRt
    }

cmdScript = do
  guid <- generateGUID
  _ <- L.runIO $ pure ("Some IO 1" :: String)
  -- logInfo $ "Generated guid is: " ++ guid
  forkFlow "forked test flow" cmdScript2
  runSysCmd "echo hello"

cmdScript2 = do
  guid <- generateGUID
  -- logInfo $ "Generated guid from 2-nd script is: " ++ guid
  runSysCmd "echo hello from 2-nd script"



spec :: Spec
spec =
  describe "Tests" $ do
    it "Regular mode" $ do
      rt <- initRegularRT
      res <- runFlow rt cmdScript
      res `shouldBe` "hello\n"

    it "Recorder mode" $ do
      rt <- initRecorderRT
      res <- runFlow rt cmdScript
      case _runMode rt of
        P.RecordingMode rrt -> do
          recs <- readMVar (P.recordingMVar rrt)
          (V.length recs) `shouldBe` 5
          res `shouldBe` "hello\n"
        _ -> fail "wrong mode"

    it "Player mode" $ do
      rt <- initRecorderRT
      res <- runFlow rt cmdScript
      case _runMode rt of
        P.RecordingMode rrt -> do
          entries <- readMVar (P.recordingMVar rrt)
          pRt <- initPlayerRT entries
          res2 <- runFlow pRt cmdScript
          res `shouldBe` res2
          case _runMode pRt of
            P.ReplayingMode prtm -> do
              errors <- readMVar (P.errorMVar prtm)
              errors `shouldBe` Nothing
        _ -> fail "wrong mode"
