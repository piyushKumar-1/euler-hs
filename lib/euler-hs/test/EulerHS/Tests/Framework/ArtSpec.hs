{-# LANGUAGE DuplicateRecordFields #-}

module EulerHS.Tests.Framework.ArtSpec where

import EulerHS.Prelude
import Test.Hspec
import EulerHS.Types as T
-- import qualified EulerHS.Framework.Playback.Machine as P
-- import qualified EulerHS.Framework.Playback.Types   as P
-- import qualified EulerHS.Framework.Playback.Entries as P'
import qualified Data.Text as Text
import           EulerHS.Runtime
import           EulerHS.Language as L
import           EulerHS.Interpreters
import qualified Data.Map as Map
import qualified Data.Vector as V

initRegularRT = do
  opts <- newMVar Map.empty
  flowRt <- withFlowRuntime Nothing pure
  pure $ flowRt
   { _runMode = T.RegularMode
   }

initRecorderRT = do
  recMVar <- newMVar V.empty
  forkedRecMvar <- newMVar Map.empty
  opts <- newMVar Map.empty
  let recRt = T.RecorderRuntime
        { flowGUID = "testFlow"
        , recordingMVar = recMVar
        , forkedRecordingsVar = forkedRecMvar
        , disableEntries = []
        }
  flowRt <- withFlowRuntime Nothing pure
  pure $ flowRt
    { _runMode = T.RecordingMode recRt
    }


initPlayerRT recEntries forkedRecordings = do
  opts <- newMVar Map.empty
  step <- newMVar 0
  errMVar <- newMVar Nothing
  ffEV <- newMVar Map.empty
  let pRt = T.PlayerRuntime
        { recording = recEntries
        , stepMVar = step
        , errorMVar = errMVar
        , disableVerify = []
        , disableMocking = []
        , skipEntries = []
        , entriesFiltered = False
        , flowGUID = "MainFlow"
        , forkedFlowRecordings = forkedRecordings
        , forkedFlowErrorsVar = ffEV
        }
  flowRt <- withFlowRuntime Nothing pure
  pure $ flowRt
    { _runMode = T.ReplayingMode pRt
    }

cmdScript = do
  guid <- generateGUID
  _ <- L.runIO $ pure ("Some IO 1" :: String)
  -- conn :: DBResult (T.SqlConn Redis) <- initSqlDBConnection $ T.SQLiteConfig "dbname"
  logInfo "Art test:" $ Text.append "Generated guid is:" guid
  forkFlow "forked test flow1" forkScript
  forkFlow "forked test flow2" forkScript
  runSysCmd "echo hello"

cmdScript1 = do
  guid <- generateGUID
  _ <- L.runIO $ pure ("Some IO 1" :: String)
  -- conn :: DBResult (T.SqlConn Redis) <- initSqlDBConnection $ T.SQLiteConfig "dbname"
  logInfo "Art test:" $ Text.append "Generated guid is:" guid
  forkFlow "forked test flow1" forkScript
  runSysCmd "echo hello"

cmdScript2 = do
  guid <- generateGUID
  _ <- L.runIO $ pure ("Some IO 1" :: String)
  -- conn :: DBResult (T.SqlConn Redis) <- initSqlDBConnection $ T.SQLiteConfig "dbname"
  logInfo "Art test:" $ Text.append "Generated guid is:" guid
  forkFlow "forked test flow1" forkScript
  forkFlow "forked test flow2" forkScript1
  runSysCmd "echo hello"

forkScript = do
  guid <- generateGUID
  logInfo "Art test 2:" $ guid
  runSysCmd "echo hello from 2-nd script"

forkScript1 = do
  guid <- generateGUID
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
        T.RecordingMode rrt -> do
          recs <- readMVar (T.recordingMVar rrt)
          forkedRecsMap <- readMVar (T.forkedRecordingsVar rrt)
          (V.length recs) `shouldBe` 10
          (Map.size forkedRecsMap) `shouldBe` 2
          res `shouldBe` "hello\n"
        _ -> fail "wrong mode"

    it "Player mode: correct flow to replay" $ do
      rt <- initRecorderRT
      res <- runFlow rt cmdScript
      case _runMode rt of
        T.RecordingMode rrt -> do
          entries <- readMVar (T.recordingMVar rrt)
          forkedRecordings <- readMVar (T.forkedRecordingsVar rrt)
          pRt <- initPlayerRT entries =<< sequence (fmap readMVar forkedRecordings)
          res2 <- runFlow pRt cmdScript
          res2 `shouldBe` res
          case _runMode pRt of
            T.ReplayingMode prtm -> do
              errors <- readMVar (T.errorMVar prtm)
              errors `shouldBe` Nothing

              forkedErrors  <- readMVar (T.forkedFlowErrorsVar prtm)
              forkedMErrors <- sequence (fmap (readMVar @IO) $ Map.elems forkedErrors)
              catMaybes forkedMErrors `shouldBe` []
        _ -> fail "wrong mode"

    it "Player mode: incorrect flow to replay" $ do
      rt <- initRecorderRT
      res <- runFlow rt cmdScript
      case _runMode rt of
        T.RecordingMode rrt -> do
          entries <- readMVar (T.recordingMVar rrt)
          forkedRecordings <- readMVar (T.forkedRecordingsVar rrt)
          pRt <- initPlayerRT entries =<< sequence (fmap readMVar forkedRecordings)
          _ <- try @_ @SomeException $ runFlow pRt cmdScript1
          case _runMode pRt of
            T.ReplayingMode prtm -> do
              errors <- readMVar (T.errorMVar prtm)
              errors `shouldNotBe` Nothing
        _ -> fail "wrong mode"


    it "Player mode: incorrect flowFork to replay" $ do
      rt <- initRecorderRT
      res <- runFlow rt cmdScript
      case _runMode rt of
        T.RecordingMode rrt -> do
          entries <- readMVar (T.recordingMVar rrt)
          forkedRecordings <- readMVar (T.forkedRecordingsVar rrt)
          pRt <- initPlayerRT entries =<< sequence (fmap readMVar forkedRecordings)
          _ <- try @_ @SomeException $ runFlow pRt cmdScript2
          case _runMode pRt of
            T.ReplayingMode prtm -> do
              errors <- readMVar (T.errorMVar prtm)
              errors `shouldBe` Nothing

              forkedErrors  <- readMVar (T.forkedFlowErrorsVar prtm)
              forkedMErrors <- sequence (fmap (readMVar @IO) $ Map.elems forkedErrors)
              (length $ catMaybes forkedMErrors) `shouldBe` 1
        _ -> fail "wrong mode"
