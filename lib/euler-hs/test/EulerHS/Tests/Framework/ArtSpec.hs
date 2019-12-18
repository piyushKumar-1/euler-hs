{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE StandaloneDeriving    #-}

module EulerHS.Tests.Framework.ArtSpec where

import           Data.Aeson as A
import           Data.Aeson.Encode.Pretty
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
import           EulerHS.TestData.Types
import           EulerHS.Tests.Framework.Common
import           EulerHS.Types as T


spec :: Spec
spec = do
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



