module EulerHS.Tests.Framework.FlowSpec where

import           EulerHS.Prelude hiding (getOption, get)
import           Test.Hspec hiding (runIO)
import           Network.Wai.Handler.Warp
import           Data.Aeson               (encode)
import qualified Data.ByteString.Lazy as BSL
import           Unsafe.Coerce
import qualified Data.UUID as UUID (fromText)
import qualified Control.Exception as E
import           Servant.Server
import           Servant.Client (BaseUrl(..), Scheme(..))
import           EulerHS.Interpreters
import           EulerHS.Language as L
import           EulerHS.Runtime (withFlowRuntime)
import           EulerHS.TestData.Types
import           EulerHS.TestData.API.Client
import           EulerHS.TestData.Scenarios.Scenario1 (testScenario1)
import           EulerHS.Testing.Types (FlowMockedValues'(..))
import           EulerHS.Testing.Flow.Interpreter (runFlowWithTestInterpreter)

user :: Any
user = unsafeCoerce $ Right $ User "John" "Snow" "00000000-0000-0000-0000-000000000000"

localGUID :: Any
localGUID = unsafeCoerce ("FFFFFFFF-FFFF-FFFF-FFFF-FFFFFFFFFFFF" :: String)

lhost :: BSL.ByteString
lhost = encode ("localhost" :: String)


scenario1MockedValues :: FlowMockedValues'
scenario1MockedValues = FlowMockedValues'
  { mockedCallServantAPI = [user]
  , mockedRunIO = [localGUID]
  , mockedGetOption = [lhost]
  , mockedGenerateGUID = ["00000000-0000-0000-0000-000000000000"]
  , mockedRunSysCmd = ["Neo"]
  }

ioActWithException :: IO Text
ioActWithException = do
  E.throw (E.AssertionFailed "Exception from IO")
  pure "Text from IO"

withServer :: IO () -> IO ()
withServer action = do
  serverStartupLock <- newEmptyMVar
  let settings = setBeforeMainLoop (putMVar serverStartupLock ()) $
        setPort port defaultSettings
  threadId          <- forkIO $ runSettings settings $ serve api server
  readMVar serverStartupLock
  action
  killThread threadId


spec :: Spec
spec = do
  around (withFlowRuntime Nothing) $ do

    describe "EulerHS flow language tests" $ do

      describe "TestInterpreters" $ do

        it "testScenario1" $ \rt -> do
          mv <- newMVar scenario1MockedValues
          res <- runFlowWithTestInterpreter mv rt testScenario1
          res `shouldBe` (User "John" "Snow" "00000000-0000-0000-0000-000000000000")

      around_ withServer $ do
        describe "CallServantAPI tests with server" $ do

          it "Simple request (book)" $ \rt -> do
            let url = BaseUrl Http "localhost" port ""
            bookEither <- runFlow rt $ callServantAPI url getBook
            bookEither `shouldSatisfy` isRight

          it "Simple request (user)" $ \rt -> do
            let url = BaseUrl Http "localhost" port ""
            userEither <- runFlow rt $ callServantAPI url getUser
            userEither `shouldSatisfy` isRight

      describe "CallServantAPI tests without server" $ do

        it "Simple request (book)" $ \rt -> do
          let url = BaseUrl Http "localhost" port ""
          bookEither <- runFlow rt $ callServantAPI url getBook
          bookEither `shouldSatisfy` isLeft

        it "Simple request (user)" $ \rt -> do
          let url = BaseUrl Http "localhost" port ""
          userEither <- runFlow rt $ callServantAPI url getUser
          userEither `shouldSatisfy` isLeft

      it "RunIO" $ \rt -> do
        result <- runFlow rt $ runIO (pure ("hi" :: String))
        result `shouldBe` "hi"

      it "RunIO with exception" $ \rt -> do
        result <- E.catch
          (runFlow rt $ do
            runIO ioActWithException
            pure ("Never returned" :: Text))
          (\e -> do let err = show (e :: E.AssertionFailed)
                    pure err)
        result `shouldBe` ("Exception from IO" :: Text)

      it "RunIO with catched exception" $ \rt -> do
        result <-runFlow rt $ do
            runIO $
              E.catch
                ioActWithException
                (\e -> do let err = show (e :: E.AssertionFailed)
                          pure err)
        result `shouldBe` ("Exception from IO" :: Text)

      describe "Options" $ do

        it "One key" $ \rt -> do
          result <- runFlow rt $ do
            _ <- setOption @TestStringKey "lore ipsum"
            getOption @TestStringKey
          result `shouldBe` (Just "lore ipsum")

        it "Not found" $ \rt -> do
          result <- runFlow rt $ do
            _ <- setOption @TestStringKey "lore ipsum"
            getOption @TestStringKey2
          result `shouldBe` Nothing


        it "Two keys" $ \rt -> do
          result <- runFlow rt $ do
            _ <- setOption @TestStringKey "lore ipsum"
            _ <- setOption @TestStringKey2 "lore ipsum2"
            s1 <- getOption @TestStringKey
            s2 <- getOption @TestStringKey2
            pure (s1,s2)
          result `shouldBe` (Just "lore ipsum", Just "lore ipsum2")

--        it "Different encoding" $ \rt -> do
--          result <- runFlow rt $ do
--            _ <- setOption @TestStringKey "lore ipsum"
--            _ <- setOption @TestStringKey2 "lore ipsum2"
--            _ <- setOption @TestStringKeyAnotherEnc "lore ipsum enc"
--            _ <- setOption @TestStringKey2AnotherEnc "lore ipsum2 enc"
--            s1 <- getOption @TestStringKey
--            s2 <- getOption @TestStringKey2
--            s1enc <- getOption @TestStringKeyAnotherEnc
--            s2enc <- getOption @TestString2KeyAnotherEnc
--            pure (s1,s2,s1enc,s2enc)
--          result `shouldBe` (Just "lore ipsum", Just "lore ipsum2", Just "lore ipsum enc", Just "lore ipsum2 enc")


      it "RunSysCmd" $ \rt -> do
        result <- runFlow rt $ runSysCmd "echo test"
        result `shouldBe` "test\n"

      it "RunSysCmd with bad command" $ \rt -> do
        result <- E.catch
          (runFlow rt $ runSysCmd "badEcho test")
          (\e -> do let err = show (e :: E.SomeException)
                    pure err)
        result `shouldBe` ("readCreateProcess: badEcho test (exit 127): failed" :: String)


      it "GenerateGUID" $ \rt -> do
        guid <- runFlow rt generateGUID
        let maybeGUID = UUID.fromText guid
        maybeGUID `shouldSatisfy` isJust

      it "ThrowException" $ \rt -> do
        result <- E.catch
          (runFlow rt $ do
            throwException (E.AssertionFailed "Exception message")
            pure "Never returned")
          (\e -> do let err = show (e :: E.AssertionFailed)
                    pure err)
        result `shouldBe` "Exception message"
