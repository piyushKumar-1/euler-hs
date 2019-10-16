module EulerHS.Tests.Framework.FlowSpec where

import           EulerHS.Prelude hiding (getOption, get)
import           Test.Hspec hiding (runIO)
import           Network.Wai.Handler.Warp (run)
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


runServer :: IO ()
runServer = void $ forkIO $ run port (serve api server)

spec :: Spec
spec = do
  around (withFlowRuntime Nothing) $ do

    describe "EulerHS flow language tests" $ do

      describe "TestInterpreters" $ do

        it "testScenario1" $ \rt -> do
          mv <- newMVar scenario1MockedValues
          res <- runFlowWithTestInterpreter mv rt testScenario1
          res `shouldBe` (User "John" "Snow" "00000000-0000-0000-0000-000000000000")

      beforeAll_ runServer $ do
        describe "CallServantAPI tests" $ do

          it "Simple request (book)" $ \rt -> do
            let url = BaseUrl Http "localhost" port ""
            bookEither <- runFlow rt $ callServantAPI url getBook
            bookEither `shouldSatisfy` isRight

          it "Simple request (user)" $ \rt -> do
            let url = BaseUrl Http "localhost" port ""
            userEither <- runFlow rt $ callServantAPI url getUser
            userEither `shouldSatisfy` isRight

      it "RunIO" $ \rt -> do
        result <- runFlow rt $ runIO (pure ("hi" :: String))
        result `shouldBe` "hi"

      it "Options Set Get" $ \rt -> do
        result <- runFlow rt $ do
          _ <- setOption TestStringKey "lore ipsum"
          getOption TestStringKey
        result `shouldBe` (Just "lore ipsum")

      it "RunSysCmd" $ \rt -> do
        result <- runFlow rt $ runSysCmd "echo test"
        result `shouldBe` "test\n"

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
