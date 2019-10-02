module EulerHS.Tests.Framework.Languages.Flow (flow) where

import           EulerHS.Prelude   hiding (getOption)
import           Test.Hspec        hiding (runIO)
import           Network.Wai.Handler.Warp (run)



import qualified Data.UUID                       as UUID (fromText)
import qualified Control.Exception               as E
import           Servant.Server
import           Servant.Client (BaseUrl(..), Scheme(..))

import           EulerHS.Types
import           EulerHS.Interpreters
import           EulerHS.Language
import           EulerHS.Runtime

import           EulerHS.Helpers
import           EulerHS.Tests.Framework.FlowRuntime
import           EulerHS.TestData

import EulerHS.TestData.Scenarios.Scenario1 (testScenario1, scenario1MockedValues)
import EulerHS.Tests.Framework.Interpreters.TestInterpreter (runFlowWithTestInterpreter)


withDefaultFlowRt :: (FlowRuntime -> IO ()) -> IO ()
withDefaultFlowRt = bracket initDefaultFlowRt (const (pure ()))

runServer :: IO ()
runServer = void $ forkIO $ run port (serve api server)

flow :: Spec
flow = do
  around withDefaultFlowRt $ do

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
            bookEither <- runFlow rt $
              callServantAPI url getBook
            bookEither `shouldSatisfy` isRight

          it "Simple request (user)" $ \rt -> do
            let url = BaseUrl Http "localhost" port ""
            userEither <- runFlow rt $
              callServantAPI url getUser
            userEither `shouldSatisfy` isRight

      it "RunIO" $ \rt -> do
        result <- runFlow rt $ do
          runIO (pure ("hi" :: String))
        result `shouldBe` "hi"

      it "Options Set Get" $ \rt -> do
        result <- runFlow rt $ do
          _ <- setOption TestStringKey "lore ipsum"
          getOption TestStringKey
        result `shouldBe` (Just "lore ipsum")

      it "RunSysCmd" $ \rt -> do
        result <- runFlow rt $ do
          runSysCmd "echo test"
        result `shouldBe` "test\n"

      it "GenerateGUID" $ \rt -> do
        guid <- runFlow rt $ do
          generateGUID
        let maybeGUID = UUID.fromText guid
        maybeGUID `shouldSatisfy` isJust

      it "ThrowException" $ \rt -> do
        result <- E.catch
          (runFlow rt $ do
            throwException (E.AssertionFailed "Exception message")
            pure "Newer returned")
          (\e -> do let err = show (e :: E.AssertionFailed)
                    pure err)
        result `shouldBe` "Exception message"