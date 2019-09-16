module Test.Framework.Language (testLanguage) where

import           EulerHS.Prelude
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck as QC

import           Test.Types.Runtime
import           Servant.Mock
import           GHC.Generics
import           Data.Aeson
import           Servant.Server
import           Servant.API
import           Servant.Client (ClientM, client, BaseUrl(..), Scheme(..))
import           Test.QuickCheck.Arbitrary
import           Network.Wai.Handler.Warp (run, runSettings, setBeforeMainLoop, setPort, defaultSettings, testWithApplication, Port)
import           EulerHS.Interpreters
import           EulerHS.Language
import           EulerHS.Runtime

data User = User { firstName :: String, lastName :: String }
  deriving (Generic, Show, Eq)

instance ToJSON User
instance FromJSON User

instance Arbitrary User where
  arbitrary = User <$> arbitrary <*> arbitrary

data Book = Book { author :: String, name :: String }
  deriving (Generic, Show, Eq)

instance ToJSON Book
instance FromJSON Book

instance Arbitrary Book where
  arbitrary = Book <$> arbitrary <*> arbitrary

type API = "user" :> Get '[JSON] User
      :<|> "book" :> Get '[JSON] Book

api :: Proxy API
api = Proxy

server :: Server API
server = mock api Proxy

getUser :: ClientM User
getBook :: ClientM Book
(getUser :<|> getBook) = client api

fillEmptyMVarWhenServerIsReady :: Application -> Port -> MVar () -> IO ()
fillEmptyMVarWhenServerIsReady app port mvar = runSettings (setBeforeMainLoop (putMVar mvar ()) $ setPort port defaultSettings) app

runWhenServerIsReady :: Application -> Port -> Assertion -> Assertion
runWhenServerIsReady app port act = do
  sem <- newEmptyMVar
  tId <- forkIO $ fillEmptyMVarWhenServerIsReady app port sem
  readMVar sem
  act
  killThread tId

test01 :: TestRT -> Assertion
test01 rt = runWhenServerIsReady (serve api server) (port rt) $ do
  let url = BaseUrl Http "localhost" (port rt) ""
  bookEither <- withFlowRuntime Nothing $ \flowRt ->
    runFlow flowRt $ callServantAPI url getBook
  case bookEither of
    Left err -> assertFailure $ show err
    Right book -> return ()

test02 :: TestRT -> Assertion
test02 rt = runWhenServerIsReady (serve api server) (port rt) $ do
  let url = BaseUrl Http "localhost" (port rt) ""
  userEither <- withFlowRuntime Nothing $ \flowRt ->
    runFlow flowRt $ callServantAPI url getUser
  case userEither of
    Left err -> assertFailure $ show err
    Right user -> return ()

test03 :: TestRT -> Assertion
test03 rt = do
  let url = BaseUrl Http "localhost" (port rt) ""
  bookEither <- withFlowRuntime Nothing $ \flowRt ->
    runFlow flowRt $ callServantAPI url getBook
  case bookEither of
    Left err -> return ()
    Right book -> assertFailure "Somehow got an answer from nothing"

unitTests :: TestRT -> TestTree
unitTests rt = testGroup "Unit tests" [ testCase "Simple request (book)" (test01 rt)
                                      , testCase "Simple request (book)" (test02 rt)
                                      , testCase "Incorrect request" (test03 rt) ]

testLanguage :: TestRT -> TestTree
testLanguage rt = testGroup "EulerHS.Framework.Language tests" [unitTests rt]
