module Test.Framework.Language (testLanguage) where

import           EulerHS.Prelude hiding (getOption)
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck as QC

import           Servant.Mock
import           GHC.Generics
import           Data.Aeson
import qualified Data.UUID                       as UUID (fromText)
import qualified Control.Exception               as E
import           Servant.Server
import           Servant.API
import           Servant.Client (ClientM, client, BaseUrl(..), Scheme(..))
import           Test.QuickCheck.Arbitrary
import           Network.Wai.Handler.Warp (run, runSettings, setBeforeMainLoop, setPort, defaultSettings, testWithApplication, Port)

import           EulerHS.Types
import           EulerHS.Interpreters
import           EulerHS.Language
import           EulerHS.Runtime
import           EulerHS.Types

data TestStringKey = TestStringKey
  deriving (Generic, Show, Eq)

instance ToJSON TestStringKey
instance FromJSON TestStringKey

instance OptionEntity TestStringKey String


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

port :: Int
port = 8081

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

testRunSysCmd :: FlowRuntime -> Assertion
testRunSysCmd rt = do
  result <- runFlow rt $ runSysCmd "echo test"
  case result of
    "test\n" -> pure ()
    _ -> assertFailure "runSysCmd failed"

testGenerateGUID :: FlowRuntime -> Assertion
testGenerateGUID rt = do
  guid <- runFlow rt $ generateGUID
  let maybeGUID = UUID.fromText guid
  case maybeGUID of
    Just _ -> pure ()
    Nothing -> assertFailure "Incorrect GUID generation"

testThrowException :: FlowRuntime -> Assertion
testThrowException rt = do
  res <- E.catch
    (runFlow rt $ do
      throwException (E.AssertionFailed "Exception message")
      pure "Newer returned")
    (\e -> do let err = show (e :: E.AssertionFailed)
              pure err)
  case res of
    "Exception message" -> pure ()
    _ -> assertFailure "Incorrect throwException"


test01 :: FlowRuntime -> Assertion
test01 rt = runWhenServerIsReady (serve api server) port $ do
  let url = BaseUrl Http "localhost" port ""
  bookEither <- runFlow rt $
    callServantAPI url getBook
  case bookEither of
    Left err -> assertFailure $ show err
    Right book -> return ()

test02 :: FlowRuntime -> Assertion
test02 rt = runWhenServerIsReady (serve api server) port $ do
  let url = BaseUrl Http "localhost" port ""
  userEither <- runFlow rt $
    callServantAPI url getUser
  case userEither of
    Left err -> assertFailure $ show err
    Right user -> return ()

test03 :: FlowRuntime -> Assertion
test03 rt = do
  let url = BaseUrl Http "localhost" port ""
  bookEither <- runFlow rt $
    callServantAPI url getBook
  case bookEither of
    Left err -> return ()
    Right book -> assertFailure "Somehow got an answer from nothing"

test04 :: FlowRuntime -> Assertion
test04 rt = do
  result <- runFlow rt $
    runIO (pure ("hi" :: String))
  case result of
    "hi" -> return ()
    _ -> assertFailure $ "incorrect runIO behavior"

test05 :: FlowRuntime -> Assertion
test05 rt = do
  result <- runFlow rt $ do
    _ <- setOption TestStringKey "lore ipsum"
    getOption TestStringKey
  case result of
    Just "lore ipsum" -> return ()
    _ -> assertFailure $ "incorrect options get set behavior"

unitTests :: FlowRuntime -> TestTree
unitTests rt = testGroup "Unit tests" [ testCase "Simple request (book)" (test01 rt)
                                      , testCase "Simple request (book)" (test02 rt)
                                      , testCase "Incorrect request" (test03 rt)
                                      , testCase "RunIO" (test04 rt)
                                      , testCase "Options set get" (test05 rt)
                                      , testCase "test RunSysCmd" (testRunSysCmd rt)
                                      , testCase "test GenerateGUID" (testGenerateGUID rt)
                                      , testCase "test ThrowException" (testThrowException rt)
                                      ]

testLanguage :: FlowRuntime -> TestTree
testLanguage rt = testGroup "EulerHS.Framework.Language tests" [unitTests rt]
