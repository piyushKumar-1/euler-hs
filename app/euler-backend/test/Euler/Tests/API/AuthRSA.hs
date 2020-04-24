{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}

module Euler.Tests.API.AuthRSA where

import Euler.Tests.Common

import           Data.Time.Clock (NominalDiffTime)
import qualified Data.Text                              as Text

import           EulerHS.Prelude
import           EulerHS.Language
import           EulerHS.Runtime
import           EulerHS.Interpreters
import qualified EulerHS.Types                          as T

import qualified Euler.API.Authentication               as Auth
import           Euler.Server (FlowServer', eulerServer_, FlowHandler)
import qualified Euler.Server                           as S
import qualified Euler.Playback.MethodPlayer            as S

import           Network.Wai.Handler.Warp
import           Network.HTTP.Types

import           Servant.Server (serve)
import           Servant.API
import           Servant.Client
import           System.Process
import           System.Exit

import           Test.Hspec

import qualified WebService.Types                       as T
import qualified WebService.Language                    as L
import           Euler.API.RouteParameters

spec :: Spec
spec = pure ()
  where
  x = describe "Authentication API" $ do
    around_ (prepareDB . runServer) $
      describe "Authentication API, server with DB" $ do
        it "Authentication works" $ withFlowRuntime Nothing $ \rt -> do
          let url = BaseUrl Http "localhost" port ""
          eres <- runFlow rt $ callAPI url $ protectedClient $
            Auth.Signed
              { signature_payload = "{ \"merchant_id\": \"merchantId\", \"someField\": \"Something here\", \"someOtherField\": true}"
              , signature         = "o9AQmdfJgIxlcVVjdzhQuonzOKRczg6HZzOPaxtusUndI4yac4fLfN6WHVdHUtGYiqCo0NapVB2BWATTyGp2zSjwWYO1OopZiX88tQSFUzIQT6/OT0kZ4XHaj6UM5nPpW+xZGXsmB1mWMwVHgSsVgw5HGpPZoi365QvgV/6EchNlIGRGlCLmM1l67f6AOcHrZwSbrqbT+yd/HP6JJAMg83ffVfbdQKwqB8/j4IMfAoZO7e/MLHlGBvNlpDfRNryXLhljVD0EI8q5lejbYFMuQgwkXoU4w7WzBA9p+hXvw48mf/sDy7jLnTOc/q6Sn/W7R2hvXkFinMygV98bnao9kA=="
              , merchant_key_id   = 1
              }

          eres `shouldSatisfy` isRight
          let Right res = eres
          res `shouldBe` "Something here"


        it "If malformed signature should return 401" $ withFlowRuntime Nothing $ \rt -> do
          let url = BaseUrl Http "localhost" port ""
          eerr <- runFlow rt $ callAPI url $ protectedClient $
            Auth.Signed
              { signature_payload = "{\"merchant_id\": \"merchantId\", \"someField\": \"Something here\", \"someOtherField\": true}"
              , signature         = ":::::malformed signature:::::"
              , merchant_key_id   = 1
              }

          eerr `shouldSatisfy` isLeft
          let Left (FailureResponse _ Response { responseStatusCode = status, responseBody = body })= eerr
          status `shouldBe` status401
          body   `shouldBe` "Malformed signature. Expected Base64 encoded signature."

        it "If malformed signature_payload should return 401" $ withFlowRuntime Nothing $ \rt -> do
          let url = BaseUrl Http "localhost" port ""
          eerr <- runFlow rt $ callAPI url $ protectedClient $
            Auth.Signed
              { signature_payload = ":::::malformed signature_payload::::::"
              , signature         = "o9AQmdfJgIxlcVVjdzhQuonzOKRczg6HZzOPaxtusUndI4yac4fLfN6WHVdHUtGYiqCo0NapVB2BWATTyGp2zSjwWYO1OopZiX88tQSFUzIQT6/OT0kZ4XHaj6UM5nPpW+xZGXsmB1mWMwVHgSsVgw5HGpPZoi365QvgV/6EchNlIGRGlCLmM1l67f6AOcHrZwSbrqbT+yd/HP6JJAMg83ffVfbdQKwqB8/j4IMfAoZO7e/MLHlGBvNlpDfRNryXLhljVD0EI8q5lejbYFMuQgwkXoU4w7WzBA9p+hXvw48mf/sDy7jLnTOc/q6Sn/W7R2hvXkFinMygV98bnao9kA=="
              , merchant_key_id   = 1
              }

          eerr `shouldSatisfy` isLeft
          let Left (FailureResponse _ Response { responseStatusCode = status, responseBody = body })= eerr
          status `shouldBe` status401
          body   `shouldBe` "Malformed signature_payload. Can not parse expected structure from JSON."

        it "If wrong merchant_key_id should return 401 without exposing details" $ withFlowRuntime Nothing $ \rt -> do
          let url = BaseUrl Http "localhost" port ""
          eerr <- runFlow rt $ callAPI url $ protectedClient $
            Auth.Signed
              { signature_payload = "{\"merchant_id\": \"merchantId\", \"someField\": \"Something here\", \"someOtherField\": true}"
              , signature         = "o9AQmdfJgIxlcVVjdzhQuonzOKRczg6HZzOPaxtusUndI4yac4fLfN6WHVdHUtGYiqCo0NapVB2BWATTyGp2zSjwWYO1OopZiX88tQSFUzIQT6/OT0kZ4XHaj6UM5nPpW+xZGXsmB1mWMwVHgSsVgw5HGpPZoi365QvgV/6EchNlIGRGlCLmM1l67f6AOcHrZwSbrqbT+yd/HP6JJAMg83ffVfbdQKwqB8/j4IMfAoZO7e/MLHlGBvNlpDfRNryXLhljVD0EI8q5lejbYFMuQgwkXoU4w7WzBA9p+hXvw48mf/sDy7jLnTOc/q6Sn/W7R2hvXkFinMygV98bnao9kA=="
              , merchant_key_id   = 9999999
              }

          eerr `shouldSatisfy` isLeft
          let Left (FailureResponse _ Response { responseStatusCode = status, responseBody = body })= eerr
          status `shouldBe` status401
          body   `shouldBe` ""

        it "If unable to fetch without exposing details" $ withFlowRuntime Nothing $ \rt -> do
          let url = BaseUrl Http "localhost" port ""
          eerr <- runFlow rt $ callAPI url $ protectedClient $
            Auth.Signed
              { signature_payload = "{\"merchant_id\": \"merchantId\", \"someField\": \"Something here\", \"someOtherField\": true}"
              , signature         = "o9AQmdfJgIxlcVVjdzhQuonzOKRczg6HZzOPaxtusUndI4yac4fLfN6WHVdHUtGYiqCo0NapVB2BWATTyGp2zSjwWYO1OopZiX88tQSFUzIQT6/OT0kZ4XHaj6UM5nPpW+xZGXsmB1mWMwVHgSsVgw5HGpPZoi365QvgV/6EchNlIGRGlCLmM1l67f6AOcHrZwSbrqbT+yd/HP6JJAMg83ffVfbdQKwqB8/j4IMfAoZO7e/MLHlGBvNlpDfRNryXLhljVD0EI8q5lejbYFMuQgwkXoU4w7WzBA9p+hXvw48mf/sDy7jLnTOc/q6Sn/W7R2hvXkFinMygV98bnao9kA=="
              , merchant_key_id   = 9999999
              }

          eerr `shouldSatisfy` isLeft
          let Left (FailureResponse _ Response { responseStatusCode = status, responseBody = body })= eerr
          status `shouldBe` status401
          body   `shouldBe` ""

    around_ (withFlowRuntime Nothing . runServer) $
      describe "Authentication API, server with no DB" $
        it "Server returns 500 for valid request" $ withFlowRuntime Nothing $ \rt -> do
          let url = BaseUrl Http "localhost" port ""
          eerr <- runFlow rt $ callAPI url $ protectedClient $
            Auth.Signed
              { signature_payload = "{ \"merchant_id\": \"merchantId\", \"someField\": \"Something here\", \"someOtherField\": true}"
              , signature         = "o9AQmdfJgIxlcVVjdzhQuonzOKRczg6HZzOPaxtusUndI4yac4fLfN6WHVdHUtGYiqCo0NapVB2BWATTyGp2zSjwWYO1OopZiX88tQSFUzIQT6/OT0kZ4XHaj6UM5nPpW+xZGXsmB1mWMwVHgSsVgw5HGpPZoi365QvgV/6EchNlIGRGlCLmM1l67f6AOcHrZwSbrqbT+yd/HP6JJAMg83ffVfbdQKwqB8/j4IMfAoZO7e/MLHlGBvNlpDfRNryXLhljVD0EI8q5lejbYFMuQgwkXoU4w7WzBA9p+hXvw48mf/sDy7jLnTOc/q6Sn/W7R2hvXkFinMygV98bnao9kA=="
              , merchant_key_id   = 1
              }

          eerr `shouldSatisfy` isLeft
          let Left (FailureResponse _ Response { responseStatusCode = status, responseBody = body })= eerr
          status `shouldBe` status500
          body   `shouldBe` ""

----------------------------------------------------------------------

data SignaturePayloadExample = SignaturePayloadExample
  { merchant_id    :: Text   -- ^ merchant_id is mandatory
  , someField      :: String
  , someOtherField :: Bool
  } deriving (Generic, FromJSON, ToJSON, Show)

type API = "protected" :> ReqBody '[JSON] (Auth.Signed SignaturePayloadExample) :> Post '[JSON] Text

api :: Proxy API
api = Proxy

port :: Int
port = 8081

protectedClient :: Auth.Signed SignaturePayloadExample -> T.EulerClient Text
protectedClient = T.client api

server :: FlowServer' API
server = protected

protected :: Auth.Signed SignaturePayloadExample -> FlowHandler Text
protected = S.runFlow "sTestFlow" emptyRPs S.noReqBodyJSON . Auth.authenticateUsingRSAEncryption handler
  where
    handler {-merchantAccount-}_ signaturePayloadExample = do
      pure $ Text.pack $ someField signaturePayloadExample

----------------------------------------------------------------------

sqliteConn :: IsString a => a
sqliteConn = "sqlite"

keepConnsAliveForSecs :: NominalDiffTime
keepConnsAliveForSecs = 60 * 10 -- 10 mins

maxTotalConns :: Int
maxTotalConns = 8

testDBName :: String
testDBName = "./test/Euler/TestData/authRSAtest.db"

testDBTemplateName :: String
testDBTemplateName = "./test/Euler/TestData/authRSAtest.db.template"

rmTestDB :: IO ()
rmTestDB = void $ try @_ @SomeException $ system $ "rm -f " <> testDBName

cpTestDB :: IO ()
cpTestDB = do
  res <- try @_ @SomeException $ system $ "cp " <> testDBTemplateName <> " " <> testDBName
  case res of
    Right ExitSuccess -> pure ()
    _                 -> error "Unable to copy rsa test db mockup"

prepareTestDB :: IO() -> IO ()
prepareTestDB action = bracket_ rmTestDB rmTestDB (cpTestDB >> action)

prepareDBConnections :: Flow ()
prepareDBConnections = do
  ePool <- initSqlDBConnection
    $ T.mkSQLitePoolConfig sqliteConn testDBName
    $ T.PoolConfig 1 keepConnsAliveForSecs maxTotalConns
  L.throwOnFailedWithLog ePool T.SqlDBConnectionFailedException "Failed to connect to SQLite DB."

prepareDB :: (FlowRuntime -> IO ()) -> IO()
prepareDB next = withFlowRuntime Nothing $ \flowRt ->
  prepareTestDB $
    try (runFlow flowRt prepareDBConnections) >>= \case
      Left (e :: SomeException) -> putStrLn @String $ "Exception thrown: " <> show e
      Right _ -> next flowRt

runServer :: IO () -> FlowRuntime -> IO ()
runServer act flowRt = do
    let env = S.Env flowRt Nothing mkAppEnv
    serverStartupLock <- newEmptyMVar
    let settings = setBeforeMainLoop (putMVar serverStartupLock ()) $ setPort port defaultSettings
    threadId <- forkIO $ runSettings settings $ serve api (eulerServer_ api server env)
    readMVar serverStartupLock
    finally act $ killThread threadId
