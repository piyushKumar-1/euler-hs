{-# LANGUAGE OverloadedStrings #-}

module Euler.Tests.API.AuthRSA where


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

import           Network.Wai.Handler.Warp
import           Network.HTTP.Types

import           Servant.Server (serve)
import           Servant.API
import           Servant.Client

import           Test.Hspec

import qualified WebService.Types                       as T
import qualified WebService.Language                    as L

spec :: Spec
spec =
  around_ runServer $
    describe "Authentication API" $ do
      it "Authentication works" $ withFlowRuntime Nothing $ \rt -> do
        let url = BaseUrl Http "localhost" port ""
        eres <- runFlow rt $ callServantAPI url $ protectedClient $
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
        eerr <- runFlow rt $ callServantAPI url $ protectedClient $
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
        eerr <- runFlow rt $ callServantAPI url $ protectedClient $
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
        eerr <- runFlow rt $ callServantAPI url $ protectedClient $
          Auth.Signed
            { signature_payload = "{\"merchant_id\": \"merchantId\", \"someField\": \"Something here\", \"someOtherField\": true}"
            , signature         = "o9AQmdfJgIxlcVVjdzhQuonzOKRczg6HZzOPaxtusUndI4yac4fLfN6WHVdHUtGYiqCo0NapVB2BWATTyGp2zSjwWYO1OopZiX88tQSFUzIQT6/OT0kZ4XHaj6UM5nPpW+xZGXsmB1mWMwVHgSsVgw5HGpPZoi365QvgV/6EchNlIGRGlCLmM1l67f6AOcHrZwSbrqbT+yd/HP6JJAMg83ffVfbdQKwqB8/j4IMfAoZO7e/MLHlGBvNlpDfRNryXLhljVD0EI8q5lejbYFMuQgwkXoU4w7WzBA9p+hXvw48mf/sDy7jLnTOc/q6Sn/W7R2hvXkFinMygV98bnao9kA=="
            , merchant_key_id   = 9999999
            }

        eerr `shouldSatisfy` isLeft
        let Left (FailureResponse _ Response { responseStatusCode = status, responseBody = body })= eerr
        status `shouldBe` status401
        body   `shouldBe` ""

----------------------------------------------------------------------

protected :: Auth.Signed Auth.SignaturePayloadExample -> FlowHandler Text
protected = S.runFlow "sTestFlow" S.noReqBodyJSON . Auth.authenticateUsingRSAEncryption handler
  where
    handler {-merchantAccount-}_ signaturePayloadExample = do
      pure $ Text.pack $ Auth.someField signaturePayloadExample

testDBName :: String
testDBName = "./test/Euler/TestData/authRSAtest.db"

testDBTemplateName :: String
testDBTemplateName = "./test/Euler/TestData/authRSAtest.db.template"

rmTestDB :: Flow ()
rmTestDB = void $ runSysCmd $ "rm -f " <> testDBName

prepareTestDB :: FlowRuntime -> IO ()
prepareTestDB rt = void $ try @_ @SomeException $ runFlow rt $ do
  rmTestDB
  void $ runSysCmd $ "cp " <> testDBTemplateName <> " " <> testDBName

type API = "protected" :> ReqBody '[JSON] (Auth.Signed Auth.SignaturePayloadExample) :> Post '[JSON] Text

api :: Proxy API
api = Proxy

port :: Int
port = 8081

protectedClient :: Auth.Signed Auth.SignaturePayloadExample -> T.EulerClient Text
protectedClient = T.client api

server :: FlowServer' API
server = protected

sqliteConn :: IsString a => a
sqliteConn = "sqlite"

keepConnsAliveForSecs :: NominalDiffTime
keepConnsAliveForSecs = 60 * 10 -- 10 mins

maxTotalConns :: Int
maxTotalConns = 8

prepareDBConnections :: Flow ()
prepareDBConnections = do
  ePool <- initSqlDBConnection
    $ T.mkSQLitePoolConfig sqliteConn testDBName
    $ T.PoolConfig 1 keepConnsAliveForSecs maxTotalConns
  L.throwOnFailedWithLog ePool T.SqlDBConnectionFailedException "Failed to connect to SQLite DB."

runServer :: IO () -> IO ()
runServer next = withFlowRuntime Nothing $ \flowRt -> do
  prepareTestDB flowRt
  try (runFlow flowRt prepareDBConnections) >>= \case
    Left (e :: SomeException) -> putStrLn @String $ "Exception thrown: " <> show e
    Right _ -> do
      let env = S.Env flowRt Nothing
      serverStartupLock <- newEmptyMVar
      let settings = setBeforeMainLoop (putMVar serverStartupLock ()) $ setPort port defaultSettings
      threadId <- forkIO $ runSettings settings $ serve api (eulerServer_ api server env)
      readMVar serverStartupLock
      finally next $ killThread threadId
