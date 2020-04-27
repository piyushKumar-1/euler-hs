{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}

module Euler.Tests.API.AuthRSA where

import Euler.Tests.Common

import           Data.Time.Clock (NominalDiffTime)
import qualified Data.Text                              as Text
import qualified Data.ByteString as BS

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

import           Database.MySQL.Base
import           EulerHS.Extra.Test
import qualified Euler.Options.Options as Opt

testDBName :: String
testDBName = "auth_rsa_test_db"

spec :: Spec
spec = do
  let prepare next =
        withMysqlDb testDBName "test/Euler/TestData/rsaAuthMySQL.sql" mySQLRootCfg $
          withFlowRuntime Nothing $ \rt -> flip runServer rt $ do
            runFlow rt $ prepareDBConnections
            next rt

  describe "Authentication API" $ do
    around prepare $
      describe "Authentication API, server with DB" $ do
        it "Authentication works" $ \rt -> do
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


        it "If malformed signature should return 401" $ \rt -> do
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

        it "If malformed signature_payload should return 401" $ \rt -> do
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

        it "If wrong merchant_key_id should return 401 without exposing details" $ \rt -> do
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

        it "If unable to fetch without exposing details" $ \rt -> do
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
          body   `shouldBe` "{\n    \"userMessage\": \"Internal Server Error\",\n    \"error\": true,\n    \"error_message\": \"Internal Server Error\"\n}"

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


mySQLCfg :: T.MySQLConfig
mySQLCfg = T.MySQLConfig
  { connectHost     = "mysql"
  , connectPort     = 3306
  , connectUser     = "cloud"
  , connectPassword = "scape"
  , connectDatabase = testDBName
  , connectOptions  = [T.CharsetName "utf8"]
  , connectPath     = ""
  , connectSSL      = Nothing
  }

mySQLRootCfg :: T.MySQLConfig
mySQLRootCfg =
    T.MySQLConfig
      { connectUser     = "root"
      , connectPassword = "root"
      , connectDatabase = ""
      , ..
      }
  where
    T.MySQLConfig {..} = mySQLCfg


prepareDBConnections :: Flow ()
prepareDBConnections = do
  let cfg = T.mkMySQLConfig "eulerMysqlDB" mySQLCfg

  ePool <- initSqlDBConnection cfg
  setOption Opt.EulerDbCfg cfg

  L.throwOnFailedWithLog ePool T.SqlDBConnectionFailedException "Failed to connect to MySQL DB."

runServer :: IO () -> FlowRuntime -> IO ()
runServer act flowRt = do
    let env = S.Env flowRt Nothing mkAppEnv
    serverStartupLock <- newEmptyMVar

    let settings = setBeforeMainLoop (putMVar serverStartupLock ()) $
          setPort port defaultSettings

    threadId <- forkIO $ runSettings settings $
      serve api (eulerServer_ api server env)

    readMVar serverStartupLock

    finally act $ killThread threadId
