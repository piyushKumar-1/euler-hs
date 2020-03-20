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

import Database.MySQL.Base

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

mwhen :: Monoid m => Bool -> m -> m
mwhen True  = id
mwnen False = const mempty

loadMySQLDump :: String -> T.MySQLConfig -> IO ()
loadMySQLDump path T.MySQLConfig {..} =
     void $ system $
      "mysql " <> options <> " " <> connectDatabase <> " 2> /dev/null < " <> path -- ../../init/mysqldump.sql"
  where
    options =
      intercalate " "
        [                                      "--port="     <> show connectPort
        , mwhen (not $ null connectHost    ) $ "--host="     <> connectHost
        , mwhen (not $ null connectUser    ) $ "--user="     <> connectUser
        , mwhen (not $ null connectPassword) $ "--password=" <> connectPassword
        ]

mySQLCfg :: T.MySQLConfig
mySQLCfg = T.MySQLConfig
  { connectHost     = "mysql"
  , connectPort     = 3306
  , connectUser     = "cloud"
  , connectPassword = "scape"
  , connectDatabase = "euler_test_db"
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


prepareTestDB :: IO() -> IO ()
prepareTestDB action =
  bracket (T.createMySQLConn mySQLRootCfg) close $ \rootConn -> do
    let
      dropTestDbIfExist :: IO ()
      dropTestDbIfExist = do
        query rootConn "drop database if exists euler_test_db"

      createTestDb :: IO ()
      createTestDb = do
        query rootConn "create database euler_test_db"
        query rootConn "grant all privileges on euler_test_db.* to 'cloud'@'%'"


    bracket_
      (dropTestDbIfExist >> createTestDb)
      (dropTestDbIfExist)
      (loadMySQLDump "test/Euler/TestData/rsaAuthMySQL.sql" mySQLCfg >> action)



prepareDBConnections :: Flow ()
prepareDBConnections = do
  ePool <- initSqlDBConnection
    $ T.mkMySQLConfig "eulerMysqlDB" mySQLCfg
  L.throwOnFailedWithLog ePool T.SqlDBConnectionFailedException "Failed to connect to MySQL DB."

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

    let settings = setBeforeMainLoop (putMVar serverStartupLock ()) $
          setPort port defaultSettings

    threadId <- forkIO $ runSettings settings $
      serve api (eulerServer_ api server env)

    readMVar serverStartupLock

    finally act $ killThread threadId
