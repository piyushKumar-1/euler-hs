module Main where

import Console.HTTPServer (app)
import Console.Config (enableCors, httpPort, jwtSecret, loadConfig, redis, redisDb, redisHost, redisPort)
import qualified Database.Redis as Redis
import Dashboard.Auth.Types (AuthContext(AuthContext))
import Dashboard.Query.Config
import Dashboard.Query.Backend.BigQuery (newBigQueryBackend)
import Data.Text (unpack)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors (CorsResourcePolicy(..), cors, corsRequestHeaders, simpleCorsResourcePolicy)
import GHC.Natural (naturalToInt)
import Universum
import Web.JWT (hmacSecret)

ecQueryConf :: QueryConfiguration
ecQueryConf =
  QueryConfiguration
    [ TableConfiguration
        "godel-big-q.express_checkout.express_checkout20190927"
        [ FieldConfiguration "amount" FloatType
        , FieldConfiguration "card_issuer_bank_name" StringType
        , FieldConfiguration "card_type" StringType
        , FieldConfiguration "gateway" StringType
        , FieldConfiguration "merchant_id" StringType
        , FieldConfiguration "order_last_modified" StringType
        , FieldConfiguration "order_status" StringType
        , FieldConfiguration "payment_method" StringType
        , FieldConfiguration "payment_method_type" StringType
        ]
    ]

main :: IO ()
main = do
  backend <- newBigQueryBackend "godel-big-q" Nothing
  config  <- loadConfig
  rConn   <- Redis.checkedConnect . connInfo $ redis config
  let port    = naturalToInt $ httpPort config
      authCtx = AuthContext rConn (hmacSecret $ jwtSecret config)

  run port .
    middleware (enableCors config) $
    app authCtx backend ecQueryConf

  where
    connInfo rConfig = Redis.defaultConnectInfo
      { Redis.connectHost = unpack $ redisHost rConfig
      , Redis.connectPort = Redis.PortNumber . fromIntegral $ redisPort rConfig
      , Redis.connectDatabase = fromIntegral $ redisDb rConfig
      }

    middleware enableCors = if enableCors
                               then cors . const . Just $ corsPolicy
                               else id
    -- Allow '*'
    corsPolicy = simpleCorsResourcePolicy { corsRequestHeaders = [ "Content-Type" ] }
