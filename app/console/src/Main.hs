module Main where

import Console.HTTPServer (app)
import Console.Config (bqProject, enableCors, httpPort, jwtSecret, loadConfig, queryConfig, redis, redisDb, redisHost, redisPort)
import qualified Database.Redis as Redis
import Dashboard.Auth.Types (AuthContext(AuthContext))
import Dashboard.Query.Backend.BigQuery (BackendConfig(BackendConfig), newBigQueryBackend)
import Dashboard.Metrics.Prometheus
import Data.Text (unpack)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors (CorsResourcePolicy(..), cors, corsRequestHeaders, simpleCorsResourcePolicy)
import qualified Network.Wai.Middleware.Prometheus as PW
import GHC.Natural (naturalToInt)
import Universum
import Web.JWT (hmacSecret)

main :: IO ()
main = do
  config  <- loadConfig
  backend <- newBigQueryBackend (bqProject config) Nothing
  rConn   <- Redis.checkedConnect . connInfo $ redis config
  let port    = naturalToInt $ httpPort config
      authCtx = AuthContext rConn (hmacSecret $ jwtSecret config)
      qConfig = queryConfig config

  run port .
    middleware (enableCors config) $
    PW.prometheus defaultPromSettings (app authCtx $ BackendConfig backend qConfig)

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
