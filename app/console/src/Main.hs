module Main where

import Console.HTTPServer (app)
import Console.Config (enableCors, httpPort, jwtSecret, loadConfig, queryConfig, redis, redisDb, redisHost, redisPort)
import qualified Database.Redis as Redis
import Dashboard.Auth.Types (AuthContext(AuthContext))
import Dashboard.Query.Backend.BigQuery (newBigQueryBackend)
import Data.Text (unpack)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors (CorsResourcePolicy(..), cors, corsRequestHeaders, simpleCorsResourcePolicy)
import GHC.Natural (naturalToInt)
import Universum
import Web.JWT (hmacSecret)

main :: IO ()
main = do
  backend <- newBigQueryBackend "godel-big-q" Nothing
  config  <- loadConfig
  rConn   <- Redis.checkedConnect . connInfo $ redis config
  let port    = naturalToInt $ httpPort config
      authCtx = AuthContext rConn (hmacSecret $ jwtSecret config)
      qConfig = queryConfig config

  run port .
    middleware (enableCors config) $
    app authCtx backend qConfig

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
