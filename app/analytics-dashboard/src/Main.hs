module Main where

import EulerHS.Prelude

import Console.API (app)
import Console.Env (allowCors)
import Dashboard.Query.Types
import Dashboard.Query.Backend.BigQuery (newBigQueryBackend)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors (CorsResourcePolicy(..), cors, corsRequestHeaders, simpleCorsResourcePolicy)

ecQueryConf :: QueryConfiguration
ecQueryConf = QueryConfiguration [ ( "godel-big-q.express_checkout.express_checkout20190927"
                                   , TableConfiguration [ ("amount", FloatType)
                                                        , ("gateway", StringType)
                                                        , ("card_type", StringType)
                                                        , ("merchant_id", StringType)
                                                        , ("order_last_modified", StringType)
                                                        ]
                                   )
                                 ]

main :: IO ()
main = do
  backend  <- newBigQueryBackend "godel-big-q"
  wantCors <- allowCors
  run 8080 .
    middleware wantCors $
    app backend ecQueryConf

  where
    middleware wantCors = if wantCors
                          then cors . const . Just $ corsPolicy
                          else id
    -- Allow '*'
    corsPolicy = simpleCorsResourcePolicy { corsRequestHeaders = [ "Content-Type" ] }
