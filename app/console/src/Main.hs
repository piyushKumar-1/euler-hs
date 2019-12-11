module Main where

import Console.HTTPServer (app)
import Console.Config (enableCors, httpPort, loadConfig)
import Dashboard.Query.Types
import Dashboard.Query.Backend.BigQuery (newBigQueryBackend)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors (CorsResourcePolicy(..), cors, corsRequestHeaders, simpleCorsResourcePolicy)
import GHC.Natural (naturalToInt)
import Universum

ecQueryConf :: QueryConfiguration
ecQueryConf = QueryConfiguration [ ( "godel-big-q.express_checkout.express_checkout20190927"
                                   , TableConfiguration [ ("amount", FloatType)
                                                        , ("card_issuer_bank_name", StringType)
                                                        , ("card_type", StringType)
                                                        , ("gateway", StringType)
                                                        , ("merchant_id", StringType)
                                                        , ("order_last_modified", StringType)
                                                        , ("order_status", StringType)
                                                        , ("payment_method", StringType)
                                                        , ("payment_method_type", StringType)
                                                        ]
                                   )
                                 ]

main :: IO ()
main = do
  backend <- newBigQueryBackend "godel-big-q" Nothing
  config  <- loadConfig
  run (naturalToInt . httpPort $ config) .
    middleware (enableCors config) $
    app backend ecQueryConf

  where
    middleware enableCors = if enableCors
                               then cors . const . Just $ corsPolicy
                               else id
    -- Allow '*'
    corsPolicy = simpleCorsResourcePolicy { corsRequestHeaders = [ "Content-Type" ] }
