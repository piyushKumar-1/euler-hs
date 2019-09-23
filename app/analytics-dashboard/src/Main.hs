module Main where

import EulerHS.Prelude

import Console.API (app)
import Dashboard.Query.Types
import Dashboard.Query.Backend.BigQuery (newBigQueryBackend)
import Network.Wai.Handler.Warp (run)

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
  backend <- newBigQueryBackend "godel-big-q"
  run 8080 $ app backend ecQueryConf
