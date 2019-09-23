module Test.Fixtures where

import Universum

import Dashboard.Query.Backend.BigQuery (newBigQueryBackend)
import Dashboard.Query.Types
import qualified Control.Concurrent as C
import qualified Network.Wai.Handler.Warp as Warp

import Console.API (app)

testPort = 8084

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

withConsoleServer :: IO () -> IO ()
withConsoleServer action = do
  backend <- newBigQueryBackend "godel-big-q"
  bracket (liftIO $ C.forkIO $ Warp.run testPort $ app backend ecQueryConf)
    C.killThread
    (const action)
