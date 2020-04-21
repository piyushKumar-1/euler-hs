module Euler.OrderStatus
       (
         execOrderStatusBench
       ) where

import           EulerHS.Prelude

import           Euler.API.RouteParameters
import qualified Euler.API.RouteParameters as RP
import qualified Euler.Common.Types as C
import           Euler.Lens
import qualified Euler.Options.Options as Opt
import qualified Euler.Product.Domain as D
import           Euler.Product.OLTP.Order.OrderStatus
import           Euler.Product.OLTP.Services.AuthenticationService (authenticateRequest)
import           EulerHS.Interpreters
import           EulerHS.Language
import           EulerHS.Runtime
import           EulerHS.Types hiding (error)
import qualified EulerHS.Types as T

import qualified WebService.Language as L
import qualified WebService.Types as T

import           Criterion.Main
import qualified Data.Text as T
import           Database.MySQL.Base
import           System.Process hiding (env)


{-
We head slight overhead on async way.
Async should win on longer flows.

benchmarking execOrderStatusAsync, concurrent, whnfAppIO
time                 57.22 ms   (54.94 ms .. 59.34 ms)
                     0.996 R²   (0.991 R² .. 0.999 R²)
mean                 56.51 ms   (55.35 ms .. 57.40 ms)
std dev              2.002 ms   (1.382 ms .. 2.944 ms)

benchmarking execOrderStatus, sequentially, whnfAppIO
time                 52.82 ms   (51.11 ms .. 57.01 ms)
                     0.993 R²   (0.987 R² .. 0.998 R²)
mean                 48.55 ms   (46.38 ms .. 49.96 ms)
std dev              3.463 ms   (2.621 ms .. 4.846 ms)
variance introduced by outliers: 22% (moderately inflated)

benchmarking execOrderStatusAsync, concurrent, whnfIO
time                 60.41 ms   (58.26 ms .. 63.47 ms)
                     0.993 R²   (0.983 R² .. 0.998 R²)
mean                 55.88 ms   (54.05 ms .. 58.06 ms)
std dev              3.829 ms   (2.783 ms .. 5.061 ms)
variance introduced by outliers: 23% (moderately inflated)

benchmarking execOrderStatus, sequentially, whnfIO
time                 48.51 ms   (46.50 ms .. 51.06 ms)
                     0.993 R²   (0.980 R² .. 0.999 R²)
mean                 45.97 ms   (45.04 ms .. 47.02 ms)
std dev              2.092 ms   (1.612 ms .. 2.870 ms)
variance introduced by outliers: 13% (moderately inflated)

-}

execOrderStatusBench :: IO ()
execOrderStatusBench =
    prepareDB $ \rt -> do
      print "Before requests getting"
      request1 <- getRequest1 rt
      request2 <- getRequest2 rt
      request3 <- getRequest3 rt

      print "All requests was got"
      defaultMain [
        bgroup (T.unpack  $ "orderId: " <> ordId1)
            [ bench "concurrent" $ whnfAppIO (runFlow rt . execOrderStatusAsync) request1
            , bench "sequentially" $ whnfAppIO (runFlow rt . execOrderStatus) request1
            ],
        bgroup (T.unpack  $ "orderId: " <> ordId2)
            [ bench "concurrent" $ whnfAppIO (runFlow rt . execOrderStatusAsync) request2
            , bench "sequentially" $ whnfAppIO (runFlow rt . execOrderStatus) request2
            ],
        bgroup (T.unpack  $ "orderId: " <> ordId3)
            [ bench "concurrent" $ whnfAppIO (runFlow rt . execOrderStatusAsync) request3
            , bench "sequentially" $ whnfAppIO (runFlow rt . execOrderStatus) request3
            ]
        ]

getRequest1 :: FlowRuntime -> IO D.OrderStatusRequest
getRequest1 rt = do
  merchantAccount <- runFlow rt $ authenticateRequest rps1
  pure D.OrderStatusRequest
    { orderId                 = ordId1
    , merchantId              = merchantAccount ^. _merchantId
    , merchantReturnUrl       = merchantAccount ^. _returnUrl
    , resellerId              = merchantAccount ^. _resellerId
    , isAuthenticated         = True
    , sendCardIsin            = fromMaybe False $ merchantAccount ^. _enableSendingCardIsin
    , sendFullGatewayResponse = RP.sendFullPgr rps1
    , sendAuthToken           = False
    , version                 = RP.lookupRP @RP.Version rps1
    , isAsync                 = True
    }

ordId1 :: C.OrderId
ordId1 = "1475240639"

rps1 :: RouteParameters
rps1 = collectRPs
  (OrderId ordId1)
  -- Use https://www.base64encode.org/ to get base64 from api_key
  (Authorization "BASIC RTI5QTgzOEU0Qjc2NDM2RThBMkM2NjBBMDYwOTlGRUU=")
  (Version "2017-07-01")
  (UserAgent "Uagent")



getRequest2 :: FlowRuntime -> IO D.OrderStatusRequest
getRequest2 rt = do
  merchantAccount <- runFlow rt $ authenticateRequest rps2
  pure D.OrderStatusRequest
    { orderId                 = ordId2
    , merchantId              = merchantAccount ^. _merchantId
    , merchantReturnUrl       = merchantAccount ^. _returnUrl
    , resellerId              = merchantAccount ^. _resellerId
    , isAuthenticated         = True
    , sendCardIsin            = fromMaybe False $ merchantAccount ^. _enableSendingCardIsin
    , sendFullGatewayResponse = RP.sendFullPgr rps2
    , sendAuthToken           = False
    , version                 = RP.lookupRP @RP.Version rps2
    , isAsync                 = True
    }

ordId2 :: C.OrderId
ordId2 = "1346337820"

rps2 :: RouteParameters
rps2 = collectRPs
  (OrderId ordId2)
  -- Use https://www.base64encode.org/ to get base64 from api_key
  (Authorization "BASIC QzYyODExOEY4ODYwNEY3M0IxODUzNTM5MEY5NkVEREU=")
  (Version "2017-07-01")
  (UserAgent "Uagent")


getRequest3 :: FlowRuntime -> IO D.OrderStatusRequest
getRequest3 rt = do
  merchantAccount <- runFlow rt $ authenticateRequest rps3
  pure D.OrderStatusRequest
    { orderId                 = ordId3
    , merchantId              = merchantAccount ^. _merchantId
    , merchantReturnUrl       = merchantAccount ^. _returnUrl
    , resellerId              = merchantAccount ^. _resellerId
    , isAuthenticated         = True
    , sendCardIsin            = fromMaybe False $ merchantAccount ^. _enableSendingCardIsin
    , sendFullGatewayResponse = RP.sendFullPgr rps3
    , sendAuthToken           = False
    , version                 = RP.lookupRP @RP.Version rps3
    , isAsync                 = True
    }

ordId3 :: C.OrderId
ordId3 = "1346336130"

rps3 :: RouteParameters
rps3 = collectRPs
  (OrderId ordId3)
  -- Use https://www.base64encode.org/ to get base64 from api_key
  (Authorization "BASIC QUVDMTlFQzQzODU1NDQ3MEEyMzNFRkFFNjY4RjMwMTc=")
  (Version "2017-07-01")
  (UserAgent "Uagent")










prepareDB :: (FlowRuntime -> IO ()) -> IO()
prepareDB next = withFlowRuntime (Just nullLoger) $ \flowRt ->
-- prepareDB next = withFlowRuntime (Just defaultLoggerConfig) $ \flowRt ->
  prepareTestDB $
    try (runFlow flowRt prepareDBConnections) >>= \case
      Left (e :: SomeException) -> putStrLn @String $ "Exception thrown: " <> show e
      Right _ -> next flowRt
  where
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
          (loadMySQLDump "test/Euler/TestData/orderCreateMySQL.sql" mySQLCfg >> action)

-- Redis config data
redisConn :: IsString a => a
redisConn = "redis"

redisConnConfig :: T.RedisConfig
redisConnConfig = T.RedisConfig
    { connectHost           = "redis"
    , connectPort           = 6379
    , connectAuth           = Nothing
    , connectDatabase       = 0
    , connectMaxConnections = 50
    , connectMaxIdleTime    = 30
    , connectTimeout        = Nothing
    }

prepareDBConnections :: Flow ()
prepareDBConnections = do
  let cfg = T.mkMySQLConfig "eulerMysqlDB" mySQLCfg

  ePool <- initSqlDBConnection cfg
  setOption Opt.EulerDbCfg cfg

  redis <- initKVDBConnection
    $ T.mkKVDBConfig redisConn $ redisConnConfig

  L.throwOnFailedWithLog ePool T.SqlDBConnectionFailedException "Failed to connect to SQLite DB."
  L.throwOnFailedWithLog redis T.KVDBConnectionFailedException "Failed to connect to Redis DB."


mwhen :: Monoid m => Bool -> m -> m
mwhen True a  = a
mwnen False _ = mempty


-- file path relative to app/euler-backend
loadMySQLDump :: String -> T.MySQLConfig -> IO ()
loadMySQLDump path T.MySQLConfig {..} = do
    let cmd = "mysql " <> options <> " " <> connectDatabase <> " 2> /dev/null < " <> path -- ../../init/mysqldump.sql"
    -- putStrLn cmd
    void $ system cmd
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
  { connectHost     = "127.0.0.1"
  , connectPort     = 3306
  , connectUser     = "cloud"
  , connectPassword = "scape"
  , connectDatabase = "jdb"
  , connectOptions  = [T.CharsetName "utf8"]
  , connectPath     = ""
  , connectSSL      = Nothing
  }

mySQLRootCfg :: T.MySQLConfig
mySQLRootCfg =
    T.MySQLConfig
      { connectUser     = "root"
      , connectPassword = "4" -- set your root pass here
      , connectDatabase = ""
      , ..
      }
  where
    T.MySQLConfig {..} = mySQLCfg
