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
      request <- getRequest rt
      defaultMain
            [ bench "execOrderStatusAsync, concurrent, whnfAppIO" $ whnfAppIO (runFlow rt . execOrderStatusAsync) request
            , bench "execOrderStatus, sequentially, whnfAppIO" $ whnfAppIO (runFlow rt . execOrderStatus) request
            , bench "execOrderStatusAsync, concurrent, whnfIO" $ whnfIO (runFlow rt $ execOrderStatusAsync request)
            , bench "execOrderStatus, sequentially, whnfIO" $ whnfIO (runFlow rt $ execOrderStatus request)
            ]


getRequest :: FlowRuntime -> IO D.OrderStatusRequest
getRequest rt = do
  merchantAccount <- runFlow rt $ authenticateRequest rps
  pure D.OrderStatusRequest
    { orderId                 = ordId
    , merchantId              = merchantAccount ^. _merchantId
    , merchantReturnUrl       = merchantAccount ^. _returnUrl
    , resellerId              = merchantAccount ^. _resellerId
    , isAuthenticated         = True
    , sendCardIsin            = fromMaybe False $ merchantAccount ^. _enableSendingCardIsin
    , sendFullGatewayResponse = RP.sendFullPgr rps
    , sendAuthToken           = False
    , version                 = RP.lookupRP @RP.Version rps
    , isAsync                 = True
    }

ordId :: C.OrderId
ordId = "1475240639"

rps :: RouteParameters
rps = collectRPs
  (OrderId "1475240639")
  -- Use https://www.base64encode.org/ to get base64 from api_key
  (Authorization "BASIC RTI5QTgzOEU0Qjc2NDM2RThBMkM2NjBBMDYwOTlGRUU=")
  (Version "2017-07-01")
  (UserAgent "Uagent")



prepareDB :: (FlowRuntime -> IO ()) -> IO()
prepareDB next = withFlowRuntime (Just nullLoger) $ \flowRt ->
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
