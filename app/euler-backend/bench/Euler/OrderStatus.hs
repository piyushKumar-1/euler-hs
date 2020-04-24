module Euler.OrderStatus
       (
         execOrderStatusBench
       ) where

import           EulerHS.Prelude

import           Euler.API.RouteParameters
import qualified Euler.Common.Types as C
import qualified Euler.Options.Options as Opt
import           EulerHS.Interpreters
import           EulerHS.Language
import           EulerHS.Runtime
import qualified EulerHS.Types as T

import           Euler.AppEnv

import qualified Euler.Product.OLTP.Services.AuthConf   as AuthConf

import qualified Euler.Product.OLTP.Order.StatusApi as StatusApi
import qualified Euler.Product.Cache.OrderStatusCacheApi as CacheApi

import qualified Euler.Product.Cache.OrderStatusCacheImpl as CacheImpl
import qualified Euler.Product.OLTP.Order.OrderStatus   as OrderStatusImpl
import qualified Euler.Product.OLTP.Order.Update   as UpdateImpl

import qualified WebService.Language as L
import qualified WebService.Types as T

import           Criterion.Main
import qualified Data.Text as T
import           Database.MySQL.Base
import           System.Process hiding (env)


{-
We head slight overhead on async way.
Async should win on longer flows.

benchmarking orderId: 1475240639/sequentially
time                 54.16 ms   (48.12 ms .. 58.44 ms)
                     0.982 R²   (0.961 R² .. 0.998 R²)
mean                 54.59 ms   (52.74 ms .. 56.24 ms)
std dev              3.168 ms   (2.293 ms .. 4.237 ms)
variance introduced by outliers: 15% (moderately inflated)

benchmarking orderId: 1475240639/concurrent
time                 62.60 ms   (58.30 ms .. 66.88 ms)
                     0.990 R²   (0.973 R² .. 0.997 R²)
mean                 62.75 ms   (60.96 ms .. 65.07 ms)
std dev              3.730 ms   (2.827 ms .. 4.967 ms)
variance introduced by outliers: 16% (moderately inflated)

benchmarking orderId: 1346337820/sequentially
time                 30.35 ms   (28.97 ms .. 31.71 ms)
                     0.993 R²   (0.987 R² .. 0.997 R²)
mean                 29.94 ms   (28.73 ms .. 33.01 ms)
std dev              3.811 ms   (1.622 ms .. 6.697 ms)
variance introduced by outliers: 51% (severely inflated)

benchmarking orderId: 1346337820/concurrent
time                 33.47 ms   (31.90 ms .. 34.97 ms)
                     0.989 R²   (0.979 R² .. 0.997 R²)
mean                 32.22 ms   (30.57 ms .. 33.26 ms)
std dev              2.721 ms   (1.533 ms .. 3.983 ms)
variance introduced by outliers: 34% (moderately inflated)

benchmarking orderId: 1346336130/sequentially
time                 29.76 ms   (28.14 ms .. 31.79 ms)
                     0.987 R²   (0.975 R² .. 0.997 R²)
mean                 28.81 ms   (28.11 ms .. 29.52 ms)
std dev              1.760 ms   (1.231 ms .. 2.376 ms)
variance introduced by outliers: 21% (moderately inflated)

benchmarking orderId: 1346336130/concurrent
time                 34.12 ms   (32.36 ms .. 35.79 ms)
                     0.990 R²   (0.984 R² .. 0.996 R²)
mean                 34.17 ms   (33.16 ms .. 35.08 ms)
std dev              2.133 ms   (1.694 ms .. 3.029 ms)
variance introduced by outliers: 23% (moderately inflated)


-}

execOrderStatusBench :: IO ()
execOrderStatusBench = prepareDB $ \rt -> do
  defaultMain [
    bgroup (T.unpack  $ "orderId: " <> ordId1)
        [ bench "sequentially" $ whnfIO $ runOrderStatus rt rps1 False
        , bench "concurrent" $ whnfIO $ runOrderStatus rt rps1 True
        ],
    bgroup (T.unpack  $ "orderId: " <> ordId2)
        [ bench "sequentially" $  whnfIO $ runOrderStatus rt rps2 False
        , bench "concurrent" $ whnfIO $ runOrderStatus rt rps2 True
        ],
    bgroup (T.unpack  $ "orderId: " <> ordId3)
        [ bench "sequentially" $ whnfIO $ runOrderStatus rt rps3 False
        , bench "concurrent" $ whnfIO $ runOrderStatus rt rps3 True
        ]
    ]


runOrderStatus :: FlowRuntime -> RouteParameters -> Bool -> IO StatusApi.OrderStatusResponse
runOrderStatus rt rps isAsync = runFlow rt $ orderStatusMethod (mkAppEnv isAsync) rps T.emptyReq

mkAppEnv :: Bool -> AppEnv
mkAppEnv isAsync  =
  let keyAuthService = AuthConf.mkKeyAuthService
      keyTokenAuthService = AuthConf.mkKeyTokenAuthService
      orderStatusCacheService = CacheImpl.mkHandle CacheApi.defaultConfig
      orderStatusConfig = StatusApi.defaultConfig { StatusApi.asyncSlowPath = isAsync }
      orderStatusService = OrderStatusImpl.mkHandle orderStatusConfig keyTokenAuthService orderStatusCacheService
  in AppEnv
    { keyAuthH = keyAuthService
    , tokenAuthH = AuthConf.mkTokenAuthService
    , keyTokenAuthH = keyTokenAuthService
    , orderStatusCacheH = orderStatusCacheService
    , orderStatusH = orderStatusService
    , orderUpdateH = UpdateImpl.mkHandle $ UpdateImpl.IHandle keyAuthService orderStatusCacheService orderStatusService
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


ordId2 :: C.OrderId
ordId2 = "1346337820"

rps2 :: RouteParameters
rps2 = collectRPs
  (OrderId ordId2)
  -- Use https://www.base64encode.org/ to get base64 from api_key
  (Authorization "BASIC QzYyODExOEY4ODYwNEY3M0IxODUzNTM5MEY5NkVEREU=")
  (Version "2017-07-01")
  (UserAgent "Uagent")


ordId3 :: C.OrderId
ordId3 = "1346336130"

rps3 :: RouteParameters
rps3 = collectRPs
  (OrderId ordId3)
  -- Use https://www.base64encode.org/ to get base64 from api_key
  (Authorization "BASIC QUVDMTlFQzQzODU1NDQ3MEEyMzNFRkFFNjY4RjMwMTc=")
  (Version "2017-07-01")
  (UserAgent "Uagent")








-- DB cooking

prepareDB :: (FlowRuntime -> IO ()) -> IO()
prepareDB next = withFlowRuntime (Just T.nullLoger) $ \flowRt ->
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
