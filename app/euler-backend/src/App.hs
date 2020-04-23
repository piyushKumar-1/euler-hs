module App
  ( runEulerBackendApp
  , runEulerBackendApp'
  )where

import EulerHS.Prelude

import qualified EulerHS.Runtime as R
import qualified EulerHS.Interpreters as R
import qualified EulerHS.Language as L
import qualified EulerHS.Types as T
import qualified WebService.Language as L
import qualified WebService.Types as T

import           Network.Wai.Handler.Warp (Settings, runSettings, setPort, defaultSettings)

import qualified Euler.Config.Config as Config
import qualified Euler.Config.EnvVars as EnvVars
import qualified Euler.Options.Options as Opt
import qualified Euler.Server as Euler
import qualified Euler.Playback.Service as PB

import           Euler.AppEnv

import qualified Euler.Product.OLTP.Services.AuthConf   as AuthConf

import qualified Euler.Product.OLTP.Order.StatusApi as StatusApi
import qualified Euler.Product.Cache.OrderStatusCacheApi as CacheApi

import qualified Euler.Product.Cache.OrderStatusCacheImpl as CacheImpl
import qualified Euler.Product.OLTP.Order.OrderStatus   as OrderStatusImpl
import qualified Euler.Product.OLTP.Order.Update   as UpdateImpl



eulerApiPort :: Int
eulerApiPort = 8080

-- Test DB to show how the initialization can look like.
-- TODO: add real DB services.
prepareDBConnections :: L.Flow ()
prepareDBConnections = do
  mysqlCfg <- L.runIO Config.mysqlDBC
  ePool <- L.initSqlDBConnection mysqlCfg
  L.setOption Opt.EulerDbCfg mysqlCfg
  redis <- L.initKVDBConnection Config.kvdbConfig
  L.throwOnFailedWithLog ePool T.SqlDBConnectionFailedException "Failed to connect to SQLite DB."
  L.throwOnFailedWithLog redis T.KVDBConnectionFailedException "Failed to connect to Redis DB."


runEulerBackendApp' :: Settings -> IO ()
runEulerBackendApp' settings = do
  let loggerCfg = T.defaultLoggerConfig
        { T._logToFile = True
        , T._logFilePath = "/tmp/euler-backend.log"
        , T._isAsync = False
        , T._level = EnvVars.loggerLevel
        }
  R.withFlowRuntime (Just loggerCfg) $ \flowRt -> do
    putStrLn @String "Runtime created."
    putStrLn @String "Initializing DB connections..."
    try (R.runFlow flowRt prepareDBConnections) >>= \case
      Left (e :: SomeException) -> putStrLn @String $ "Exception thrown: " <> show e
      Right _ -> do
        putStrLn @String "Initializing ART..."
        -- EHS: recorder and player functions are in the same module
        recorderParams <- PB.initRecorderParams
        putStrLn @String "Building business logic configuration..."
        let env = Euler.Env flowRt recorderParams mkAppEnv
        putStrLn @String "Starting web server..."
        runSettings settings $ Euler.eulerBackendApp env

runEulerBackendApp :: IO ()
runEulerBackendApp = runEulerBackendApp' $ setPort eulerApiPort defaultSettings

-- | Normal mode business-logic configuration
mkAppEnv :: AppEnv
mkAppEnv =
  let keyAuthService = AuthConf.mkKeyAuthService
      keyTokenAuthService = AuthConf.mkKeyTokenAuthService
      orderStatusCacheService = CacheImpl.mkHandle CacheApi.defaultConfig
      orderStatusConfig = StatusApi.defaultConfig { StatusApi.asyncSlowPath = EnvVars.orderStatusAsyncSlowPath }
      orderStatusService = OrderStatusImpl.mkHandle orderStatusConfig keyTokenAuthService orderStatusCacheService
  in AppEnv
    { keyAuthH = keyAuthService
    , tokenAuthH = AuthConf.mkTokenAuthService
    , keyTokenAuthH = keyTokenAuthService
    , orderStatusCacheH = orderStatusCacheService
    , orderStatusH = orderStatusService
    , orderUpdateH = UpdateImpl.mkHandle $ UpdateImpl.IHandle keyAuthService orderStatusCacheService orderStatusService
    }