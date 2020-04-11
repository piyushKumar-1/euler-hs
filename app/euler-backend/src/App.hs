module App where

import EulerHS.Prelude

import           Network.Wai.Handler.Warp (Settings, runSettings, setPort, defaultSettings)
import           Data.Time.Clock (NominalDiffTime)

import qualified Euler.Config.Config as Config
import qualified Euler.Storage.DBConfig as DBC
import qualified Euler.Options.Options as Opt
import qualified Euler.Server as Euler
import qualified EulerHS.Runtime as R
import qualified EulerHS.Interpreters as R
import qualified EulerHS.Language as L
import qualified EulerHS.Types as T
import qualified Euler.Playback.Service as PB
import qualified WebService.Language as L
import qualified WebService.Types as T


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
        }
  -- MERGE: bl2: Config.loggerConfig
  R.withFlowRuntime (Just loggerCfg) $ \flowRt -> do
    putStrLn @String "Runtime created."
    putStrLn @String "Initializing DB connections..."
    try (R.runFlow flowRt prepareDBConnections) >>= \case
      Left (e :: SomeException) -> putStrLn @String $ "Exception thrown: " <> show e
      Right _ -> do
        putStrLn @String "Initializing ART..."
        recorderParams <- PB.initRecorderParams
        putStrLn @String "Starting web server..."
        let env = Euler.Env flowRt recorderParams
        runSettings settings $ Euler.eulerBackendApp env

runEulerBackendApp :: IO ()
runEulerBackendApp = runEulerBackendApp' $ setPort eulerApiPort defaultSettings
