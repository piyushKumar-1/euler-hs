module App where

import EulerHS.Prelude

import           Network.Wai.Handler.Warp (Settings, runSettings, setPort, defaultSettings)
import           Data.Time.Clock (NominalDiffTime)

import qualified Euler.Config.Config as Config
import qualified Euler.Constants as Constants
import qualified Euler.Server as Euler
import qualified EulerHS.Runtime as R
import qualified EulerHS.Interpreters as R
import qualified EulerHS.Language as L
import qualified EulerHS.Types as T
import qualified Euler.Playback.Service as PB
import qualified WebService.Language as L
import qualified WebService.Types as T


keepConnsAliveForSecs :: NominalDiffTime
keepConnsAliveForSecs = 60 * 10 -- 10 mins

maxTotalConns :: Int
maxTotalConns = 8

sqliteConn :: IsString a => a
sqliteConn = "sqlite"

mySQLCfg :: T.MySQLConfig
mySQLCfg = T.MySQLConfig
  { connectHost     = "localhost"
  , connectPort     = 3306
  , connectUser     = "cloud"
  , connectPassword = "scape"
  , connectDatabase = "jdb"
  , connectOptions  = [T.CharsetName "utf8"]
  , connectPath     = ""
  , connectSSL      = Nothing
  }

eulerApiPort :: Int
eulerApiPort = 8080


-- Test DB to show how the initialization can look like.
-- TODO: add real DB services.
prepareDBConnections :: L.Flow ()
prepareDBConnections = do
  ePool <- L.initSqlDBConnection
    $ T.mkSQLitePoolConfig sqliteConn "/tmp/test.db" -- T.mkMySQLPoolConfig "eulerMysqlDB" mySQLCfg --
    $ T.PoolConfig 1 keepConnsAliveForSecs maxTotalConns
  ecRedis <- L.initKVDBConnection
    $ T.mkKVDBConfig Constants.ecRedis
    $ Config.redisConfig
  kvRedis <- L.initKVDBConnection
    $ T.mkKVDBClusterConfig Constants.kvRedis
    $ Config.redisClusterConfig
  L.throwOnFailedWithLog ePool T.SqlDBConnectionFailedException "Failed to connect to SQLite DB."
  L.throwOnFailedWithLog ecRedis T.KVDBConnectionFailedException "Failed to connect to Redis DB."
  L.throwOnFailedWithLog kvRedis T.KVDBConnectionFailedException "Failed to connect to Redis cluster DB."



runEulerBackendApp' :: Settings -> IO ()
runEulerBackendApp' settings = do
  R.withFlowRuntime (Just Config.loggerConfig) $ \flowRt -> do
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
