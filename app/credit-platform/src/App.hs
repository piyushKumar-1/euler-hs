module App where

import EulerHS.Prelude

import           Network.Wai.Handler.Warp (run)
import           Data.Time.Clock (NominalDiffTime)

import qualified CreditPlatform.Server as CP
import qualified EulerHS.Runtime as R
import qualified EulerHS.Interpreters as R
import qualified EulerHS.Language as L
import qualified EulerHS.Types as T
import qualified WebService.Language as L
import qualified WebService.Types as T


keepConnsAliveForSecs :: NominalDiffTime
keepConnsAliveForSecs = 60 * 10 -- 10 mins

maxTotalConns :: Int
maxTotalConns = 8

sqliteConn :: IsString a => a
sqliteConn = "sqlite"

-- Test DB to show how the initialization can look like.
-- TODO: add real DB services.
prepareDBConnections :: L.Flow ()
prepareDBConnections = do
  ePool <- L.initSqlDBConnection
    $ T.mkSQLitePoolConfig sqliteConn "tmp/test.db"
    $ T.PoolConfig 1 keepConnsAliveForSecs maxTotalConns
  L.throwOnFailedWithLog ePool T.SqlDBConnectionFailedException "Failed to connect to SQLite DB."

prepareDBConnections' :: L.Flow ()
prepareDBConnections' = pure ()


runCreditPlatformApp :: IO ()
runCreditPlatformApp = do
  let port = 8080
  let loggerCfg = T.defaultLoggerConfig
        { T._logToFile = True
        , T._logFilePath = "/tmp/euler-hs.log"
        , T._isAsync = True
        }

  R.withFlowRuntime (Just loggerCfg) $ \flowRt -> do
    putStrLn @String "Runtime created."
    putStrLn @String "Initializing DB connections..."
    try (R.runFlow flowRt prepareDBConnections') >>= \case
      Left (e :: SomeException) -> putStrLn @String $ "Exception thrown: " <> show e
      Right _ -> do
        putStrLn @String "Starting web server..."
        run port $ CP.creditPlatformApp $ CP.Env flowRt
