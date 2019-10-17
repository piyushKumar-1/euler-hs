module EulerHS.Framework.Runtime where

import           EulerHS.Prelude
import           Data.Map            (Map, empty)
import           Network.HTTP.Client (Manager, newManager, defaultManagerSettings)

import qualified Data.Pool              as DP
import qualified Database.SQLite.Simple as SQLite
import qualified Database.MySQL.Base as MySQL
import qualified Database.Beam.Sqlite as BS
-- import qualified Database.Redis as RD (Connection)
import qualified EulerHS.Core.Runtime as R
import qualified EulerHS.Core.Types as T
import           EulerHS.Framework.Types ()
import qualified Data.Map as Map

import qualified Database.MySQL.Base             as MySQL
import qualified Database.Beam.Sqlite.Connection as SQLite
import qualified Database.Beam.Postgres          as BP
-- data Connection = Redis RD.Connection

type DBName = ByteString



data FlowRuntime = FlowRuntime
  { _coreRuntime :: R.CoreRuntime
  , _httpClientManager :: MVar Manager
  , _options :: MVar (Map ByteString ByteString)
  , _connections :: MVar (Map DBName T.KVDBConn)
  , _runMode :: T.RunMode
  , _sqlConn :: MVar (Map T.ConnTag T.NativeSqlConn)
  }

createFlowRuntime :: R.CoreRuntime -> IO (FlowRuntime )
createFlowRuntime coreRt = do
  managerVar <- newManager defaultManagerSettings >>= newMVar
  optionsVar <- newMVar mempty
  connections <- newMVar Data.Map.empty
  sqlConnPools <- newMVar Data.Map.empty
  pure $ FlowRuntime coreRt managerVar optionsVar connections T.RegularMode sqlConnPools


createFlowRuntime' :: Maybe T.LoggerConfig -> IO (FlowRuntime )
createFlowRuntime'  mbLoggerCfg =
  createLoggerRuntime' mbLoggerCfg >>= R.createCoreRuntime >>= createFlowRuntime

clearFlowRuntime :: FlowRuntime  -> IO ()
clearFlowRuntime _ = pure ()

withFlowRuntime ::  Maybe T.LoggerConfig -> (FlowRuntime  -> IO a) -> IO a
withFlowRuntime mbLoggerCfg actionF =
  bracket (createLoggerRuntime' mbLoggerCfg) R.clearLoggerRuntime $ \loggerRt ->
  bracket (R.createCoreRuntime loggerRt) R.clearCoreRuntime $ \coreRt ->
  bracket (createFlowRuntime coreRt) clearFlowRuntime actionF

createLoggerRuntime' :: Maybe T.LoggerConfig -> IO R.LoggerRuntime
createLoggerRuntime' mbLoggerCfg = case mbLoggerCfg of
  Nothing        -> R.createVoidLoggerRuntime
  Just loggerCfg -> R.createLoggerRuntime loggerCfg
