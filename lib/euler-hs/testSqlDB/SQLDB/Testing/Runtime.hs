module SQLDB.Testing.Runtime where

import           EulerHS.Prelude
import           Data.Map            (Map, empty)
import           Network.HTTP.Client (Manager, newManager, defaultManagerSettings)

import qualified Data.Pool              as DP
import qualified Database.SQLite.Simple as SQLite
import qualified Database.MySQL.Base as MySQL
import qualified Database.Beam.Sqlite as BS
-- import qualified Database.Redis as RD (Connection)
import qualified EulerHS.Runtime as R
import qualified EulerHS.Types as T
import           EulerHS.Types
import qualified Data.Map as Map

import qualified Database.MySQL.Base             as MySQL
import qualified Database.Beam.Sqlite.Connection as SQLite
import qualified Database.Beam.Postgres          as BP
  -- data Connection = Redis RD.Connection
  
 -- type DBName = ByteString
  
  
  
 -- data FlowRuntime = FlowRuntime
 --   { _coreRuntime :: R.CoreRuntime
 --   , _httpClientManager :: MVar Manager
 --   , _options :: MVar (Map ByteString ByteString)
 --   , _connections :: MVar (Map ByteString T.KVDBConn)
 --   , _sqlConn :: Map Text T.SqlConnPool
 --   }
  
-- cleanSqlConnPools :: R.FlowRuntime -> IO ()
-- cleanSqlConnPools R.FlowRuntime {..} = for_ _sqlConn destroyAllSqlConns
--   
-- destroyAllSqlConns :: T.SqlConnPool -> IO ()
-- destroyAllSqlConns (T.NativeSqlConnPool pool) = DP.destroyAllResources pool
-- destroyAllSqlConns (T.MockedSqlConnPool) = pure ()
  {-
createFlowRuntime :: Map Text T.SqlConnPool -> R.CoreRuntime -> IO (R.FlowRuntime )
createFlowRuntime sqlConnPools coreRt = do
  managerVar <- newManager defaultManagerSettings >>= newMVar
  optionsVar <- newMVar Map.empty
  connections <- newMVar Data.Map.empty
  pure $ R.FlowRuntime coreRt managerVar optionsVar connections sqlConnPools
  
  
createFlowRuntime' :: Map Text T.SqlConnPool -> Maybe T.LoggerConfig -> IO (R.FlowRuntime )
createFlowRuntime' sqlConnPools mbLoggerCfg =
  createLoggerRuntime' mbLoggerCfg >>= R.createCoreRuntime >>= (createFlowRuntime sqlConnPools)

clearFlowRuntime :: R.FlowRuntime  -> IO ()
clearFlowRuntime _ = pure ()

withFlowRuntime :: Map Text T.SqlConnPool -> Maybe T.LoggerConfig -> (R.FlowRuntime  -> IO a) -> IO a
withFlowRuntime sqlConnPools mbLoggerCfg actionF =
  bracket (createLoggerRuntime' mbLoggerCfg) R.clearLoggerRuntime $ \loggerRt ->
  bracket (R.createCoreRuntime loggerRt) R.clearCoreRuntime $ \coreRt ->
  bracket (createFlowRuntime sqlConnPools coreRt) clearFlowRuntime actionF

createLoggerRuntime' :: Maybe T.LoggerConfig -> IO R.LoggerRuntime
createLoggerRuntime' mbLoggerCfg = case mbLoggerCfg of
  Nothing        -> R.createVoidLoggerRuntime
  Just loggerCfg -> R.createLoggerRuntime loggerCfg
  -}