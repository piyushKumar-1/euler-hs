module EulerHS.Framework.Runtime
  (
    -- * Framework Runtime
    FlowRuntime(..)
  , createFlowRuntime
  , withFlowRuntime
  ) where

import           EulerHS.Prelude
import           Data.Map            (Map, empty)
import           Network.HTTP.Client (Manager, newManager, defaultManagerSettings)
import           Network.HTTP.Client.TLS (tlsManagerSettings)

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



-- | FlowRuntime state and options.
data FlowRuntime = FlowRuntime
  { _coreRuntime :: R.CoreRuntime
  -- ^ Contains logger settings
  , _httpClientManager :: MVar Manager
  -- ^ Http manager, used for external api calls
  , _options :: MVar (Map ByteString ByteString)
  -- ^ Typed key-value storage
  , _kvdbConnections :: MVar (Map ByteString T.KVDBConn)
  -- ^ Connections for key-value databases
  , _runMode :: T.RunMode
  -- ^ ART mode in which current flow runs
  , _sqldbConnections :: MVar (Map T.ConnTag T.NativeSqlConn)
  -- ^ Connections for SQL databases
  }

-- | Create default FlowRuntime.
createFlowRuntime :: R.CoreRuntime -> IO (FlowRuntime )
createFlowRuntime coreRt = do
  managerVar <- newManager tlsManagerSettings >>= newMVar
  optionsVar <- newMVar mempty
  kvdbConnections <- newMVar Data.Map.empty
  sqldbConnections <- newMVar Data.Map.empty
  pure $ FlowRuntime coreRt managerVar optionsVar kvdbConnections T.RegularMode sqldbConnections


createFlowRuntime' :: Maybe T.LoggerConfig -> IO (FlowRuntime )
createFlowRuntime'  mbLoggerCfg =
  createLoggerRuntime' mbLoggerCfg >>= R.createCoreRuntime >>= createFlowRuntime

clearFlowRuntime :: FlowRuntime  -> IO ()
clearFlowRuntime _ = pure ()

-- | Run flow with given logger config.
withFlowRuntime ::  Maybe T.LoggerConfig -> (FlowRuntime  -> IO a) -> IO a
withFlowRuntime mbLoggerCfg actionF =
  bracket (createLoggerRuntime' mbLoggerCfg) R.clearLoggerRuntime $ \loggerRt ->
  bracket (R.createCoreRuntime loggerRt) R.clearCoreRuntime $ \coreRt ->
  bracket (createFlowRuntime coreRt) clearFlowRuntime actionF

-- | Create logger runtime with given configuration.
createLoggerRuntime' :: Maybe T.LoggerConfig -> IO R.LoggerRuntime
createLoggerRuntime' mbLoggerCfg = case mbLoggerCfg of
  Nothing        -> R.createVoidLoggerRuntime
  Just loggerCfg -> R.createLoggerRuntime loggerCfg
