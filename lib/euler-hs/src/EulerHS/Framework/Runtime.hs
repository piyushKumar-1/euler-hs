module EulerHS.Framework.Runtime
  (
    -- * Framework Runtime
    FlowRuntime(..)
  , createFlowRuntime
  , withFlowRuntime
  , kvDisconnect
  ) where

import           EulerHS.Prelude

import           Data.Map                (Map)
import           Network.HTTP.Client     (Manager, newManager)
import           Network.HTTP.Client.TLS (tlsManagerSettings)

import qualified Data.Map               as Map (empty)
import qualified Data.Pool              as DP (destroyAllResources)
import qualified Database.Beam.Postgres as BP (close)
import qualified Database.SQLite.Simple as SQLite (close)
import qualified Database.MySQL.Base    as MySQL (close)
import qualified Database.Redis         as RD (disconnect)
import qualified System.Mem             as SYSM (performGC)


import           EulerHS.Framework.Types ()

import qualified EulerHS.Core.Runtime as R
import qualified EulerHS.Core.Types   as T


-- | FlowRuntime state and options.
data FlowRuntime = FlowRuntime
  { _coreRuntime :: R.CoreRuntime
  -- ^ Contains logger settings
  , _httpClientManager :: Manager
  -- ^ Http manager, used for external api calls
  , _options :: MVar (Map ByteString ByteString)
  -- ^ Typed key-value storage
  , _kvdbConnections :: MVar (Map ByteString T.NativeKVDBConn)
  -- ^ Connections for key-value databases
  , _runMode :: T.RunMode
  -- ^ ART mode in which current flow runs
  , _sqldbConnections :: MVar (Map T.ConnTag T.NativeSqlConn)
  -- ^ Connections for SQL databases
  }

-- | Create default FlowRuntime.
createFlowRuntime :: R.CoreRuntime -> IO (FlowRuntime )
createFlowRuntime coreRt = do
  managerVar <- newManager tlsManagerSettings
  optionsVar <- newMVar mempty
  kvdbConnections <- newMVar Map.empty
  sqldbConnections <- newMVar Map.empty
  pure $ FlowRuntime coreRt managerVar optionsVar kvdbConnections T.RegularMode sqldbConnections


createFlowRuntime' :: Maybe T.LoggerConfig -> IO (FlowRuntime )
createFlowRuntime'  mbLoggerCfg =
  createLoggerRuntime' mbLoggerCfg >>= R.createCoreRuntime >>= createFlowRuntime

-- | Clear resources in given 'FlowRuntime'
clearFlowRuntime :: FlowRuntime  -> IO ()
clearFlowRuntime FlowRuntime{..} = do
  _ <- takeMVar _options
  putMVar _options mempty
  kvConns <- takeMVar _kvdbConnections
  putMVar _kvdbConnections mempty
  traverse_ kvDisconnect kvConns
  sqlConns <- takeMVar _sqldbConnections
  putMVar _sqldbConnections mempty
  traverse_ sqlDisconnect sqlConns
  -- The Manager will be shut down automatically via garbage collection.
  SYSM.performGC

sqlDisconnect :: T.NativeSqlConn -> IO ()
sqlDisconnect = \case
  T.NativePGPool connPool -> DP.destroyAllResources connPool
  T.NativeMySQLPool connPool -> DP.destroyAllResources connPool
  T.NativeSQLitePool connPool -> DP.destroyAllResources connPool
  T.NativeMockedConn -> pure ()

kvDisconnect :: T.NativeKVDBConn -> IO ()
kvDisconnect = \case
  T.NativeKVDBMockedConn -> pure ()
  T.NativeRedis conn -> RD.disconnect conn

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
