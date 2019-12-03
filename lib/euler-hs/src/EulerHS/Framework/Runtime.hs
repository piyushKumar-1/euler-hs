module EulerHS.Framework.Runtime
  (
    -- * Framework Runtime
    FlowRuntime(..)
  , createFlowRuntime
  , withFlowRuntime
  ) where

import           EulerHS.Prelude

import           Data.Map                (Map)
import           Network.HTTP.Client     (Manager, newManager)
import           Network.HTTP.Client.TLS (tlsManagerSettings)

import qualified Data.Map               as Map
import qualified Data.Pool              as DP
import qualified Database.Beam.Postgres as BP
import qualified Database.SQLite.Simple as SQLite
import qualified Database.MySQL.Base    as MySQL
import qualified Database.Redis         as RD (disconnect)
import qualified System.Mem             as SYSM (performGC)


import           EulerHS.Framework.Types ()

import qualified EulerHS.Core.Runtime as R
import qualified EulerHS.Core.Types   as T


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
  kvdbConnections <- newMVar Map.empty
  sqldbConnections <- newMVar Map.empty
  pure $ FlowRuntime coreRt managerVar optionsVar kvdbConnections T.RegularMode sqldbConnections


createFlowRuntime' :: Maybe T.LoggerConfig -> IO (FlowRuntime )
createFlowRuntime'  mbLoggerCfg =
  createLoggerRuntime' mbLoggerCfg >>= R.createCoreRuntime >>= createFlowRuntime

-- | Clear resources in given 'FlowRuntime'
clearFlowRuntime :: FlowRuntime  -> IO ()
clearFlowRuntime FlowRuntime{..} = do
  R.clearCoreRuntime _coreRuntime
  _ <- takeMVar _options
  putMVar _options mempty
  kvConns <- takeMVar _kvdbConnections
  putMVar _kvdbConnections mempty
  traverse_ kvDisconnect kvConns
  sqlConns <- takeMVar _sqldbConnections
  putMVar _sqldbConnections mempty
  traverse_ sqlDisconnect sqlConns
  _ <- takeMVar _httpClientManager
  putMVar _httpClientManager =<< newManager tlsManagerSettings
  SYSM.performGC

sqlDisconnect :: T.NativeSqlConn -> IO ()
sqlDisconnect = \case
  T.NativePGConn conn -> BP.close conn
  T.NativeMySQLConn conn -> MySQL.close conn
  T.NativeSQLiteConn conn -> SQLite.close conn
  T.NativePGPool connPool -> DP.destroyAllResources connPool
  T.NativeMySQLPool connPool -> DP.destroyAllResources connPool
  T.NativeSQLitePool connPool -> DP.destroyAllResources connPool
  T.NativeMockedConn -> pure ()

kvDisconnect :: T.KVDBConn -> IO ()
kvDisconnect = \case
  T.Mocked -> pure ()
  T.Redis conn -> RD.disconnect conn

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
