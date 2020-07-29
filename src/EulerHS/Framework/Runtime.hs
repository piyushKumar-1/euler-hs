module EulerHS.Framework.Runtime
  (
    -- * Framework Runtime
    FlowRuntime(..)
  , createFlowRuntime
  , createFlowRuntime'
  , withFlowRuntime
  , kvDisconnect
  , runPubSubWorker
  , setTransientLoggerContext
  , shouldFlowLogRawSql
  ) where

import           EulerHS.Prelude

import           Data.Map (Map)
import           Network.HTTP.Client (Manager, newManager)
import           Network.HTTP.Client.TLS (tlsManagerSettings)

import qualified Data.Map as Map (empty)
import qualified Data.Pool as DP (destroyAllResources)
import qualified Database.MySQL.Base as MySQL
import qualified Database.Redis as RD
import qualified System.Mem as SYSM (performGC)

import           System.IO.Unsafe (unsafePerformIO)

import qualified EulerHS.Core.Runtime as R
import qualified EulerHS.Core.Types as T


-- | FlowRuntime state and options.
data FlowRuntime = FlowRuntime
  { _coreRuntime              :: R.CoreRuntime
  -- ^ Contains logger settings
  , _defaultHttpClientManager :: Manager
  -- ^ Http default manager, used for external api calls
  , _httpClientManagers       :: Map String Manager
  -- ^ Http managers, used for external api calls
  , _options                  :: MVar (Map Text Any)
  -- ^ Typed key-value storage
  , _kvdbConnections          :: MVar (Map Text T.NativeKVDBConn)
  -- ^ Connections for key-value databases
  , _runMode                  :: T.RunMode
  -- ^ ART mode in which current flow runs
  , _sqldbConnections         :: MVar (Map T.ConnTag T.NativeSqlPool)
  -- ^ Connections for SQL databases
  , _pubSubController         :: RD.PubSubController
  -- ^ Subscribe controller
  , _pubSubConnection         :: Maybe RD.Connection
  -- ^ Connection being used for Publish
  }

-- | Create default FlowRuntime.
createFlowRuntime :: R.CoreRuntime -> IO FlowRuntime
createFlowRuntime coreRt = do
  defaultManagerVar <- newManager tlsManagerSettings
  optionsVar        <- newMVar mempty
  kvdbConnections   <- newMVar Map.empty
  sqldbConnections  <- newMVar Map.empty
  pubSubController  <- RD.newPubSubController [] []

  pure $ FlowRuntime
    { _coreRuntime              = coreRt
    , _defaultHttpClientManager = defaultManagerVar
    , _httpClientManagers       = Map.empty
    , _options                  = optionsVar
    , _kvdbConnections          = kvdbConnections
    , _runMode                  = T.RegularMode
    , _sqldbConnections         = sqldbConnections
    , _pubSubController         = pubSubController
    , _pubSubConnection         = Nothing
    }

createFlowRuntime' :: Maybe T.LoggerConfig -> IO FlowRuntime
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

setTransientLoggerContext :: R.TransientLoggerContext -> FlowRuntime -> FlowRuntime
setTransientLoggerContext ctx rt@FlowRuntime{_coreRuntime} =
  rt { _coreRuntime = _coreRuntime { R._loggerRuntime = loggerRuntimeWithCtx } }
  where loggerRuntimeWithCtx = R.setTransientContext ctx (R._loggerRuntime _coreRuntime)

shouldFlowLogRawSql :: FlowRuntime -> Bool
shouldFlowLogRawSql = R.shouldLogRawSql . R._loggerRuntime . _coreRuntime

sqlDisconnect :: T.NativeSqlPool -> IO ()
sqlDisconnect = \case
  T.NativePGPool connPool -> DP.destroyAllResources connPool
  T.NativeMySQLPool connPool -> DP.destroyAllResources connPool
  T.NativeSQLitePool connPool -> DP.destroyAllResources connPool
  T.NativeMockedPool -> pure ()

kvDisconnect :: T.NativeKVDBConn -> IO ()
kvDisconnect = \case
  T.NativeKVDBMockedConn -> pure ()
  T.NativeKVDB conn -> RD.disconnect conn

-- | Run flow with given logger config.
withFlowRuntime ::  Maybe T.LoggerConfig -> (FlowRuntime -> IO a) -> IO a
withFlowRuntime mbLoggerCfg actionF =
  bracket (createLoggerRuntime' mbLoggerCfg) R.clearLoggerRuntime $ \loggerRt ->
  bracket (R.createCoreRuntime loggerRt) R.clearCoreRuntime $ \coreRt ->
  bracket (createFlowRuntime coreRt) clearFlowRuntime actionF

-- | Create logger runtime with given configuration.
createLoggerRuntime' :: Maybe T.LoggerConfig -> IO R.LoggerRuntime
createLoggerRuntime' mbLoggerCfg = case mbLoggerCfg of
  Nothing        -> R.createVoidLoggerRuntime
  Just loggerCfg -> R.createLoggerRuntime loggerCfg

-- Use {-# NOINLINE foo #-} as a pragma on any function foo that calls unsafePerformIO.
-- If the call is inlined, the I/O may be performed more than once.
{-# NOINLINE pubSubWorkerLock #-}
pubSubWorkerLock :: MVar ()
pubSubWorkerLock = unsafePerformIO $ newMVar ()

runPubSubWorker :: FlowRuntime -> (Text -> IO ()) -> IO (IO ())
runPubSubWorker rt log = do
    let tsecond = 10 ^ (6 :: Int)

    lock <- tryTakeMVar pubSubWorkerLock
    case lock of
      Nothing -> error "Unable to run Publish/Subscribe worker: Only one worker allowed"
      Just _  -> pure ()

    let mconnection = _pubSubConnection rt

    delayRef <- newIORef tsecond

    threadId <- case mconnection of
      Nothing   -> do
        putMVar pubSubWorkerLock ()
        error "Unable to run Publish/Subscribe worker: No connection to Redis provided"

      Just conn -> forkIO $ forever $ do
        res <- try @_ @SomeException $ RD.pubSubForever conn (_pubSubController rt) $ do
          writeIORef delayRef tsecond
          log "Publish/Subscribe worker: Run successfuly"

        case res of
          Left e -> do
              delay <- readIORef delayRef

              log $ "Publish/Subscribe worker: Got error: " <> show e
              log $ "Publish/Subscribe worker: Restart in " <> show (delay `div` tsecond) <> " sec"

              modifyIORef' delayRef (\d -> d + d `div` 2) -- (* 1.5)
              threadDelay delay
          Right _ -> pure ()

    pure $ do
      killThread threadId
      putMVar pubSubWorkerLock ()
      log $ "Publish/Subscribe worker: Killed"
