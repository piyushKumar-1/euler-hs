module EulerHS.Framework.Runtime where

import           EulerHS.Prelude
import           Data.Map            (Map, empty)
import           Network.HTTP.Client (Manager, newManager, defaultManagerSettings)

import qualified Database.Redis as RD (Connection)
import qualified EulerHS.Core.Runtime as R
import qualified EulerHS.Core.Types as T
import qualified EulerHS.Framework.Types as T

data Connection = Redis RD.Connection

type DBName = ByteString

data FlowRuntime = FlowRuntime
  { _coreRuntime :: R.CoreRuntime
  , _httpClientManager :: MVar Manager
  , _options :: MVar (Map ByteString ByteString)
  , _connections :: MVar (Map DBName Connection)
  }

createFlowRuntime :: R.CoreRuntime -> IO FlowRuntime
createFlowRuntime coreRt = do
  managerVar <- newManager defaultManagerSettings >>= newMVar
  optionsVar <- newMVar mempty
  connections <- newMVar Data.Map.empty
  pure $ FlowRuntime coreRt managerVar optionsVar connections

createFlowRuntime' :: Maybe T.LoggerConfig -> IO FlowRuntime
createFlowRuntime' mbLoggerCfg =
  createLoggerRuntime' mbLoggerCfg >>= R.createCoreRuntime >>= createFlowRuntime

clearFlowRuntime :: FlowRuntime -> IO ()
clearFlowRuntime _ = pure ()

withFlowRuntime :: Maybe T.LoggerConfig -> (FlowRuntime -> IO a) -> IO a
withFlowRuntime mbLoggerCfg actionF =
  bracket (createLoggerRuntime' mbLoggerCfg) R.clearLoggerRuntime $ \loggerRt ->
  bracket (R.createCoreRuntime loggerRt) R.clearCoreRuntime $ \coreRt ->
  bracket (createFlowRuntime coreRt) clearFlowRuntime actionF

createLoggerRuntime' :: Maybe T.LoggerConfig -> IO R.LoggerRuntime
createLoggerRuntime' mbLoggerCfg = case mbLoggerCfg of
  Nothing        -> R.createVoidLoggerRuntime
  Just loggerCfg -> R.createLoggerRuntime loggerCfg
