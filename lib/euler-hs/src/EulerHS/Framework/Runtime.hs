module EulerHS.Framework.Runtime where

import           EulerHS.Prelude
import           Network.HTTP.Client (Manager, newManager, defaultManagerSettings)

import qualified EulerHS.Core.Runtime as R
import qualified EulerHS.Core.Types as T
import qualified EulerHS.Framework.Types as T

data FlowRuntime = FlowRuntime
  { _loggerRuntime :: R.LoggerRuntime
  , _httpClientManager :: MVar Manager
  }

createFlowRuntime :: R.LoggerRuntime -> IO FlowRuntime
createFlowRuntime loggerRt = do
  managerVar <- newManager defaultManagerSettings >>= newMVar
  pure $ FlowRuntime loggerRt managerVar

clearFlowRuntime :: FlowRuntime -> IO ()
clearFlowRuntime _ = pure ()

withFlowRuntime :: Maybe T.LoggerConfig -> (FlowRuntime -> IO a) -> IO a
withFlowRuntime mbLoggerCfg actionF =
  bracket createLoggerRuntime' R.clearLoggerRuntime $ \loggerRt ->
  -- bracket (R.createCoreRuntime loggerRt) R.clearCoreRuntime $ \coreRt   ->
  bracket (createFlowRuntime loggerRt) clearFlowRuntime actionF
  where
    createLoggerRuntime' = case mbLoggerCfg of
      Nothing        -> R.createVoidLoggerRuntime
      Just loggerCfg -> R.createLoggerRuntime loggerCfg
