module EulerHS.Framework.Runtime where

import EulerHS.Prelude

import qualified EulerHS.Core.Runtime as R
import qualified EulerHS.Core.Types as T
import qualified EulerHS.Framework.Types as T

data FlowRuntime = FlowRuntime
  { _loggerRuntime :: R.LoggerRuntime
  }

createFlowRuntime :: R.LoggerRuntime -> IO FlowRuntime
createFlowRuntime loggerRt = pure $ FlowRuntime loggerRt

clearFlowRuntime :: FlowRuntime -> IO ()
clearFlowRuntime _ = pure ()


withAppRuntime :: Maybe T.LoggerConfig -> (FlowRuntime -> IO a) -> IO a
withAppRuntime mbLoggerCfg actionF =
  bracket createLoggerRuntime' R.clearLoggerRuntime $ \loggerRt ->
  -- bracket (R.createCoreRuntime loggerRt) R.clearCoreRuntime $ \coreRt   ->
  bracket (createFlowRuntime loggerRt) clearFlowRuntime actionF
  where
    createLoggerRuntime' = case mbLoggerCfg of
      Nothing        -> R.createVoidLoggerRuntime
      Just loggerCfg -> R.createLoggerRuntime loggerCfg
