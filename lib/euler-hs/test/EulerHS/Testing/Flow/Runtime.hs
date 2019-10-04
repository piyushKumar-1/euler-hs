module EulerHS.Testing.Flow.Runtime where

import           EulerHS.Prelude
import           EulerHS.Runtime

import           Network.HTTP.Client     (defaultManagerSettings, newManager)

type FlowRtInitializer = IO FlowRuntime

initDefaultFlowRt :: FlowRtInitializer
initDefaultFlowRt = do
  manager       <- newMVar =<< newManager defaultManagerSettings
  options       <- newMVar mempty
  loggerRuntime <- createVoidLoggerRuntime
  coreRuntime   <- createCoreRuntime loggerRuntime

  pure $ FlowRuntime coreRuntime manager options
