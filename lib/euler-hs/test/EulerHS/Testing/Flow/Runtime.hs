module EulerHS.Testing.Flow.Runtime where

import           EulerHS.Prelude
import           EulerHS.Runtime

import           Network.HTTP.Client     (defaultManagerSettings, newManager)
import           Database.Redis (checkedConnect, defaultConnectInfo, Redis(..))
import           Data.Map (singleton)

type FlowRtInitializer = IO FlowRuntime

initDefaultFlowRt :: FlowRtInitializer
initDefaultFlowRt = do
  manager <- newMVar =<< newManager defaultManagerSettings
  options <- newMVar mempty
  lrt <- createVoidLoggerRuntime
  conn <- newEmptyMVar
  pure $ FlowRuntime lrt manager options conn
