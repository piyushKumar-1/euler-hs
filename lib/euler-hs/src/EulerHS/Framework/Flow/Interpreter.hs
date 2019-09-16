module EulerHS.Framework.Flow.Interpreter where

import           EulerHS.Prelude
import qualified Servant.Client                  as S

import qualified EulerHS.Core.Language as L
import qualified EulerHS.Core.Runtime as R
import qualified EulerHS.Core.Interpreters as R
import qualified EulerHS.Framework.Language as L
import qualified EulerHS.Framework.Runtime as R

interpretFlowMethod :: R.FlowRuntime -> L.FlowMethod a -> IO a
interpretFlowMethod (R.FlowRuntime loggerRt _) (L.CallAPI _ next) = error "CallAPI not yet supported."
interpretFlowMethod (R.FlowRuntime _ managerVar) (L.CallServantAPI bUrl clientAct next) = do
  manager <- takeMVar managerVar
  result <- next <$> catchAny (S.runClientM clientAct (S.mkClientEnv manager bUrl)) (pure . Left . S.ConnectionError)
  putMVar managerVar manager
  pure result
interpretFlowMethod (R.FlowRuntime loggerRt _) (L.EvalLogger loggerAct next) =
  next <$> R.runLogger loggerRt loggerAct

runFlow :: R.FlowRuntime -> L.Flow a -> IO a
runFlow flowRt = foldF (interpretFlowMethod flowRt)
