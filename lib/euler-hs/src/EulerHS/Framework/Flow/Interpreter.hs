module EulerHS.Framework.Flow.Interpreter where

import           EulerHS.Prelude
import           Data.Aeson                      (encode, decode)
import qualified Data.ByteString.Lazy            as BSL
import qualified Data.Map                        as Map
import qualified Servant.Client                  as S


import qualified EulerHS.Core.Language as L
import qualified EulerHS.Core.Runtime as R
import qualified EulerHS.Core.Interpreters as R
import qualified EulerHS.Framework.Language as L
import qualified EulerHS.Framework.Runtime as R

interpretFlowMethod :: R.FlowRuntime -> L.FlowMethod a -> IO a
interpretFlowMethod _ (L.CallAPI _ next) = error "CallAPI not yet supported."
interpretFlowMethod (R.FlowRuntime _ managerVar _) (L.CallServantAPI bUrl clientAct next) = do
  manager <- takeMVar managerVar
  result <- next <$> catchAny (S.runClientM clientAct (S.mkClientEnv manager bUrl)) (pure . Left . S.ConnectionError)
  putMVar managerVar manager
  pure result
interpretFlowMethod (R.FlowRuntime loggerRt _ _) (L.EvalLogger loggerAct next) =
  next <$> R.runLogger loggerRt loggerAct

interpretFlowMethod _ (L.RunIO ioAct next) =
  next <$> ioAct

interpretFlowMethod R.FlowRuntime {..} (L.GetOption k next) =
  next <$> maybeValue
  where
    maybeValue = do
      m <- readMVar _options
      pure $ decode . BSL.fromStrict =<< Map.lookup (BSL.toStrict $ encode k) m

interpretFlowMethod R.FlowRuntime {..} (L.SetOption k v next) =
  next <$> set
  where
    set = do
      m <- takeMVar _options
      let newMap = Map.insert (BSL.toStrict $ encode k) (BSL.toStrict $ encode v) m
      putMVar _options newMap

runFlow :: R.FlowRuntime -> L.Flow a -> IO a
runFlow flowRt = foldF (interpretFlowMethod flowRt)
