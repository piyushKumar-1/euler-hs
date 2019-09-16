module EulerHS.Framework.Flow.Interpreter where

import           EulerHS.Prelude
import           Network.HTTP.Client             (Manager)
import qualified Servant.Client                  as S

import qualified EulerHS.Framework.Flow.Language as L

interpretFlowMethod :: MVar Manager -> L.FlowMethod a -> IO a
interpretFlowMethod m (L.CallServantAPI bUrl clientAct continuation) = do
  man <- takeMVar m
  result <- continuation <$> catchAny (S.runClientM clientAct (S.mkClientEnv man bUrl)) (pure . Left . S.ConnectionError)
  putMVar m man
  pure result

runFlowMethod :: MVar Manager -> L.Flow a -> IO a
runFlowMethod m = foldF (interpretFlowMethod m)
