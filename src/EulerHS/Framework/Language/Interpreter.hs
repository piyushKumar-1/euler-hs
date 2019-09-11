module EulerHS.Framework.Language.Interpreter where

import           EulerHS.Prelude
import           Network.HTTP.Client             (Manager)

import qualified EulerHS.Framework.Language.Flow as L

import qualified Servant.Client                  as S

interpretFlowMethodL :: MVar Manager -> L.FlowMethod a -> IO a
interpretFlowMethodL m (L.CallServantAPI bUrl clientAct continuation) = do
  man <- takeMVar m
  continuation <$> catchAny (S.runClientM clientAct (S.mkClientEnv man bUrl)) (pure . Left . S.ConnectionError)

runFlowMethodL :: MVar Manager -> L.Flow a -> IO a
runFlowMethodL m = foldF (interpretFlowMethodL m)
