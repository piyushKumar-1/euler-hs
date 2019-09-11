module EulerHS.Framework.Language.Interpreter where

import           EulerHS.Prelude
import           EulerHS.Framework.Language.Flow
import           EulerHS.Framework.Language.Flow as L
import           EulerHS.Framework.Language.Types
import           Data.Aeson                      (encode, decode)
import           Network.HTTP.Client             (Manager)

import qualified Data.ByteString.Lazy     as BSL (toStrict, fromStrict)
import qualified Data.Map as Map                 (lookup, insert)
import qualified Servant.Client           as S


runFlow :: Runtime -> Flow a -> IO a
runFlow rt = foldF (interpretFlowMethod rt)

interpretFlowMethod :: Runtime -> FlowMethod a -> IO a

interpretFlowMethod rt (RunIO ioAct next) =
  next <$> ioAct

interpretFlowMethod Runtime{..} (GetOption k next) =
  next <$> maybeValue
  where
    maybeValue = do
          m <- readMVar options
          pure $ decode . BSL.fromStrict =<< Map.lookup (BSL.toStrict $ encode k) m

interpretFlowMethod Runtime{..} (SetOption k v next) =
  next <$> set
  where
    set = do
      m <- takeMVar options
      let newMap = Map.insert (BSL.toStrict $ encode k) (BSL.toStrict $ encode v) m
      putMVar options newMap

interpretFlowMethod Runtime{..} (CallServantAPI bUrl clientAct continuation) = do
  continuation <$> catchAny (S.runClientM clientAct (S.mkClientEnv networkManager bUrl)) (pure . Left . S.ConnectionError)


interpretFlowMethodL :: MVar Manager -> L.FlowMethod a -> IO a
interpretFlowMethodL m (L.CallServantAPI bUrl clientAct continuation) = do
  man <- takeMVar m
  continuation <$> catchAny (S.runClientM clientAct (S.mkClientEnv man bUrl)) (pure . Left . S.ConnectionError)

runFlowMethodL :: MVar Manager -> L.Flow a -> IO a
runFlowMethodL m = foldF (interpretFlowMethodL m)
