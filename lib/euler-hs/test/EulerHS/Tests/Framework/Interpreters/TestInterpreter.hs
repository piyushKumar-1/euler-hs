module EulerHS.Tests.Framework.Interpreters.TestInterpreter where

import           EulerHS.Prelude
import           Data.Aeson                      (encode, decode, eitherDecode)
import Servant.Client (ClientError(..))
import qualified EulerHS.Language as L
import qualified EulerHS.Runtime as R
import qualified EulerHS.Interpreters as R

import EulerHS.TestData.Types.Framework.Flow
import Data.Coerce
import Unsafe.Coerce

user :: Any
user = unsafeCoerce $ User "John" "Snow" "00000000-0000-0000-0000-000000000000"

lhost = encode ("localhost" :: String)

data Ex = Ex deriving (Show, Eq, Generic)
instance Exception Ex

runFlowWithTestInterpreter :: R.FlowRuntime -> L.Flow a -> IO a
runFlowWithTestInterpreter flowRt = foldF (interpretFlowMethod flowRt)

interpretFlowMethod :: R.FlowRuntime -> L.FlowMethod a -> IO a

interpretFlowMethod _ (L.RunIO ioAct next) =
  next <$> ioAct

interpretFlowMethod rt (L.CallServantAPI bUrl clientAct next) = do
  let res = unsafeCoerce user

  next <$> (pure $ Right res)

interpretFlowMethod R.FlowRuntime {..} (L.GetOption k next) =
  next <$> (pure $ decode lhost)

interpretFlowMethod R.FlowRuntime {..} (L.SetOption k v next) =
  next <$> pure ()

interpretFlowMethod _ (L.GenerateGUID next) = do
  next <$> (pure "00000000-0000-0000-0000-000000000000")

interpretFlowMethod _ (L.RunSysCmd cmd next) = do
  next <$> (pure "Neo") 

interpretFlowMethod _ _ = error "not yet supported."

