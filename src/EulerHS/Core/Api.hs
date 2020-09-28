{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}


module EulerHS.Core.Api where

import           EulerHS.Prelude
import qualified Servant.Client as SC
import qualified Servant.Client.Core as SCC
import           Servant.Client.Core.RunClient (RunClient)
import qualified Servant.Client.Free as SCF
import qualified Servant.Client.Internal.HttpClient as SCIHC

newtype EulerClient a = EulerClient (Free SCF.ClientF a)
    deriving newtype (Functor, Applicative, Monad, RunClient)

-- newtype EulerBaseUrl = EulerBaseUrl SCC.BaseUrl

client :: SC.HasClient EulerClient api => Proxy api -> SC.Client EulerClient api
client api = SCC.clientIn api $ Proxy @EulerClient

interpretClientF :: (String -> IO()) -> SCC.BaseUrl -> SCF.ClientF a -> SC.ClientM a
interpretClientF _   _    (SCF.Throw e)             = throwM e
interpretClientF log bUrl (SCF.RunRequest req next) = do
    liftIO . log $ show (SCIHC.requestToClientRequest bUrl req)
    res <- SCC.runRequest req
    liftIO . log $ show (res)
    pure $ next res

runEulerClient :: (String -> IO()) -> SCC.BaseUrl -> EulerClient a -> SCIHC.ClientM a
runEulerClient log bUrl (EulerClient f) = foldFree (interpretClientF log bUrl) f
