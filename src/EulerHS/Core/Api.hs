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

interpretClientF :: (String -> IO ()) -> SCC.BaseUrl -> SCF.ClientF a -> SC.ClientM a
interpretClientF _   _    (SCF.Throw e)             = throwM e
interpretClientF log bUrl (SCF.RunRequest req next) = do
  liftIO $ logServantRequest log bUrl req
  res <- SCC.runRequestAcceptStatus Nothing req
  liftIO . log $ show res
  pure $ next res

logServantRequest :: (String -> IO ()) -> SCC.BaseUrl -> SCC.Request -> IO ()
logServantRequest log url req =
  do
    log $ "url: " <> show url <>
          "method: " <> show method <>
          "body: " <> body <>
          "headers: " <> show headers <>
          "query string: " <> show queryString -- included in url?

  where
    body = case SCC.requestBody req of
      Just (body, mediaType) -> show body
      Nothing -> "body = (empty)"

    method = SCC.requestMethod req
    headers = SCC.requestHeaders req
    queryString = SCC.requestQueryString req

  -- liftIO . log $ show (SCIHC.requestToClientRequest bUrl req)

runEulerClient :: (String -> IO()) -> SCC.BaseUrl -> EulerClient a -> SCIHC.ClientM a
runEulerClient log bUrl (EulerClient f) = foldFree (interpretClientF log bUrl) f
