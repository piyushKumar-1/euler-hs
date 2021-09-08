{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE  RankNTypes #-}

module EulerHS.Api where

import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as LBS (toStrict)
import qualified Data.CaseInsensitive as CI
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TE (decodeUtf8)
import           GHC.Generics ()
import qualified EulerHS.Logger.Types as Log (LogMaskingConfig (..))
import           EulerHS.Masking (defaultMaskText, getContentTypeForServant,
                                  maskQueryStrings, maskServantHeaders,
                                  parseRequestResponseBody, shouldMaskKey)
import           EulerHS.Prelude
import qualified Servant.Client as SC
import qualified Servant.Client.Core as SCC
import           Servant.Client.Core.RunClient (RunClient)
import qualified Servant.Client.Free as SCF
import qualified Servant.Client.Internal.HttpClient as SCIHC

newtype EulerClient a = EulerClient (Free SCF.ClientF a)
    deriving newtype (Functor, Applicative, Monad, RunClient)

data LogServantRequest
  = LogServantRequest
    { url         :: SCF.BaseUrl
    , method      :: Text
    , body        :: String
    , headers     :: Seq (Text, Text)
    , queryString :: Seq (Text, Maybe Text)
    }
    deriving stock (Show,Generic)
    deriving anyclass A.ToJSON

data LogServantResponse
  = LogServantResponse
    { statusCode  :: String
    , headers     :: Seq (Text,Text)
    , httpVersion :: String
    , body        :: String
    }
    deriving stock (Show,Generic)
    deriving anyclass A.ToJSON

client :: SC.HasClient EulerClient api => Proxy api -> SC.Client EulerClient api
client api = SCC.clientIn api $ Proxy @EulerClient

interpretClientF :: (forall msg . A.ToJSON msg => msg -> IO()) -> Maybe Log.LogMaskingConfig -> SCC.BaseUrl -> SCF.ClientF a -> SC.ClientM a
interpretClientF _   _   _ (SCF.Throw e)             = throwM e
interpretClientF log mbMaskConfig bUrl (SCF.RunRequest req next) = do
  liftIO $ logServantRequest log mbMaskConfig bUrl req
  res <- SCC.runRequestAcceptStatus Nothing req
  liftIO $ logServantResponse log mbMaskConfig res
  pure $ next res

runEulerClient :: (forall msg . A.ToJSON msg => msg -> IO()) -> Maybe Log.LogMaskingConfig -> SCC.BaseUrl -> EulerClient a -> SCIHC.ClientM a
runEulerClient log mbMaskConfig bUrl (EulerClient f) = foldFree (interpretClientF log mbMaskConfig bUrl) f

logServantRequest :: (forall a . A.ToJSON a => a -> IO()) -> Maybe Log.LogMaskingConfig -> SCC.BaseUrl -> SCC.Request -> IO ()
logServantRequest log mbMaskConfig url' req = do
  log $ LogServantRequest
    { url = url'
    , method = method'
    , body = body'
    , headers = headers'
    , queryString = queryString'
    }
  where
    body' = case SCC.requestBody req of
      Just (reqbody, _) ->
        case reqbody of
          SCC.RequestBodyBS s -> Text.unpack $ parseRequestResponseBody (shouldMaskKey mbMaskConfig) getMaskText (getContentTypeForServant . toList $ SCC.requestHeaders req) s
          SCC.RequestBodyLBS s -> Text.unpack $ parseRequestResponseBody (shouldMaskKey mbMaskConfig) getMaskText (getContentTypeForServant . toList $ SCC.requestHeaders req) $ LBS.toStrict s
          SCC.RequestBodySource sr -> show $ SCC.RequestBodySource sr
      Nothing -> "body = (empty)"
    method' = TE.decodeUtf8 $ SCC.requestMethod req
    headers' = fmap (bimap (TE.decodeUtf8 . CI.original) TE.decodeUtf8) $ maskServantHeaders (shouldMaskKey mbMaskConfig) getMaskText $ SCC.requestHeaders req
    queryString' = fmap (bimap TE.decodeUtf8 (fmap TE.decodeUtf8)) $ maskQueryStrings (shouldMaskKey mbMaskConfig) getMaskText $ SCC.requestQueryString req
    getMaskText :: Text
    getMaskText = maybe defaultMaskText (fromMaybe defaultMaskText . Log._maskText) mbMaskConfig

logServantResponse :: (forall a . A.ToJSON a => a -> IO()) -> Maybe Log.LogMaskingConfig -> SCC.Response -> IO ()
logServantResponse log mbMaskConfig res =
  log $ LogServantResponse
    { statusCode = status
    , headers = responseheaders
    , httpVersion = version
    , body = responseBody
    }
    where
      status = show $ SCC.responseStatusCode res
      responseheaders = fmap (bimap (TE.decodeUtf8 . CI.original) TE.decodeUtf8) $ maskServantHeaders (shouldMaskKey mbMaskConfig) getMaskText $ SCC.responseHeaders res
      version = show $ SCC.responseHttpVersion res
      responseBody =
          Text.unpack
        . parseRequestResponseBody (shouldMaskKey mbMaskConfig) getMaskText (getContentTypeForServant . toList $ SCC.responseHeaders res)
        . LBS.toStrict
        $ SCC.responseBody res

      getMaskText :: Text
      getMaskText = maybe defaultMaskText (fromMaybe defaultMaskText . Log._maskText) mbMaskConfig
