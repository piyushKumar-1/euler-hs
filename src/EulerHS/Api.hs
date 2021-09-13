{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE  RankNTypes #-}

module EulerHS.Api where

import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as LBS (toStrict)
import qualified Data.CaseInsensitive as CI
import qualified Data.HashMap.Strict as HM
import           Data.Time.Clock (diffTimeToPicoseconds)
import           Data.Time.Clock.System (getSystemTime, systemToTAITime)
import           Data.Time.Clock.TAI (diffAbsoluteTime)
import qualified Data.Text.Encoding as TE (decodeUtf8)
import           GHC.Generics ()
import qualified EulerHS.Logger.Types as Log (LogMaskingConfig (..))
import           EulerHS.Masking (defaultMaskText, getContentTypeForServant,
                                  maskServantHeaders,
                                  parseRequestResponseBody, shouldMaskKey)
import           EulerHS.Prelude
import qualified Servant.Client as SC
import qualified Servant.Client.Core as SCC
import           Servant.Client.Core.RunClient (RunClient)
import qualified Servant.Client.Free as SCF
import qualified Servant.Client.Internal.HttpClient as SCIHC
import qualified Network.HTTP.Types.Status as HttpStatus

newtype EulerClient a = EulerClient (Free SCF.ClientF a)
    deriving newtype (Functor, Applicative, Monad, RunClient)

data LogServantRequest
  = LogServantRequest
    { url         :: SCF.BaseUrl
    , method      :: Text
    , body        :: A.Value
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
    , body        :: A.Value
    }
    deriving stock (Show,Generic)
    deriving anyclass A.ToJSON

data ServantApiCallLogEntry = ServantApiCallLogEntry
  { url :: SCF.BaseUrl
  , method :: Text
  , req_headers :: A.Value
  , req_body :: A.Value
  , res_code :: Int
  , res_body :: A.Value
  , res_headers :: A.Value
  , latency :: Integer
  }
    deriving stock (Show,Generic)
    deriving anyclass A.ToJSON

mkServantApiCallLogEntry :: Maybe Log.LogMaskingConfig -> SCF.BaseUrl -> SCC.Request -> SCC.Response -> Integer -> ServantApiCallLogEntry
mkServantApiCallLogEntry mbMaskConfig bUrl req res lat = ServantApiCallLogEntry
  { url = bUrl
  , method = method'
  , req_headers = req_headers'
  , req_body = req_body'
  , res_code = res_code'
  , res_body = res_body'
  , res_headers = res_headers'
  , latency = lat
  }
  where
    method' = TE.decodeUtf8 $ SCC.requestMethod req
    req_headers' = foldHeaders
      $ fmap (bimap (TE.decodeUtf8 . CI.original) TE.decodeUtf8)
      $ maskServantHeaders (shouldMaskKey mbMaskConfig) getMaskText
      $ SCC.requestHeaders req
    req_body' = case SCC.requestBody req of
      Just (reqbody, _) ->
        case reqbody of
          SCC.RequestBodyBS s -> parseRequestResponseBody (shouldMaskKey mbMaskConfig) getMaskText (getContentTypeForServant . toList $ SCC.requestHeaders req) s
          SCC.RequestBodyLBS s -> parseRequestResponseBody (shouldMaskKey mbMaskConfig) getMaskText (getContentTypeForServant . toList $ SCC.requestHeaders req) $ LBS.toStrict s
          SCC.RequestBodySource sr -> A.String $ show $ SCC.RequestBodySource sr
      Nothing -> A.String "body = (empty)"
    res_code' = HttpStatus.statusCode $ SCC.responseStatusCode res
    res_body' = parseRequestResponseBody (shouldMaskKey mbMaskConfig) getMaskText (getContentTypeForServant . toList $ SCC.responseHeaders res)
        . LBS.toStrict
        $ SCC.responseBody res
    res_headers' = foldHeaders
      $ fmap (bimap (TE.decodeUtf8 . CI.original) TE.decodeUtf8)
      $ maskServantHeaders (shouldMaskKey mbMaskConfig) getMaskText
      $ SCC.responseHeaders res
    getMaskText :: Text
    getMaskText = maybe defaultMaskText (fromMaybe defaultMaskText . Log._maskText) mbMaskConfig
    foldHeaders :: Seq (Text, Text) -> A.Value
    foldHeaders = A.toJSON . foldl' (\m (k,v) -> HM.insert k v m) HM.empty

client :: SC.HasClient EulerClient api => Proxy api -> SC.Client EulerClient api
client api = SCC.clientIn api $ Proxy @EulerClient

interpretClientF :: (forall msg . A.ToJSON msg => msg -> IO()) -> Maybe Log.LogMaskingConfig -> SCC.BaseUrl -> SCF.ClientF a -> SC.ClientM a
interpretClientF _   _   _ (SCF.Throw e)             = throwM e
interpretClientF log mbMaskConfig bUrl (SCF.RunRequest req next) = do
  start <- liftIO $ systemToTAITime <$> getSystemTime
  res <- SCC.runRequestAcceptStatus Nothing req
  end <- liftIO $ systemToTAITime <$> getSystemTime
  let lat = div (diffTimeToPicoseconds $ diffAbsoluteTime end start) picoMilliDiff
  let logEntry = mkServantApiCallLogEntry mbMaskConfig bUrl req res lat
  liftIO $ log logEntry
  pure $ next res
  where
    picoMilliDiff :: Integer
    picoMilliDiff = 1000000000

runEulerClient :: (forall msg . A.ToJSON msg => msg -> IO()) -> Maybe Log.LogMaskingConfig -> SCC.BaseUrl -> EulerClient a -> SCIHC.ClientM a
runEulerClient log mbMaskConfig bUrl (EulerClient f) = foldFree (interpretClientF log mbMaskConfig bUrl) f
