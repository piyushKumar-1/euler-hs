{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE  RankNTypes #-}
{-# LANGUAGE  ScopedTypeVariables #-}

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
    { url         :: String
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
  { url :: String
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
  { url = SCF.showBaseUrl bUrl
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
    req_headers' = headersToJson
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
    res_headers' = headersToJson
      $ fmap (bimap (TE.decodeUtf8 . CI.original) TE.decodeUtf8)
      $ maskServantHeaders (shouldMaskKey mbMaskConfig) getMaskText
      $ SCC.responseHeaders res
    getMaskText :: Text
    getMaskText = maybe defaultMaskText (fromMaybe defaultMaskText . Log._maskText) mbMaskConfig
    headersToJson :: Seq (Text, Text) -> A.Value
    headersToJson = A.toJSON . foldl' (\m (k,v) -> HM.insert k v m) HM.empty

client :: SC.HasClient EulerClient api => Proxy api -> SC.Client EulerClient api
client api = SCC.clientIn api $ Proxy @EulerClient

interpretClientF :: (forall msg . A.ToJSON msg => msg -> IO()) -> Maybe Log.LogMaskingConfig -> SCC.BaseUrl -> SCF.ClientF a -> SC.ClientM a
interpretClientF _   _   _ (SCF.Throw e)             = throwM e
interpretClientF log mbMaskConfig bUrl (SCF.RunRequest req next) = do
  start <- liftIO $ systemToTAITime <$> getSystemTime
  eRes <- try $! SCC.runRequestAcceptStatus Nothing req
  end <- liftIO $ systemToTAITime <$> getSystemTime
  let lat = div (diffTimeToPicoseconds $ diffAbsoluteTime end start) picoMilliDiff
  case eRes of
    Right res -> do
      let logEntry = mkServantApiCallLogEntry mbMaskConfig bUrl req res lat
      liftIO $ log logEntry
      pure $ next res
    Left (e :: SCF.ClientError) -> do
      maybeErrorRes <- case e of
        SCF.FailureResponse _ r -> pure $ Just r
        SCF.DecodeFailure _ r -> pure $ Just r
        SCF.UnsupportedContentType _ r -> pure $ Just r
        SCF.InvalidContentTypeHeader r -> pure $ Just r
        SCF.ConnectionError _ -> pure Nothing
      maybe (pure ()) (\r -> liftIO $ log $ mkServantApiCallLogEntry mbMaskConfig bUrl req r lat) maybeErrorRes
      throwM e
  where
    picoMilliDiff :: Integer
    picoMilliDiff = 1000000000

runEulerClient :: (forall msg . A.ToJSON msg => msg -> IO()) -> Maybe Log.LogMaskingConfig -> SCC.BaseUrl -> EulerClient a -> SCIHC.ClientM a
runEulerClient log mbMaskConfig bUrl (EulerClient f) = foldFree (interpretClientF log mbMaskConfig bUrl) f
