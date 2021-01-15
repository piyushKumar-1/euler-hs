{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}


module EulerHS.Core.Api where
import           EulerHS.Prelude
import qualified Servant.Client as SC
import qualified Servant.Client.Core as SCC
import           Servant.Client.Core.RunClient (RunClient)
import qualified Servant.Client.Free as SCF
import qualified Servant.Client.Internal.HttpClient as SCIHC
import qualified Network.HTTP.Types as HTTP
import qualified Data.ByteString.Lazy as LBS(toStrict)
import qualified Data.Text as Text (unpack)
import qualified EulerHS.Core.Types.Logger as Log (LogMaskingConfig(..))
import           EulerHS.Core.Masking

newtype EulerClient a = EulerClient (Free SCF.ClientF a)
    deriving newtype (Functor, Applicative, Monad, RunClient)

data LogServantRequest
  = LogServantRequest
    { url :: SCF.BaseUrl
    , method :: HTTP.Method
    , body :: String
    , headers :: Seq HTTP.Header
    , queryString :: Seq HTTP.QueryItem
    }
    deriving (Show)

data LogServantResponse
  = LogServantResponse
    { statusCode :: HTTP.Status
    , headers :: Seq HTTP.Header
    , httpVersion :: HTTP.HttpVersion
    , body :: String
    }
    deriving (Show)

client :: SC.HasClient EulerClient api => Proxy api -> SC.Client EulerClient api
client api = SCC.clientIn api $ Proxy @EulerClient

interpretClientF :: (String -> IO ()) -> Maybe Log.LogMaskingConfig -> SCC.BaseUrl -> SCF.ClientF a -> SC.ClientM a
interpretClientF _   _   _ (SCF.Throw e)             = throwM e
interpretClientF log mbMaskConfig bUrl (SCF.RunRequest req next) = do
  liftIO $ logServantRequest log mbMaskConfig bUrl req
  res <- SCC.runRequestAcceptStatus Nothing req
  liftIO $ logServantResponse log mbMaskConfig res
  pure $ next res

runEulerClient :: (String -> IO()) -> Maybe Log.LogMaskingConfig -> SCC.BaseUrl -> EulerClient a -> SCIHC.ClientM a
runEulerClient log mbMaskConfig bUrl (EulerClient f) = foldFree (interpretClientF log mbMaskConfig bUrl) f

logServantRequest :: (String -> IO ()) -> Maybe Log.LogMaskingConfig -> SCC.BaseUrl -> SCC.Request -> IO ()
logServantRequest log mbMaskConfig url req = do
  log $ show $ LogServantRequest
    { url = url
    , method = method
    , body = body
    , headers = headers
    , queryString = queryString
    }

  where
    body = case SCC.requestBody req of
      Just (reqbody, _) ->
        case reqbody of
          SCC.RequestBodyBS s -> Text.unpack $ parseRequestResponseBody (shouldMaskKey mbMaskConfig) getMaskText s
          SCC.RequestBodyLBS s -> Text.unpack $ parseRequestResponseBody (shouldMaskKey mbMaskConfig) getMaskText  $ LBS.toStrict s
          SCC.RequestBodySource sr -> show $ SCC.RequestBodySource sr
      Nothing -> "body = (empty)"

    method = SCC.requestMethod req
    headers = maskHeaders (shouldMaskKey mbMaskConfig) getMaskText $ SCC.requestHeaders req
    queryString = maskQueryStrings (shouldMaskKey mbMaskConfig) getMaskText $ SCC.requestQueryString req

    getMaskText :: Text
    getMaskText = maybe defaultMaskText (fromMaybe defaultMaskText . Log._maskText) mbMaskConfig

logServantResponse ::  (String -> IO ()) -> Maybe Log.LogMaskingConfig -> SCC.Response -> IO ()
logServantResponse log mbMaskConfig res =
  log $ show $ LogServantResponse
    { statusCode = status
    , headers = responseheaders
    , httpVersion = version
    , body = responseBody
    }
    where
      status = SCC.responseStatusCode res
      responseheaders = maskHeaders (shouldMaskKey mbMaskConfig) getMaskText $ SCC.responseHeaders res
      version = SCC.responseHttpVersion res
      responseBody =
          Text.unpack
        . parseRequestResponseBody (shouldMaskKey mbMaskConfig) getMaskText
        . LBS.toStrict
        $ SCC.responseBody res

      getMaskText :: Text
      getMaskText = maybe defaultMaskText (fromMaybe defaultMaskText . Log._maskText) mbMaskConfig

-- shouldMaskKey :: Maybe Log.LogMaskingConfig -> Text -> Bool
-- shouldMaskKey Nothing _ = False
-- shouldMaskKey (Just Log.LogMaskingConfig{..}) key =
--   case _keyType of
--     Log.WhiteListKey -> not $ member key _maskKeys
--     Log.BlackListKey -> member key _maskKeys

-- defaultMaskText :: Text
-- defaultMaskText = "***"

-- maskHeaders :: (Text -> Bool) -> Text -> Seq HTTP.Header -> Seq HTTP.Header
-- maskHeaders shouldMask maskText headers = maskHeader <$> headers
--   where
--     maskHeader :: HTTP.Header -> HTTP.Header
--     maskHeader (headerName,headerValue) =
--       if shouldMask (decodeUtf8 $ CI.original headerName)
--         then (headerName,(encodeUtf8 maskText))
--         else (headerName,headerValue)

-- maskQueryStrings :: (Text -> Bool) -> Text -> Seq HTTP.QueryItem -> Seq HTTP.QueryItem
-- maskQueryStrings shouldMask maskText queryStrings = maskQueryString <$> queryStrings
--   where
--     maskQueryString :: HTTP.QueryItem -> HTTP.QueryItem
--     maskQueryString (key,value) =
--       if shouldMask (decodeUtf8 key)
--         then (key,(Just $ encodeUtf8 maskText))
--         else (key,value)

-- parseRequestResponseBody :: (Text -> Bool) -> Text -> ByteString -> Text
-- parseRequestResponseBody shouldMask maskText req =
--   case Aeson.eitherDecodeStrict req of
--     Right value ->  decodeUtf8 . Aeson.encode $ maskJSON shouldMask maskText value
--     Left _ -> decodeUtf8 . Aeson.encode $ maskJSON shouldMask maskText $ handleQueryString req

-- maskJSON :: (Text -> Bool) -> Text -> Aeson.Value -> Aeson.Value
-- maskJSON shouldMask maskText (Aeson.Object r) = (Aeson.Object $ handleObject shouldMask maskText r)
-- maskJSON shouldMask maskText (Aeson.Array r) =  (Aeson.Array $ maskJSON shouldMask maskText <$> r)
-- maskJSON _ _ value = value

-- handleObject :: (Text -> Bool) -> Text -> Aeson.Object -> Aeson.Object
-- handleObject shouldMask maskText obj = HashMap.mapWithKey maskingFn obj
--   where
--     maskingFn key value = maskJSON shouldMask maskText $ updatedValue key value
--     updatedValue key fn = if shouldMask key then Aeson.String maskText else fn

-- handleQueryString :: ByteString -> Aeson.Value
-- handleQueryString strg = Aeson.Object . fmap Aeson.String . fmap (fromMaybe "") . HashMap.fromList $ HTTP.parseQueryText strg