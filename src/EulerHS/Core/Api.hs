{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}


module EulerHS.Core.Api where
import qualified Data.Aeson as Aeson
import           EulerHS.Prelude
import qualified Servant.Client as SC
import qualified Servant.Client.Core as SCC
import           Servant.Client.Core.RunClient (RunClient)
import qualified Servant.Client.Free as SCF
import qualified Servant.Client.Internal.HttpClient as SCIHC
import qualified Network.HTTP.Types as HTTP
import Network.HTTP.Types (parseQueryText)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.ByteString.Lazy as LBS(toStrict)
import qualified Data.Text as Text (unpack)
import Data.HashSet (member)
import qualified EulerHS.Core.Types.Logger as Log (LogMaskingConfig(..), MaskKeyType (..))

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

-- newtype EulerBaseUrl = EulerBaseUrl SCC.BaseUrl

client :: SC.HasClient EulerClient api => Proxy api -> SC.Client EulerClient api
client api = SCC.clientIn api $ Proxy @EulerClient

interpretClientF :: (String -> IO ()) -> Maybe Log.LogMaskingConfig -> SCC.BaseUrl -> SCF.ClientF a -> SC.ClientM a
interpretClientF _   _   _ (SCF.Throw e)             = throwM e
interpretClientF log mbMaskConfig bUrl (SCF.RunRequest req next) = do
  liftIO $ logServantRequest log mbMaskConfig bUrl req
  res <- SCC.runRequestAcceptStatus Nothing req
  liftIO . log $ show res
  pure $ next res

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
          SCC.RequestBodyBS s -> Text.unpack $ parse (shouldMask mbMaskConfig) s 
          SCC.RequestBodyLBS s -> Text.unpack $ parse (shouldMask mbMaskConfig) $ LBS.toStrict s 
          SCC.RequestBodySource sr -> show $ SCC.RequestBodySource sr
      Nothing -> "body = (empty)"

    method = SCC.requestMethod req
    headers = SCC.requestHeaders req
    queryString = SCC.requestQueryString req

    shouldMask :: Maybe Log.LogMaskingConfig -> Text -> Bool
    shouldMask Nothing _ = False
    shouldMask (Just Log.LogMaskingConfig{..}) key = 
      case _keyType of
        Log.WhiteListKey -> not $ member key _maskKeys
        Log.BlackListKey -> member key _maskKeys

runEulerClient :: (String -> IO()) -> Maybe Log.LogMaskingConfig -> SCC.BaseUrl -> EulerClient a -> SCIHC.ClientM a
runEulerClient log mbMaskConfig bUrl (EulerClient f) = foldFree (interpretClientF log mbMaskConfig bUrl) f


parse :: (Text -> Bool) -> ByteString -> Text
parse shouldMask req = 
  case Aeson.eitherDecodeStrict req of
    Right value ->  decodeUtf8 . Aeson.encode $ parse' shouldMask value
    Left _ -> decodeUtf8 . Aeson.encode $ parse' shouldMask $ handleQueryString req

parse' :: (Text -> Bool) -> Aeson.Value -> Aeson.Value
parse' shouldMask (Aeson.Object r) = (Aeson.Object $ handleObject shouldMask r)
parse' shouldMask (Aeson.Array r) =  (Aeson.Array $ parse' shouldMask <$> r)
parse' _ (Aeson.String r) = (Aeson.String r)
parse' _ r = r

handleObject :: (Text -> Bool) -> Aeson.Object -> Aeson.Object
handleObject shouldMask obj = HashMap.mapWithKey maskingFn obj
  where
    maskingFn key value = parse' shouldMask $ updatedValue key value
    updatedValue key fn = if shouldMask key then Aeson.String "***" else fn

handleQueryString :: ByteString -> Aeson.Value
handleQueryString strg = Aeson.Object . fmap Aeson.String . fmap (fromMaybe "") . HashMap.fromList $ parseQueryText strg 