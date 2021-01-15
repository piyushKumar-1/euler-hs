module EulerHS.Core.Masking where


import qualified Data.Aeson as Aeson
import           EulerHS.Prelude
import qualified Network.HTTP.Types as HTTP
import qualified Data.HashMap.Strict as HashMap
import           Data.HashSet (member)
import qualified EulerHS.Core.Types.Logger as Log (LogMaskingConfig(..), MaskKeyType (..))
import qualified Data.CaseInsensitive as CI

shouldMaskKey :: Maybe Log.LogMaskingConfig -> Text -> Bool
shouldMaskKey Nothing _ = False
shouldMaskKey (Just Log.LogMaskingConfig{..}) key =
  case _keyType of
    Log.WhiteListKey -> not $ member key _maskKeys
    Log.BlackListKey -> member key _maskKeys

defaultMaskText :: Text
defaultMaskText = "***"

maskHeaders :: (Text -> Bool) -> Text -> Seq HTTP.Header -> Seq HTTP.Header
maskHeaders shouldMask maskText headers = maskHeader <$> headers
  where
    maskHeader :: HTTP.Header -> HTTP.Header
    maskHeader (headerName,headerValue) =
      if shouldMask (decodeUtf8 $ CI.original headerName)
        then (headerName,encodeUtf8 maskText)
        else (headerName,headerValue)

maskQueryStrings :: (Text -> Bool) -> Text -> Seq HTTP.QueryItem -> Seq HTTP.QueryItem
maskQueryStrings shouldMask maskText queryStrings = maskQueryString <$> queryStrings
  where
    maskQueryString :: HTTP.QueryItem -> HTTP.QueryItem
    maskQueryString (key,value) =
      if shouldMask (decodeUtf8 key)
        then (key,Just $ encodeUtf8 maskText)
        else (key,value)

parseRequestResponseBody :: (Text -> Bool) -> Text -> ByteString -> Text
parseRequestResponseBody shouldMask maskText req =
  case Aeson.eitherDecodeStrict req of
    Right value ->  decodeUtf8 . Aeson.encode $ maskJSON shouldMask maskText value
    Left _ -> decodeUtf8 . Aeson.encode $ maskJSON shouldMask maskText $ handleQueryString req

maskJSON :: (Text -> Bool) -> Text -> Aeson.Value -> Aeson.Value
maskJSON shouldMask maskText (Aeson.Object r) = Aeson.Object $ handleObject shouldMask maskText r
maskJSON shouldMask maskText (Aeson.Array r) =  Aeson.Array $ maskJSON shouldMask maskText <$> r
maskJSON _ _ value = value

handleObject :: (Text -> Bool) -> Text -> Aeson.Object -> Aeson.Object
handleObject shouldMask maskText = HashMap.mapWithKey maskingFn
  where
    maskingFn key value = maskJSON shouldMask maskText $ updatedValue key value
    updatedValue key fn = if shouldMask key then Aeson.String maskText else fn

handleQueryString :: ByteString -> Aeson.Value
handleQueryString strg = Aeson.Object . fmap (Aeson.String . fromMaybe "") . HashMap.fromList $ HTTP.parseQueryText strg