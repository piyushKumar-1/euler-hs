module EulerHS.Extra.Aeson
(
 jsonSetField
, encodeJSON
, decodeJSON
, obfuscate
) where

import           Prelude

import           Data.Aeson (FromJSON, ToJSON, Value (..))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Text as Aeson
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.HashMap.Strict as HashMap
import           Data.Text (Text)
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Lazy as LazyText

-- utility functions

-- | Set a field inside a JSON Object
jsonSetField :: ToJSON a => Text -> a -> Aeson.Value -> Aeson.Value
jsonSetField fieldName fieldValue obj = case obj of
  Aeson.Object fields ->
    Aeson.Object $ HashMap.insert fieldName (Aeson.toJSON fieldValue) fields
  _ ->
    error $ "This should be an object... got " <> show obj

-- | Encode a value to JSON Text
--
-- Note: the name `jsonEncode` is already taken by Aeson
encodeJSON :: ToJSON a => a -> Text
encodeJSON = LazyText.toStrict . Aeson.encodeToLazyText

-- | Parse JSON Text into a value
decodeJSON :: FromJSON a => Text -> Maybe a
decodeJSON = Aeson.decode . LazyByteString.fromStrict . Text.encodeUtf8

-- | Rip away all values from a Value
obfuscate :: Value -> Value
obfuscate v = go v where
    go (Object o) = Object $ go <$> o
    go (Array a) = Array $ go <$> a
    go (String  _) = String "***"
    go (Number _) = Number 0
    go (Bool _) = Bool False
    go Null = Null

