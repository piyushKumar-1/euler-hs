{-# LANGUAGE TypeApplications #-}

module EulerHS.Extra.Aeson
(
 jsonSetField
, encodeJSON
, decodeJSON
, obfuscate
-- for JSON instances of storage types
, jsonNumberToString
, jsonStringToNumber
, updateJSONString
) where

import           Prelude

import           Data.Aeson (FromJSON, ToJSON (..), Value (..), decode)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Text as Aeson
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.HashMap.Strict as HashMap
import           Data.Maybe (fromJust)
import           Data.Scientific (floatingOrInteger)
import           Data.Text (Text)
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Lazy as LazyText
import qualified EulerHS.Extra.Text as Extra



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
    go (Object o)  = Object $ go <$> o
    go (Array a)   = Array $ go <$> a
    go (String  _) = String "***"
    go (Number _)  = Number 0
    go (Bool _)    = Bool False
    go Null        = Null


-- | Helpers for Storage's JSON instances

-- | Convert a string just from an integral number inside Value
jsonNumberToString :: Value -> Value
jsonNumberToString =  \case
  -- TODO should be @Int64 I suppose
  (Number n) -> case floatingOrInteger @Double @Int n of
    Right i -> toJSON $ show i
    Left _  -> error "not an integral"
  Null -> Null
  _ -> error "jsonNumberToString: not a number"

-- Convert a string to Int inside  Value
jsonStringToNumber :: Value -> Value
jsonStringToNumber = \case
  (String n) -> toJSON $ fromJust (decode $ Extra.toLazy $ Text.encodeUtf8 n :: Maybe Int)
  Null -> Null
  _ -> error "jsonStringToNumber: not a string"

-- | Update a string inside Value
updateJSONString :: (Text -> Text) -> Value -> Value
updateJSONString f (String t) = String $ f t
updateJSONString _ Null = Null
updateJSONString _ _ = error "updateJSONString: expected a JSON String"
