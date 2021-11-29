{-# LANGUAGE TypeApplications #-}

module EulerHS.Extra.Aeson
(
  jsonSetField
-- , encodeJSON
-- , decodeJSON
, obfuscate
-- , encodeT
-- , eitherDecodeT
-- for JSON instances of storage types
, jsonNumberToString
, jsonStringToNumber
, updateJSONString
-- aeson options
, aesonOmitNothingFields
, untaggedOptions
, stripLensPrefixOptions
, stripAllLensPrefixOptions
, unaryRecordOptions
) where

import qualified Juspay.Extra.Text as Extra

import           Prelude

import           Data.Aeson (Options (..), ToJSON (..), Value (..), decode,
                             defaultOptions)
import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict as HashMap
import           Data.Maybe (fromJust)
import           Data.Scientific (floatingOrInteger)
import           Data.Text (Text)
import qualified Data.Text.Encoding as Text



-- utility functions

-- | Set a field inside a JSON Object
jsonSetField :: ToJSON a => Text -> a -> Aeson.Value -> Aeson.Value
jsonSetField fieldName fieldValue obj = case obj of
  Aeson.Object fields ->
    Aeson.Object $ HashMap.insert fieldName (Aeson.toJSON fieldValue) fields
  _ ->
    error $ "This should be an object... got " <> show obj

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

-------------------------------------------------------------------------------
-- Aeson options
-- tests test/extra/Options.hs
-------------------------------------------------------------------------------

-- Use aesonOmitNothingFields to omit Nothing fields.
-- Former aesonOrderCreateOptions, aesonOmitNothingOption and broken aesonOptions
-- aesonOptions broken because it used False, which default in aeson.
-- If you want to show Nothing fields then just use defaultOptions, please!
-- With options turned on you got:
-- Person "Omar" Nothing
-- "{\"name\":\"Omar\"}"
-- while default behavior is
-- "{\"age\":null,\"name\":\"Omar\"}"
aesonOmitNothingFields :: Options
aesonOmitNothingFields = defaultOptions
  { omitNothingFields = True -- hey! It should be True. We relay on it.
  }

-- It strips a prefix of record's fields
{-
data Dog = Dog
  { cName :: Text
  , cColour :: Maybe Text
  }
Dog "Buddy" (Just "white") -> "{\"Name\":\"Buddy\",\"Colour\":\"white\"}"
-}
stripLensPrefixOptions :: Options
stripLensPrefixOptions = defaultOptions { fieldLabelModifier = drop 1 }

-- It drops a prefix while it equal to head
{-
data Wooolf = Wooolf
  { cccName :: Text
  , cccColour :: Maybe Text
  }
Wooolf "Boooss" (Just "grey") -> "{\"Name\":\"Boooss\",\"Colour\":\"grey\"}"
-}
stripAllLensPrefixOptions :: Options
stripAllLensPrefixOptions = defaultOptions { fieldLabelModifier = dropPrefix}
  where
    dropPrefix :: String -> String
    dropPrefix field = if length field > 0
                         then dropWhile (== head field) field
                         else field

-- It reduces encoding of a body with a constructor to just body,
-- and throwing away a constructor.
-- To decode the resulted json use `unaryRecordOptions`
-- See tests for more examples test/extra/Options.hs
untaggedOptions :: Options
untaggedOptions = defaultOptions
  { sumEncoding = Aeson.UntaggedValue
  }

-- When a record wrapped to the constructor,
-- the former one encoded with 'contents' key, the latter one - with 'tag' key.
-- If one passes a record to decoding WITHOUT 'tag' and 'contents'
-- it consumed when unaryRecordOptions is set, otherwise it fails.
-- To encode json without tag use `untaggedOptions`
-- See tests for more examples test/extra/Options.hs
unaryRecordOptions :: Options
unaryRecordOptions = defaultOptions
  { unwrapUnaryRecords = True
  }
