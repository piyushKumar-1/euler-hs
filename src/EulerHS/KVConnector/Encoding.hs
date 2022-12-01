{-# LANGUAGE OverloadedStrings #-}
module EulerHS.KVConnector.Encoding
  (
    encode,
    decode,
    eitherDecode,
    encodeLiveOrDead,
    decodeLiveOrDead
  )
 where

import           EulerHS.Prelude
import qualified Data.Aeson as Aeson
import qualified Data.Serialize as Cereal
import qualified Data.ByteString.Lazy as BSL
import Data.Cereal.Instances ()

encode :: (Aeson.ToJSON a, Cereal.Serialize a) => Bool -> a -> BSL.ByteString
encode isEnabled val =
  if isEnabled
     then BSL.fromStrict $ "CBOR" <> Cereal.encode val
     else "JSON" <> Aeson.encode val

eitherDecode :: (Aeson.FromJSON a, Cereal.Serialize a) => BSL.ByteString -> Either String a
eitherDecode val =
  let (h, v) = BSL.splitAt 4 val
   in case h of
        "CBOR" -> Cereal.decodeLazy v
        "JSON" -> Aeson.eitherDecode v
        _ -> Aeson.eitherDecode val

decode :: (Aeson.FromJSON a, Cereal.Serialize a) => BSL.ByteString -> Maybe a
decode = hush . eitherDecode
  where
    hush (Right a) = Just a
    hush _ = Nothing


-- LIVE/DEAD marker for values

encodeLiveOrDead :: Bool -> BSL.ByteString -> BSL.ByteString
encodeLiveOrDead isLive val = 
  if isLive
    then "LIVE" <> val
    else "DEAD" <> val

decodeLiveOrDead :: BSL.ByteString -> Maybe (Bool, BSL.ByteString)
decodeLiveOrDead val =
  let (h, v) = BSL.splitAt 4 val
    in case h of
      "LIVE" -> Just (True , v)
      "DEAD" -> Just (False, v)
      _   -> Nothing
