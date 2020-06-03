module EulerHS.Core.Types.BinaryString
( BinaryString
, LBinaryString(..)
) where

import EulerHS.Prelude

import qualified Data.ByteString        as Strict
import qualified Data.ByteString.Lazy   as Lazy
import qualified Data.ByteString.Base64 as B64
import qualified Data.Text              as Text
import qualified Data.Text.Encoding     as Encoding
import qualified Control.Monad.Fail     as MonadFail

-- TODO: Move to euler-db

--------------------------------------------------------------------------
-- Base64 encoding/decoding helpers
--------------------------------------------------------------------------

newtype BinaryString
  = BinaryString
    { getBinaryString :: Strict.ByteString }
    deriving (Show, Eq, Ord)

instance ToJSON BinaryString where
  toJSON = toJSON . base64Encode . getBinaryString

instance FromJSON BinaryString where
  parseJSON val = pure . BinaryString =<< base64Decode =<< parseJSON val


--------------------------------------------------------------------------
-- Lazy BinaryString
--------------------------------------------------------------------------

newtype LBinaryString 
  = LBinaryString
    { getLBinaryString :: Lazy.ByteString }
    deriving (Show, Eq, Ord)

instance ToJSON LBinaryString where
  toJSON = toJSON . base64Encode . Lazy.toStrict . getLBinaryString

instance FromJSON LBinaryString where
  parseJSON val =
    pure . LBinaryString . Lazy.fromStrict =<< base64Decode =<< parseJSON val

--------------------------------------------------------------------------
-- Base64 encoding/decoding helpers
--------------------------------------------------------------------------

-- | Base64 encode a bytestring
--
-- NOTE: Decoding to UTF-8 cannot fail so is safe
--
base64Encode :: Strict.ByteString -> Text.Text
base64Encode = Encoding.decodeUtf8 . B64.encode

-- | Base64 decode a Base64-encoded string
--
-- NOTE: This may fail if the string is malformed using MonadFail
--
base64Decode :: MonadFail.MonadFail m => Text.Text -> m Strict.ByteString
base64Decode s = case B64.decode (Encoding.encodeUtf8 s) of
  Left err -> fail err
  Right res -> return res