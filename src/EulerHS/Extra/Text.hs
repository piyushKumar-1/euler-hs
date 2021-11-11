

module EulerHS.Extra.Text where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import           Data.Text (unpack)
import           EulerHS.Prelude hiding (unpack)

-- ByteString

toLazy :: BS.ByteString -> BSL.ByteString
toLazy s = BSL.fromChunks [s]

-- Text

-- former readMay
-- | readMaybe for Text
readMaybeT :: Read a => Text -> Maybe a
readMaybeT = readMaybe . unpack
