module EulerHS.Framework.Types.Options
  (
    -- * Options
    -- | Determine the relationship between key & value
    OptionEntity
    -- * Make option key
  , mkOptionKey
  ) where

import           EulerHS.Prelude
import           Type.Reflection (typeRep)
import           Data.Aeson(encode)
import qualified Data.ByteString.Lazy as BSL

class (Typeable k, FromJSON k, ToJSON k, FromJSON v, ToJSON v)
  => OptionEntity k v |  k -> v

mkOptionKey :: forall k v. OptionEntity k v => k -> Text
mkOptionKey k = show (typeRep @k) <> (decodeUtf8 $ BSL.toStrict $ encode k)
