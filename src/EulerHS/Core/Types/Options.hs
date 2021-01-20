module EulerHS.Core.Types.Options
  (
    -- * Options
    -- | Determine the relationship between key & value
    OptionEntity
    -- * Make option key
  , mkOptionKey
  ) where

import           EulerHS.Prelude

class Show k
  => OptionEntity k v | k -> v

mkOptionKey :: forall k v. OptionEntity k v => k -> Text
mkOptionKey = show

