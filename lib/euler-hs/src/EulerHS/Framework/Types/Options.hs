module EulerHS.Framework.Types.Options
  (
    -- * Options
    -- | Determine the relationship between key & value
    OptionEntity
  ) where

import           EulerHS.Prelude

class (Typeable k, FromJSON v, ToJSON v) => OptionEntity k v |  k -> v