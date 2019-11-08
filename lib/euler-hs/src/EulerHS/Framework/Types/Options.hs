module EulerHS.Framework.Types.Options
  ( 
    -- * Options
    -- | Determine the relationship between key & value
    OptionEntity
  ) where

import           EulerHS.Prelude

class (FromJSON k, FromJSON v, ToJSON k, ToJSON v) => OptionEntity k v |  k -> v