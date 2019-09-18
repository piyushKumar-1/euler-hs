module EulerHS.Framework.Types.Options
  ( OptionEntity
  ) where

import           EulerHS.Prelude

class (FromJSON k, FromJSON v, ToJSON k, ToJSON v) => OptionEntity k v |  k -> v