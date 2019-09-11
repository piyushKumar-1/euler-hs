module EulerHS.Framework.Language.Types
  ( module X
  , Runtime(..)
  , OptionEntity
  ) where

import           EulerHS.Framework.Language.Types.API as X
import           EulerHS.Prelude
import           Data.Map (Map)



class (FromJSON k, FromJSON v, ToJSON k, ToJSON v) => OptionEntity k v |  k -> v

data Runtime = Runtime
  { options :: MVar (Map ByteString ByteString)
  }