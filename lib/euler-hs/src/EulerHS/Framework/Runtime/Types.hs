module EulerHS.Framework.Runtime.Types
  ( Runtime(..)
  ) where

import           EulerHS.Prelude
import           Data.Map                        (Map)
import           Network.HTTP.Client             (Manager)

data Runtime = Runtime
  { options :: MVar (Map ByteString ByteString)
  , networkManager :: Manager
  }