module Euler.Product.Cache.OrderStatusCacheApi
  ( -- * Abstract handle
    SHandle(..)

    -- * Pure types
    -- Here you export types that are necessary to interact with service X,
    -- but are too specific to fit into their own module, e.g., the priorities
    -- above in the logger service.
  , Config(..)
  , defaultConfig
    -- * Derived functions
    -- Here you export derived functions that work for any implementation of
    -- service X.
    -- They are typically of the shape `fun :: Handle -> params -> IO result`
  -- , withHandle
  -- * convenient functions
  --, runStatus
  ) where

import           EulerHS.Prelude

import           EulerHS.Language

import           Euler.API.Order                              as AO
import qualified Euler.Constants                              as Constants (orderStatusCacheTTL)



data SHandle = SHandle
    { addToCache        :: !(Text -> Text -> Bool -> AO.OrderStatusResponse -> Flow ())
    , getCachedResponse :: !(Text -> Text -> Bool -> Flow (Maybe AO.OrderStatusResponse))
    , invalidateCache   :: !(Text -> Text -> Flow ())
    }

data Config = Config
  { orderStatusCacheTTL :: !Integer
  }
    deriving (Eq, Show)

defaultConfig :: Config
defaultConfig = Config
  { orderStatusCacheTTL = Constants.orderStatusCacheTTL
  }