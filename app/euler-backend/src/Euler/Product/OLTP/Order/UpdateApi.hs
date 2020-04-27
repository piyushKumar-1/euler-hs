module Euler.Product.OLTP.Order.UpdateApi
  ( -- * Abstract handle
    SHandle(..)
    -- * Service configuration
--  , Config(..)
--  , defaultConfig
    -- * Re-export order status API types
  , module Euler.API.Order
  ) where

import           EulerHS.Language

import           Euler.API.RouteParameters (RouteParameters)
import           Euler.API.Order



data SHandle = SHandle
    { updateOrder :: !(RouteParameters -> OrderUpdateRequest -> Flow OrderStatusResponse)
    }


--data Config = Config
--    { asyncSlowPath :: !Bool
--    }
--    deriving (Eq, Show)


--defaultConfig :: Config
--defaultConfig = Config { asyncSlowPath = False }
