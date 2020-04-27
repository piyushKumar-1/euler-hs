module Euler.Product.OLTP.Order.StatusApi
  ( -- * Abstract handle
    SHandle(..)
    -- * Service configuration
  , Config(..)
  , defaultConfig
    -- * Re-export order status API types
  , module AO
  ) where

import           EulerHS.Prelude

import           EulerHS.Language
import           WebService.Types

import           Euler.API.RouteParameters (RouteParameters)
import           Euler.API.Order                              as AO
-- EHS: implement the request as an API type and get rid of this dependecy
import qualified Euler.Product.Domain                         as D



data SHandle = SHandle
    { statusById      :: !(RouteParameters -> EmptyReq -> Flow AO.OrderStatusResponse)
    , statusByRequest :: !(D.OrderStatusRequest -> Flow AO.OrderStatusResponse)
    }


data Config = Config
    { asyncSlowPath :: !Bool
    }
    deriving (Eq, Show)


defaultConfig :: Config
defaultConfig = Config { asyncSlowPath = False }
