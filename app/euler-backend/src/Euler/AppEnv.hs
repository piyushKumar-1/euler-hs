module Euler.AppEnv where

import EulerHS.Prelude

import qualified EulerHS.Language as L
import WebService.Types

import           Euler.API.RouteParameters

-- new style services
import qualified Euler.Product.OLTP.Services.Auth.AuthService as Auth
import qualified Euler.Product.OLTP.Order.StatusApi as Status
import qualified Euler.Product.OLTP.Order.UpdateApi as Update
import qualified Euler.Product.Cache.CacheApi as Cache



data AppEnv = AppEnv
  { keyAuthH          :: Auth.SHandle    -- ^ key-based auth
  , tokenAuthH        :: Auth.SHandle    -- ^ token-based auth
  , keyTokenAuthH     :: Auth.SHandle    -- ^ cobined key-token auth
  , orderStatusCacheH :: Cache.SHandle   -- ^ cache for order status
  , orderStatusH      :: Status.SHandle  -- ^ order status
  , orderUpdateH      :: Update.SHandle  -- ^ order update
  }


orderStatusMethod :: AppEnv -> RouteParameters -> EmptyReq -> L.Flow Status.OrderStatusResponse
orderStatusMethod appEnv = Status.statusById $ orderStatusH appEnv

orderUpdateMethod :: AppEnv -> RouteParameters -> Update.OrderUpdateRequest -> L.Flow Update.OrderStatusResponse
orderUpdateMethod appEnv = Update.updateOrder $ orderUpdateH appEnv