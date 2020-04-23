module Euler.AppEnv
  ( -- * Business-logic configuration
    AppEnv(..)
    -- * Handler keys and show/parse functions
  , HandlerKey(..)
  , showHandlerKey
  , parseHandlerKey
    -- * Specific accessors for handlers to use in servant
  , orderStatusMethod
  , orderUpdateMethod
    -- * CPS-style runner for player
  , runHandlerWith
  ) where

import EulerHS.Prelude

import qualified EulerHS.Language as L
import           WebService.Types
import           WebService.Relude

import           Data.Map as M hiding (map)

import           Euler.API.RouteParameters
import qualified Euler.Product.OLTP.Services.Auth.AuthService as Auth
import qualified Euler.Product.OLTP.Order.StatusApi as Status
import qualified Euler.Product.OLTP.Order.UpdateApi as Update
import qualified Euler.Product.Cache.OrderStatusCacheApi as Cache



-- | Business-logic configuration
data AppEnv = AppEnv
  { keyAuthH          :: Auth.SHandle    -- ^ key-based auth
  , tokenAuthH        :: Auth.SHandle    -- ^ token-based auth
  , keyTokenAuthH     :: Auth.SHandle    -- ^ cobined key-token auth
  , orderStatusCacheH :: Cache.SHandle   -- ^ cache for order status
  , orderStatusH      :: Status.SHandle  -- ^ order status
  , orderUpdateH      :: Update.SHandle  -- ^ order update
  }


-- | Keys for handlers
data HandlerKey
  = OrderStatus
  | OrderUpdate
  deriving (Bounded, Enum, Eq, Ord, Show)

showHandlerKey :: HandlerKey -> Text
showHandlerKey OrderStatus = "orderStatus"
showHandlerKey OrderUpdate = "orderUpdate"

parseHandlerKey :: Text -> Maybe HandlerKey
parseHandlerKey = inverseMap showHandlerKey


-- | Internal handlers wrapper to use in map
data Handler
  = OrderStatusHandler (RouteParameters -> EmptyReq -> L.Flow Status.OrderStatusResponse)
  | OrderUpdateHandler (RouteParameters -> Update.OrderUpdateRequest -> L.Flow Update.OrderStatusResponse)


handlersMap :: AppEnv -> Map HandlerKey Handler
handlersMap appEnv = fromList
  [ (OrderStatus, OrderStatusHandler $ orderStatusMethod appEnv)
  , (OrderUpdate, OrderUpdateHandler $ orderUpdateMethod appEnv)
  ]

handler :: HandlerKey -> AppEnv -> Maybe Handler
handler key appEnv = lookup key $ handlersMap appEnv

-- | CPS-style runner for use with ART-player
runHandlerWith
  :: Text
  -> AppEnv
  -> (forall req resp. (FromJSON req, FromJSON resp, ToJSON resp, Eq resp, Show resp) => (RouteParameters -> req ->  L.Flow resp) -> a)
  -> a
runHandlerWith keyT env cont =
  let  key = case (parseHandlerKey keyT) of
          Just key' -> key'
          Nothing ->  error $ "no handlers registered for " <> show key
  in case (handler key env) of
    Just (OrderStatusHandler h) -> cont h
    Just (OrderUpdateHandler h) -> cont h
    Nothing                     -> error $ "no handlers registered for " <> show key

-- ----------------------------------------------------------------------------
-- Handlers themselves

orderStatusMethod :: AppEnv -> (RouteParameters -> EmptyReq -> L.Flow Status.OrderStatusResponse)
orderStatusMethod appEnv = Status.statusById $ orderStatusH appEnv

orderUpdateMethod :: AppEnv -> (RouteParameters -> Update.OrderUpdateRequest -> L.Flow Update.OrderStatusResponse)
orderUpdateMethod appEnv = Update.updateOrder $ orderUpdateH appEnv