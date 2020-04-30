-----------------------------------------------------------------------------
-- |
-- Player runtime modes static business-logic configuration configuration.
module Euler.Playback.AppEnv where

import EulerHS.Prelude


import           Euler.AppEnv

import qualified Euler.Product.OLTP.Services.AuthConf   as AuthConf

import qualified Euler.Product.OLTP.Order.StatusApi as StatusApi
import qualified Euler.Product.Cache.OrderStatusCacheApi as CacheApi

import qualified Euler.Product.Cache.OrderStatusCacheImpl as CacheImpl
import qualified Euler.Product.OLTP.Order.OrderStatus   as OrderStatusImpl
import qualified Euler.Product.OLTP.Order.Update   as UpdateImpl



mkAppEnv :: AppEnv
mkAppEnv =
  let keyAuthService = AuthConf.mkKeyAuthService
      keyTokenAuthService = AuthConf.mkKeyTokenAuthService
      orderStatusCacheService = CacheImpl.mkHandle CacheApi.defaultConfig
      orderStatusConfig = StatusApi.defaultConfig
      orderStatusService = OrderStatusImpl.mkHandle orderStatusConfig keyTokenAuthService orderStatusCacheService
  in AppEnv
    { keyAuthH = keyAuthService
    , tokenAuthH = AuthConf.mkTokenAuthService
    , keyTokenAuthH = keyTokenAuthService
    , orderStatusCacheH = orderStatusCacheService
    , orderStatusH = orderStatusService
    , orderUpdateH = UpdateImpl.mkHandle $ UpdateImpl.IHandle keyAuthService orderStatusCacheService orderStatusService
    }