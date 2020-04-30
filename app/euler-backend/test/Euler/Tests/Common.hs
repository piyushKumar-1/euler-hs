module Euler.Tests.Common where

import           EulerHS.Prelude

import           Euler.AppEnv
import qualified EulerHS.Types as T

import qualified Euler.Product.Cache.OrderStatusCacheApi as CacheApi
import qualified Euler.Product.Cache.OrderStatusCacheImpl as CacheImpl
import qualified Euler.Product.OLTP.Order.OrderStatus as OrderStatusImpl
import qualified Euler.Product.OLTP.Order.StatusApi as StatusApi
import qualified Euler.Product.OLTP.Order.Update as UpdateImpl
import qualified Euler.Product.OLTP.Services.AuthConf as AuthConf


mkAppEnv :: AppEnv
mkAppEnv = mkAppEnv' False

mkAppEnv' :: Bool -> AppEnv
mkAppEnv' isAsync =
  let keyAuthService = AuthConf.mkKeyAuthService
      keyTokenAuthService = AuthConf.mkKeyTokenAuthService
      orderStatusCacheService = CacheImpl.mkHandle CacheApi.defaultConfig
      orderStatusConfig = StatusApi.defaultConfig { StatusApi.asyncSlowPath = isAsync }
      orderStatusService = OrderStatusImpl.mkHandle orderStatusConfig keyTokenAuthService orderStatusCacheService
  in AppEnv
    { keyAuthH = keyAuthService
    , tokenAuthH = AuthConf.mkTokenAuthService
    , keyTokenAuthH = keyTokenAuthService
    , orderStatusCacheH = orderStatusCacheService
    , orderStatusH = orderStatusService
    , orderUpdateH = UpdateImpl.mkHandle $ UpdateImpl.IHandle keyAuthService orderStatusCacheService orderStatusService
    }


mySQLCfg :: String -> T.MySQLConfig
mySQLCfg testDBName = T.MySQLConfig
  { connectHost     = "mysql"
  , connectPort     = 3306
  , connectUser     = "cloud"
  , connectPassword = "scape"
  , connectDatabase = testDBName
  , connectOptions  = [T.CharsetName "utf8"]
  , connectPath     = ""
  , connectSSL      = Nothing
  }

mySQLRootCfg :: String -> T.MySQLConfig
mySQLRootCfg testDBName =
    T.MySQLConfig
      { connectUser     = "root"
      , connectPassword = "root" -- use your local password when test out of docker
      , connectDatabase = ""
      , ..
      }
  where
    T.MySQLConfig {..} = mySQLCfg testDBName


-- Redis config data
redisConn :: IsString a => a
redisConn = "redis"

redisConnConfig :: T.RedisConfig
redisConnConfig = T.RedisConfig
    { connectHost           = "redis"
    , connectPort           = 6379
    , connectAuth           = Nothing
    , connectDatabase       = 0
    , connectMaxConnections = 50
    , connectMaxIdleTime    = 30
    , connectTimeout        = Nothing
    }
