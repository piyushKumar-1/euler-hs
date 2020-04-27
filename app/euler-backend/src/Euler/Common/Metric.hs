module Euler.Common.Metric where

import EulerHS.Prelude
import qualified Prometheus as P


withPrefix :: Text -> Text
withPrefix = ("euler_" <>)

----------------------------------------------------------------------

clientAuthTokenGeneratedCounter :: P.Vector Text P.Counter
clientAuthTokenGeneratedCounter =
    P.unsafeRegister $ P.vector "merchant_id" $ P.counter $ P.Info (withPrefix "client_auth_token_generated") ""


orderStatusCacheHitCounter :: P.Vector Text P.Counter
orderStatusCacheHitCounter =
    P.unsafeRegister $ P.vector "merchant_id" $ P.counter $ P.Info (withPrefix "order_status_cache_hit") ""


orderStatusCacheMissCounter :: P.Vector Text P.Counter
orderStatusCacheMissCounter =
    P.unsafeRegister $ P.vector "merchant_id" $ P.counter $ P.Info (withPrefix "order_status_cache_miss") ""

orderStatusCacheAddCounter :: P.Vector Text P.Counter
orderStatusCacheAddCounter =
    P.unsafeRegister $ P.vector "merchant_id" $ P.counter $ P.Info (withPrefix "order_status_cache_add") ""
----------------------------------------------------------------------


incrementClientAuthTokenGeneratedCount :: Text -> IO ()
incrementClientAuthTokenGeneratedCount merchantId =
    P.withLabel clientAuthTokenGeneratedCounter merchantId P.incCounter

incrementOrderStatusCacheHitCount :: Text -> IO ()
incrementOrderStatusCacheHitCount merchantId =
    P.withLabel orderStatusCacheHitCounter merchantId P.incCounter

incrementOrderStatusCacheMissCount :: Text -> IO ()
incrementOrderStatusCacheMissCount merchantId =
    P.withLabel orderStatusCacheMissCounter merchantId P.incCounter

incrementOrderStatusCacheAddCount :: Text -> IO ()
incrementOrderStatusCacheAddCount merchantId =
    P.withLabel orderStatusCacheAddCounter merchantId P.incCounter