module Euler.Product.Cache.OrderStatusCacheImpl where

import           EulerHS.Prelude

import           EulerHS.Language
import           WebService.Language

import           Euler.Lens
import           Euler.API.Order
import qualified Euler.Common.Metric      as Metric
import qualified Euler.Constants          as Constants (ecRedis)
import qualified Euler.Constant.Feature   as FeatureC
import           Euler.Product.Cache.OrderStatusCacheApi
import           Euler.Storage.Repository.Feature


mkHandle
  :: Config
  -> SHandle
mkHandle config = SHandle
  { addToCache = \orderId merchId isAuth res -> addToCache' orderId merchId isAuth res (orderStatusCacheTTL config)
  , getCachedResponse = getCachedResponse'
  , invalidateCache = invalidateCache'
  }


-- | Run an 'Flow' action with access to an 'SHandle'.
withHandle
    :: Config                 -- ^ Configuration
    -> (SHandle -> Flow a)    -- ^ Inner action
    -> Flow a
withHandle config action = do
    action $ SHandle
      { addToCache = \orderId merchId isAuth res -> addToCache' orderId merchId isAuth res (orderStatusCacheTTL config)
      , getCachedResponse = getCachedResponse'
      , invalidateCache = invalidateCache'
      }


-- | Cache an order status response
addToCache'
  -- EHS: clarify types
  :: Text -- C.OrderPId           -- ^ aks @willbasky PId or Id ?
  -> Text -- D.MerchantAccountId
  -> Bool
  -> OrderStatusResponse
  -> Integer                  -- ^ TTL
  -> Flow ()
addToCache' orderId merchId isAuth res ttl = do
  mbFeat <- loadFeature FeatureC.EulerOrderStatusCaching $ merchId
  let caching = maybe False (^. _enabled) mbFeat
  case caching of
    True  -> do
      _ <- logInfoT "Order Status add to cache"
            $ "adding order status response to cache for merchant_id "
              <> merchId <> " orderId " <> orderId
      _ <- rSetex Constants.ecRedis cacheKey res ttl
      runIO $ Metric.incrementOrderStatusCacheAddCount merchId
      pure ()
    False -> pure ()
  where
    cacheKey = mkCacheKey orderId merchId isAuth


-- | Try to retrieve a cached response
getCachedResponse'
  :: Text
  -> Text
  -> Bool
  -> Flow (Maybe OrderStatusResponse)
getCachedResponse' orderId merchId isAuth = do
  mbFeat <- loadFeature FeatureC.EulerOrderStatusCaching $ merchId
  let caching = maybe False (^. _enabled) mbFeat
  case caching of
    True  -> do
      _ <- logInfoT "Fetch cache from order status"
             $ "Order status cache feature is enabled for merchand id: " <> merchId
      val <- rGet Constants.ecRedis cacheKey
      -- EHS: investigate
      -- let resp = fromMaybe (toForeign "") (parseAndReplaceWithStringNull Just Nothing v)
      -- _ <- Presto.log ("Cache value for this order status cache key " <> key) v
      -- case (runExcept (decode (camelCaseToSnakeCase resp))) of
      --  Right typedVal -> pure (replaceObjValWithForeignNull typedVal Just Nothing)
      --  Left err -> log "decode_error" ("Error while decoding cached value for " <> key <> "_" <> show err) *> pure Nothing
      case val of
        Just val' -> do
          _ <- logInfoT "Fetch cache from order status"
                 $ "Order status response found in cache for merchantId: " <> merchId <> ", orderId: " <> orderId
          runIO $ Metric.incrementOrderStatusCacheHitCount merchId
          pure val'
        Nothing -> do
          _ <- logInfoT "Fetch cache from order status"
                 $ "Could not find order status response in cache for merchantId: " <> merchId <> ", orderId: " <> orderId
          runIO $ Metric.incrementOrderStatusCacheMissCount merchId
          pure Nothing
    False -> do
      _ <- logInfoT "Fetch cache from order status"
             $ "Order status cache feature is not enabled for merchand id: " <> merchId
      pure Nothing
  where
      cacheKey = mkCacheKey orderId merchId isAuth


-- | Invalidate all possible keys for order status cached data
invalidateCache' :: Text -> Text -> Flow ()
invalidateCache' orderId merchantId = do
  -- EHS: logs conformity?
  logInfoT "invalidateOrderStatusCacheStart"
    $ "Invalidating order status cache for " <> merchantId <> " and order_id " <> orderId
  void $ rDel Constants.ecRedis $ mkCacheKey' orderId merchantId <$> allPrefs

-- ----------------------------------------------------------------------------
-- Redis keys related stuff

eulerOStatusPrefix :: IsString a => a
eulerOStatusPrefix = "euler_ostatus_"

eulerOStatusUnAuthPrefix :: IsString a => a
eulerOStatusUnAuthPrefix = "euler_ostatus_unauth_"

oStatusPrefix :: IsString a => a
oStatusPrefix = "ostatus_"

oStatusUnAuthPrefix :: IsString a => a
oStatusUnAuthPrefix = "ostatus_unauth_"

allPrefs :: IsString a => [a]
allPrefs = [eulerOStatusPrefix, eulerOStatusUnAuthPrefix, oStatusPrefix, oStatusUnAuthPrefix]

mkCacheKey :: Text -> Text -> Bool -> Text
mkCacheKey orderId merchId True  = mkCacheKey' orderId merchId eulerOStatusPrefix
mkCacheKey orderId merchId False = mkCacheKey' orderId merchId eulerOStatusUnAuthPrefix

mkCacheKey' :: Text -> Text -> Text -> Text
mkCacheKey' orderId merchId pref = pref <> merchId <> "_" <> orderId
