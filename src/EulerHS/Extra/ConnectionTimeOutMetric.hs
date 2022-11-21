{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DerivingStrategies  #-}

module EulerHS.Extra.ConnectionTimeOutMetric where

import qualified EulerHS.Types as T
import           Euler.Events.MetricApi.MetricApi
import qualified EulerHS.Framework.Language as L
import           EulerHS.Prelude

incrementConnectionTimeOutMetric :: L.MonadFlow m => ConnectionTimeOutMetricHandle -> TimeoutCounter -> Text -> m ()
incrementConnectionTimeOutMetric handle metric name = do
  L.runIO $ handle.connectTimeoutCounter (metric, name)

data ConnectionTimeOutMetricHandle = ConnectionTimeOutMetricHandle
  { connectTimeoutCounter :: (TimeoutCounter, Text) -> IO ()
  }

data TimeoutCounter
  = RedisConnectionTimeout
  | DBConnectionTimeout

mkConnectionTimeOutHandle :: IO ConnectionTimeOutMetricHandle
mkConnectionTimeOutHandle = do
  metrics <- register collectionLock
  pure $ ConnectionTimeOutMetricHandle $ \case
    (RedisConnectionTimeout, redisName)   ->
      inc (metrics </> #redis_connection_timeout) redisName
    (DBConnectionTimeout,dbName)    ->
      inc (metrics </> #db_connection_timeout) dbName

redis_connection_timeout = counter #redis_connection_timeout
      .& lbl @"redis_name" @Text
      .& build

db_connection_timeout = counter #db_connection_timeout
      .& lbl @"db_name" @Text
      .& build

collectionLock =
     redis_connection_timeout
  .> db_connection_timeout
  .> MNil


------------------------------------------------------------

data EulerRedisCfg = EulerRedisCfg
  deriving stock (Generic, Typeable, Show, Eq)
  deriving anyclass (ToJSON, FromJSON)

instance T.OptionEntity EulerRedisCfg ConnectionTimeOutMetricHandle
