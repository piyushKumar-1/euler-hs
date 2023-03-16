module EulerHS.Extra.Monitoring.Flow where

import           EulerHS.Prelude
import qualified Data.Aeson as A
import qualified EulerHS.Framework.Language as L
import qualified EulerHS.Extra.Monitoring.Types as EEMT
import           Data.Time.Clock.POSIX (getPOSIXTime)
import qualified Juspay.Extra.Config as Conf

isLatecyMetricEnabled :: Bool
isLatecyMetricEnabled = fromMaybe False $ readMaybe =<< Conf.lookupEnvT "LATENCY_METRIC_ENABLED"

getCurrentDateInMillisIO :: IO Int
getCurrentDateInMillisIO = do
  t <- (* 1000) <$> getPOSIXTime
  pure . floor $ t

defaultLatencyMetric :: EEMT.LatencyInfo
defaultLatencyMetric = EEMT.LatencyInfo 0 0

incrementDBLatencyMetric :: (HasCallStack, L.MonadFlow m) => Int -> m ()
incrementDBLatencyMetric latency = when isLatecyMetricEnabled $ do
    (EEMT.LatencyInfo oldLatency count) <- maybe defaultLatencyMetric (\(EEMT.DBMetricInfo x) -> x) <$> L.getOptionLocal EEMT.DBMetricInfoKey
    L.setOptionLocal EEMT.DBMetricInfoKey $ EEMT.DBMetricInfo (EEMT.LatencyInfo (oldLatency + latency) (count + 1))

incrementRedisLatencyMetric :: (HasCallStack, L.MonadFlow m) => Int -> m ()
incrementRedisLatencyMetric latency = when isLatecyMetricEnabled $  do
    (EEMT.LatencyInfo oldLatency count) <- maybe defaultLatencyMetric (\(EEMT.RedisMetricInfo x) -> x) <$> L.getOptionLocal EEMT.RedisMetricInfoKey
    L.setOptionLocal EEMT.RedisMetricInfoKey $ EEMT.RedisMetricInfo (EEMT.LatencyInfo (oldLatency + latency) (count + 1))

incrementAPILatencyMetric :: (HasCallStack, L.MonadFlow m) => Int -> m ()
incrementAPILatencyMetric latency = when isLatecyMetricEnabled $ do
    (EEMT.LatencyInfo oldLatency count) <- maybe defaultLatencyMetric (\(EEMT.APIMetricInfo x) -> x) <$> L.getOptionLocal EEMT.APIMetricInfoKey
    L.setOptionLocal EEMT.APIMetricInfoKey $ EEMT.APIMetricInfo (EEMT.LatencyInfo (oldLatency + latency) (count + 1))

getLatencyMetric :: EEMT.LatencyInfo -> A.Value 
getLatencyMetric (EEMT.LatencyInfo latency count) = A.toJSON (EEMT.LatencyLogInfo count latency (latency `div` count))

logLatencyMetricLog :: (HasCallStack, L.MonadFlow m) => m ()
logLatencyMetricLog = do
    dbMetric    <- ((fromMaybe A.Null) . ((getLatencyMetric . (\(EEMT.DBMetricInfo x) -> x)) <$>)) <$> L.getOptionLocal EEMT.DBMetricInfoKey
    redisMetric <- ((fromMaybe A.Null) . ((getLatencyMetric . (\(EEMT.RedisMetricInfo x) -> x)) <$>)) <$> L.getOptionLocal EEMT.RedisMetricInfoKey
    apiMetric    <- ((fromMaybe A.Null) . ((getLatencyMetric . (\(EEMT.APIMetricInfo x) -> x)) <$>)) <$> L.getOptionLocal EEMT.APIMetricInfoKey
    L.logInfoV ("LATENCY_METRIC" :: Text) (A.object $ [("dbMetric",dbMetric),("redisMetric",redisMetric),("apiMetric",apiMetric)])