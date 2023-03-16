module EulerHS.Extra.Monitoring.Flow where

import           EulerHS.Prelude
import qualified EulerHS.Framework.Language as L
import qualified EulerHS.Extra.Monitoring.Types as EEMT
import           Data.Time.Clock.POSIX (getPOSIXTime)

getCurrentDateInMillisIO :: IO Int
getCurrentDateInMillisIO = do
  t <- (* 1000) <$> getPOSIXTime
  pure . floor $ t

incrementDBLatencyMetric :: (HasCallStack, L.MonadFlow m) => Int -> m ()
incrementDBLatencyMetric latency = do
    mSystemEntry <- L.getOptionLocal EEMT.DBMetricInfoKey
    case mSystemEntry of
        Just (EEMT.DBMetricInfo (EEMT.LatencyInfo oldLatency count))  -> do 
            L.setOptionLocal EEMT.DBMetricInfoKey $ EEMT.DBMetricInfo (EEMT.LatencyInfo (oldLatency + latency) (count + 1))
        Nothing                                   -> pure ()

incrementRedisLatencyMetric :: (HasCallStack, L.MonadFlow m) => Int -> m ()
incrementRedisLatencyMetric latency = do
    mSystemEntry <- L.getOptionLocal EEMT.RedisMetricInfoKey
    case mSystemEntry of
        Just (EEMT.RedisMetricInfo (EEMT.LatencyInfo oldLatency count))  -> do 
            L.setOptionLocal EEMT.RedisMetricInfoKey $ EEMT.RedisMetricInfo (EEMT.LatencyInfo (oldLatency + latency) (count + 1))
        Nothing                                   -> pure ()

incrementAPILatencyMetric :: (HasCallStack, L.MonadFlow m) => Int -> m ()
incrementAPILatencyMetric latency = do
    mSystemEntry <- L.getOptionLocal EEMT.APIMetricInfoKey
    case mSystemEntry of
        Just (EEMT.APIMetricInfo (EEMT.LatencyInfo oldLatency count))  -> do 
            L.setOptionLocal EEMT.APIMetricInfoKey $ EEMT.APIMetricInfo (EEMT.LatencyInfo (oldLatency + latency) (count + 1))
        Nothing                                   -> pure ()