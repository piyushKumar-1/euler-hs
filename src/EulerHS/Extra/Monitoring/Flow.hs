{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module EulerHS.Extra.Monitoring.Flow where

import           EulerHS.Prelude
import qualified Data.Aeson as A
import qualified EulerHS.Framework.Language as L
import qualified EulerHS.Framework.Runtime as R
import qualified EulerHS.Extra.Monitoring.Types as EEMT
import           Data.Time.Clock.POSIX (getPOSIXTime)
import           Data.Time.Clock (nominalDiffTimeToSeconds)
import           Data.Fixed (Fixed (MkFixed))
import qualified Juspay.Extra.Config as Conf
import qualified Data.Map as Map
import           Unsafe.Coerce (unsafeCoerce)
import           EulerHS.Options (OptionEntity, mkOptionKey)

isLatecyMetricEnabled :: Bool
isLatecyMetricEnabled = fromMaybe False $ readMaybe =<< Conf.lookupEnvT "LATENCY_METRIC_ENABLED"

getCurrentDateInMillisIO :: IO Double
getCurrentDateInMillisIO = do
  t <- getPOSIXTime
  let (MkFixed i) = nominalDiffTimeToSeconds t
  pure $ fromInteger i * 1e-9

defaultLatencyMetric :: EEMT.LatencyInfo
defaultLatencyMetric = EEMT.LatencyInfo 0 0

getOptionLocalIO :: forall k v. (OptionEntity k v) => R.FlowRuntime -> k -> IO (Maybe v)
getOptionLocalIO R.FlowRuntime{..} k = do
    m <- readMVar _optionsLocal
    let valAny = Map.lookup (mkOptionKey @k @v k) m
    pure $ unsafeCoerce valAny

setOptionLocalIO :: forall k v. (OptionEntity k v) => R.FlowRuntime -> k -> v ->  IO ()
setOptionLocalIO R.FlowRuntime{..} k v = do
    m <- takeMVar _optionsLocal
    let newMap = Map.insert (mkOptionKey @k @v k) (unsafeCoerce @_ @Any v) m
    putMVar _optionsLocal newMap

incrementDBLatencyMetric :: R.FlowRuntime -> Double -> IO ()
incrementDBLatencyMetric flowRt latency = when isLatecyMetricEnabled $ do
    (EEMT.LatencyInfo oldLatency count) <- maybe defaultLatencyMetric (\(EEMT.DBMetricInfo x) -> x) <$> getOptionLocalIO flowRt EEMT.DBMetricInfoKey
    setOptionLocalIO flowRt EEMT.DBMetricInfoKey $ EEMT.DBMetricInfo (EEMT.LatencyInfo (oldLatency + latency) (count + 1))

incrementRedisLatencyMetric :: R.FlowRuntime -> Double -> IO ()
incrementRedisLatencyMetric flowRt latency = when isLatecyMetricEnabled $  do
    (EEMT.LatencyInfo oldLatency count) <- maybe defaultLatencyMetric (\(EEMT.RedisMetricInfo x) -> x) <$> getOptionLocalIO flowRt EEMT.RedisMetricInfoKey
    setOptionLocalIO flowRt EEMT.RedisMetricInfoKey $ EEMT.RedisMetricInfo (EEMT.LatencyInfo (oldLatency + latency) (count + 1))

incrementAPILatencyMetric :: R.FlowRuntime -> Double -> IO ()
incrementAPILatencyMetric flowRt latency = when isLatecyMetricEnabled $ do
    (EEMT.LatencyInfo oldLatency count) <- maybe defaultLatencyMetric (\(EEMT.APIMetricInfo x) -> x) <$> getOptionLocalIO flowRt EEMT.APIMetricInfoKey
    setOptionLocalIO flowRt EEMT.APIMetricInfoKey $ EEMT.APIMetricInfo (EEMT.LatencyInfo (oldLatency + latency) (count + 1))

logLatencyMetricLog :: (HasCallStack, L.MonadFlow m) => m ()
logLatencyMetricLog = do
    dbMetric    <- ((fromMaybe A.Null) . ((A.toJSON . (\(EEMT.DBMetricInfo x) -> x)) <$>)) <$> L.getOptionLocal EEMT.DBMetricInfoKey
    redisMetric <- ((fromMaybe A.Null) . ((A.toJSON . (\(EEMT.RedisMetricInfo x) -> x)) <$>)) <$> L.getOptionLocal EEMT.RedisMetricInfoKey
    apiMetric    <- ((fromMaybe A.Null) . ((A.toJSON . (\(EEMT.APIMetricInfo x) -> x)) <$>)) <$> L.getOptionLocal EEMT.APIMetricInfoKey
    L.logInfoV ("LATENCY_METRIC" :: Text) (A.object $ [("dbMetric",dbMetric),("redisMetric",redisMetric),("apiMetric",apiMetric)])