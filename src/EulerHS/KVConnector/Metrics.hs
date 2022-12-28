{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module EulerHS.KVConnector.Metrics where

import           GHC.Float (int2Double)
import           EulerHS.Prelude
import qualified EulerHS.Language as L
import           EulerHS.Options  (OptionEntity)
import           Euler.Events.MetricApi.MetricApi
import qualified Juspay.Extra.Config as Conf
import           EulerHS.KVConnector.Types  (DBLogEntry(..))

incrementKVMetric :: L.MonadFlow m => KVMetricHandler -> KVMetric -> DBLogEntry a -> m ()
incrementKVMetric handle metric dblog = do
  let mid = fromMaybe "" $ _merchant_id dblog
  let tag = fromMaybe "" $ _apiTag dblog
  let source = _source dblog
  let model = _model dblog
  let action = _action dblog
      latency = _latency dblog
      cpuLatency = _cpuLatency dblog
  L.runIO $ ((kvCounter handle) (metric, tag, action, source, model, mid, latency, cpuLatency))

data KVMetricHandler = KVMetricHandler
  { kvCounter :: (KVMetric, Text, Text, Text, Text, Text, Int, Integer) -> IO ()
  }

data KVMetric = KVAction

data Source = KV | SQL | KV_AND_SQL
    deriving Show

mkKVMetricHandler :: IO KVMetricHandler
mkKVMetricHandler = do
  metrics <- register collectionLock
  pure $ KVMetricHandler $ \case
    (KVAction, tag, action, source, model , mid, latency, cpuLatency) -> do
      inc (metrics </> #kv_action_counter) tag action source model  mid
      observe (metrics </> #kv_latency_observe) (int2Double latency) tag action source model
      observe (metrics </> #kv_cpu_latency_observe) (fromInteger cpuLatency) tag action source model

kv_action_counter = counter #kv_action_counter
      .& lbl @"tag" @Text
      .& lbl @"action" @Text
      .& lbl @"source" @Text
      .& lbl @"model" @Text
      .& lbl @"mid" @Text
      .& build

kv_latency_observe = histogram #kv_latency_observe
      .& lbl @"tag" @Text
      .& lbl @"action" @Text
      .& lbl @"source" @Text
      .& lbl @"model" @Text
      .& build

kv_cpu_latency_observe = histogram #kv_cpu_latency_observe
      .& lbl @"tag" @Text
      .& lbl @"action" @Text
      .& lbl @"source" @Text
      .& lbl @"model" @Text
      .& build

collectionLock =
     kv_action_counter
  .> kv_latency_observe
  .> kv_cpu_latency_observe
  .> MNil


---------------------------------------------------------

data KVMetricCfg = KVMetricCfg
  deriving stock (Generic, Typeable, Show, Eq)
  deriving anyclass (ToJSON, FromJSON)

instance OptionEntity KVMetricCfg KVMetricHandler

---------------------------------------------------------

isKVMetricEnabled :: Bool
isKVMetricEnabled = fromMaybe False $ readMaybe =<< Conf.lookupEnvT "KV_METRIC_ENABLED"

---------------------------------------------------------

incrementMetric :: (HasCallStack, L.MonadFlow m) => KVMetric -> DBLogEntry a ->  m ()
incrementMetric metric dblog = do
  env <- L.getOption KVMetricCfg
  case env of
    Just val -> incrementKVMetric val metric dblog
    Nothing -> pure ()