{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module EulerHS.KVConnector.Metrics where

import           EulerHS.Prelude
import qualified EulerHS.Language as L
import           EulerHS.Options  (OptionEntity)
import           Euler.Events.MetricApi.MetricApi
import qualified Juspay.Extra.Config as Conf

incrementKVMetric :: L.MonadFlow m => KVMetricHandler -> KVMetric -> Text -> Text -> Source -> m ()
incrementKVMetric handle metric tag action source = do
  L.runIO $ ((kvCounter handle) (metric, tag, action, source))

data KVMetricHandler = KVMetricHandler
  { kvCounter :: (KVMetric, Text, Text, Source) -> IO ()
  }

data KVMetric = KVAction

data Source = KV | SQL | KV_AND_SQL
    deriving Show

mkKVMetricHandler :: IO KVMetricHandler
mkKVMetricHandler = do
  metrics <- register collectionLock
  pure $ KVMetricHandler $ \case
    (KVAction, tag, action, source)   ->
      inc (metrics </> #kv_action) tag action source

kv_action = counter #kv_action
      .& lbl @"tag" @Text
      .& lbl @"action" @Text
      .& lbl @"source" @Source
      .& build

collectionLock =
     kv_action
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

incrementMetric :: (HasCallStack, L.MonadFlow m) => KVMetric -> Text -> Text -> Source -> m ()
incrementMetric metric tag action source = do
  env <- L.getOption KVMetricCfg
  case env of
    Just val -> incrementKVMetric val metric tag action source
    Nothing -> pure ()