{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module EulerHS.KVConnector.Metrics where

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
  L.runIO $ ((kvCounter handle) (metric, tag, action, source,model,mid))

data KVMetricHandler = KVMetricHandler
  { kvCounter :: (KVMetric, Text, Text, Text,Text,Text) -> IO ()
  }

data KVMetric = KVAction


-- data DBLogEntry a = DBLogEntry
--   { _log_type     :: Text
--   , _action       :: Text
--   , _data         :: a
--   , _latency      :: Int
--   , _model        :: Text
--   , _cpuLatency   :: Integer
--   , _source       :: Text
--   , _apiTag       :: Maybe Text
--   , _merchant_id  :: Maybe Text
--   }
data Source = KV | SQL | KV_AND_SQL
    deriving Show

mkKVMetricHandler :: IO KVMetricHandler
mkKVMetricHandler = do
  metrics <- register collectionLock
  pure $ KVMetricHandler $ \case
    (KVAction, tag, action, source, model , mid)   ->
      inc (metrics </> #kv_action) tag action source model mid

kv_action = counter #kv_action
      .& lbl @"tag" @Text
      .& lbl @"action" @Text
      .& lbl @"source" @Text
      .& lbl @"model" @Text
      .& lbl @"mid" @Text
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

incrementMetric :: (HasCallStack, L.MonadFlow m) => KVMetric -> DBLogEntry a ->  m ()
incrementMetric metric dblog = do
  env <- L.getOption KVMetricCfg
  case env of
    Just val -> incrementKVMetric val metric dblog
    Nothing -> pure ()