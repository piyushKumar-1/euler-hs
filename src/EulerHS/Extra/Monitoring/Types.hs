{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DerivingStrategies  #-}

module EulerHS.Extra.Monitoring.Types where

import           EulerHS.Prelude
import           EulerHS.Types (OptionEntity)

data DBMetricInfo = DBMetricInfo {
    _latencyInfo :: LatencyInfo 
}
  deriving stock (Show, Generic)

data RedisMetricInfo = RedisMetricInfo {
    _latencyInfo :: LatencyInfo 
}
  deriving stock (Show, Generic)

data APIMetricInfo = APIMetricInfo {
    _latencyInfo :: LatencyInfo 
}
  deriving stock (Show, Generic)

data LatencyInfo = LatencyInfo {
    _latency :: Int
,   _count   :: Int
}
  deriving stock (Show, Generic)

data DBMetricInfoKey = DBMetricInfoKey
  deriving stock (Eq, Show, Generic, Ord)
  deriving anyclass (FromJSON, ToJSON)


data RedisMetricInfoKey = RedisMetricInfoKey
  deriving stock (Eq, Show, Generic, Ord)
  deriving anyclass (FromJSON, ToJSON)

data APIMetricInfoKey = APIMetricInfoKey
  deriving stock (Eq, Show, Generic, Ord)
  deriving anyclass (FromJSON, ToJSON)

instance OptionEntity DBMetricInfoKey DBMetricInfo

instance OptionEntity RedisMetricInfoKey RedisMetricInfo

instance OptionEntity APIMetricInfoKey APIMetricInfo
