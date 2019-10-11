{-# LANGUAGE DeriveAnyClass #-}

module Euler.Common.RedisService where

import EulerHS.Prelude

-- from src/Product/OLTP/Services/RedisService.purs
data ResourceType = ResourceInt Int | ResourceStr Text
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)