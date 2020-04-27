{-# LANGUAGE DeriveAnyClass #-}

module Euler.Product.Domain.Feature where


import           EulerHS.Prelude

import           Data.Time


newtype FeaturePId = FeaturePId
  { featurePId :: Int
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

data Feature = Feature
  { id :: FeaturePId
  , version :: Int
  , enabled :: Bool
  , name :: Text
  , merchantId :: Maybe Text
  , disabledUntil :: Maybe LocalTime
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

