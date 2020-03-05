{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE DeriveGeneric #-}

module Euler.Storage.Types.Feature
  ( FeatureT(..)
  , Feature
  -- , Id
  , featureEMod
  , defaultFeature
  ) where

import EulerHS.Prelude hiding (id)
import Data.Time
import qualified Database.Beam as B


data FeatureT f = Feature
  { id :: B.C f (Maybe Int)
  , version :: B.C f Int
  , enabled :: B.C f Bool
  , name :: B.C f Text
  , merchantId :: B.C f (Maybe Text)
  , disabledUntil :: B.C f (Maybe LocalTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table FeatureT where
  data PrimaryKey FeatureT f =
    Id (B.C f (Maybe Int)) deriving (Generic, B.Beamable)
  primaryKey = Id . id

type Feature = FeatureT Identity
-- type Id = B.PrimaryKey FeatureT Identity

deriving instance Show Feature
deriving instance Eq Feature
deriving instance ToJSON Feature
deriving instance FromJSON Feature
deriving instance Read Feature
deriving instance Ord Feature

featureEMod :: B.EntityModification
  (B.DatabaseEntity be db) be (B.TableEntity FeatureT)
featureEMod = B.modifyTableFields
  B.tableModification
    { id = B.fieldNamed "id"
    , version = B.fieldNamed "version"
    , enabled = B.fieldNamed "enabled"
    , name = B.fieldNamed "name"
    , merchantId = B.fieldNamed "merchant_id"
    , disabledUntil = B.fieldNamed "disabled_until"
    }

defaultFeature :: Feature
defaultFeature = Feature
  { id = Nothing -- :: Maybe Int
  , version = 1 -- :: Int
  , enabled = False -- :: Bool
  , name = "featureName" -- "eulerOrderStatusCachingKey" :: Text
  , merchantId = Nothing -- :: Maybe Text
  , disabledUntil = Nothing -- :: Maybe LocalTime
  }
