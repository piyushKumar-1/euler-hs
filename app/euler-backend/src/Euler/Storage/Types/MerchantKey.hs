{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE DeriveGeneric #-}

module Euler.Storage.Types.MerchantKey
  ( MerchantKeyT(..)
  , MerchantKey
  -- , Id
  , merchantKeyEMod
  , defaultMerchantKey
  ) where

import EulerHS.Prelude hiding (id)
import Euler.Common.Types.DefaultDate
import Data.Time
import qualified Database.Beam as B



-- newtype MerchantKey = MerchantKey MKey


data MerchantKeyT f = MerchantKey
  { id                :: B.C f (Maybe Int)
  , version           :: B.C f Int
  , apiKey            :: B.C f (Maybe Text)
  , status            :: B.C f (Maybe Text)
  , dateCreated       :: B.C f LocalTime
  , lastUpdated       :: B.C f LocalTime
  , merchantAccountId :: B.C f (Maybe Int)
  , scope             :: B.C f (Maybe Text)
  }
  deriving (Generic, B.Beamable)

instance B.Table MerchantKeyT where
  data PrimaryKey MerchantKeyT f =
    Id (B.C f (Maybe Int)) deriving (Generic, B.Beamable)
  primaryKey = Id . id

type MerchantKey = MerchantKeyT Identity
-- type Id = B.PrimaryKey MerchantKeyT Identity

deriving instance Show MerchantKey
deriving instance Eq MerchantKey
deriving instance ToJSON MerchantKey
deriving instance FromJSON MerchantKey
deriving instance Read MerchantKey
deriving instance Ord MerchantKey

merchantKeyEMod :: B.EntityModification
  (B.DatabaseEntity be db) be (B.TableEntity MerchantKeyT)
merchantKeyEMod = B.modifyTableFields
  B.tableModification
    { id = B.fieldNamed "id"
    , version = B.fieldNamed "version"
    , apiKey = B.fieldNamed "api_key"
    , status = B.fieldNamed "status"
    , dateCreated = B.fieldNamed "date_created"
    , lastUpdated = B.fieldNamed "last_updated"
    , merchantAccountId = B.fieldNamed "merchant_account_id"
    , scope = B.fieldNamed "scope"
    }

defaultMerchantKey :: MerchantKey
defaultMerchantKey = MerchantKey
  { id                = Just  1 -- :: Maybe Int
  , version           =  0 -- :: Int
  , apiKey            = Just "apiKey" -- :: Maybe Text
  , status            = Just "ACTIVE" -- :: Maybe Text
  , dateCreated       = defaultDate  -- :: LocalTime
  , lastUpdated       = defaultDate  -- :: LocalTime
  , merchantAccountId = Just 1 -- :: Maybe Int
  , scope             = Just "MERCHANT" -- :: Maybe Text
  }
