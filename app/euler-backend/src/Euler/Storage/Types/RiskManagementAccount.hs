{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE DeriveGeneric #-}

module Euler.Storage.Types.RiskManagementAccount
  ( RiskManagementAccountT (..)
  , RiskManagementAccount
  , Id
  , riskManagementAccountEMod
  ) where

import EulerHS.Prelude hiding (id)
import Data.Time
import qualified Database.Beam as B


data RiskManagementAccountT f = RiskManagementAccount
  { id :: B.C f Int
  , version :: B.C f Int
  , accountDetailsJson :: B.C f Text
  , merchantAccountId :: B.C f Int
  , provider :: B.C f Text
  , dateCreated :: B.C f (Maybe LocalTime)
  , lastUpdated :: B.C f (Maybe LocalTime)
  , riskDsl :: B.C f (Maybe Text)
  }
  deriving (Generic, B.Beamable)

instance B.Table RiskManagementAccountT where
  data PrimaryKey RiskManagementAccountT f =
    Id (B.C f Int) deriving (Generic, B.Beamable)
  primaryKey = Id . id

type RiskManagementAccount = RiskManagementAccountT Identity
type Id = B.PrimaryKey RiskManagementAccountT Identity

deriving instance Show RiskManagementAccount
deriving instance Eq RiskManagementAccount
deriving instance ToJSON RiskManagementAccount
deriving instance FromJSON RiskManagementAccount
deriving instance Read RiskManagementAccount
deriving instance Ord RiskManagementAccount

riskManagementAccountEMod :: B.EntityModification
  (B.DatabaseEntity be db) be (B.TableEntity RiskManagementAccountT)
riskManagementAccountEMod = B.modifyTableFields
  B.tableModification
    { id = B.fieldNamed "id"
    , version = B.fieldNamed "version"
    , accountDetailsJson = B.fieldNamed "account_details_json"
    , merchantAccountId = B.fieldNamed "merchant_account_id"
    , provider = B.fieldNamed "provider"
    , dateCreated = B.fieldNamed "date_created"
    , lastUpdated = B.fieldNamed "last_updated"
    , riskDsl = B.fieldNamed "risk_dsl"
    }
