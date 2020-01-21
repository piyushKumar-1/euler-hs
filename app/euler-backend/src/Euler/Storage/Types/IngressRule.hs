{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE DeriveGeneric #-}

module Euler.Storage.Types.IngressRule
  ( IngressRuleT(..)
  , IngressRule
  , Id
  , ingressRuleEMod
  , defaultIngressRule
  ) where

import EulerHS.Prelude hiding (id)
import Data.Time
import Euler.Common.Types.DefaultDate
import qualified Database.Beam as B


data IngressRuleT f = IngressRule
  { id                :: B.C f (Maybe Int)
  , version           :: B.C f Int
  , dateCreated       :: B.C f LocalTime
  , lastUpdated       :: B.C f LocalTime
  , ipAddress         :: B.C f Text
  , merchantAccountId :: B.C f Int
  }
  deriving (Generic, B.Beamable)

instance B.Table IngressRuleT where
  data PrimaryKey IngressRuleT f =
    Id (B.C f (Maybe Int)) deriving (Generic, B.Beamable)
  primaryKey = Id . id

type IngressRule = IngressRuleT Identity
type Id = B.PrimaryKey IngressRuleT Identity

deriving instance Show IngressRule
deriving instance Eq IngressRule
deriving instance ToJSON IngressRule
deriving instance FromJSON IngressRule
deriving instance Read IngressRule
deriving instance Ord IngressRule

ingressRuleEMod :: B.EntityModification
  (B.DatabaseEntity be db) be (B.TableEntity IngressRuleT)
ingressRuleEMod = B.modifyTableFields
  B.tableModification
    { id = B.fieldNamed "id"
    , version = B.fieldNamed "version"
    , dateCreated = B.fieldNamed "date_created"
    , lastUpdated = B.fieldNamed "last_updated"
    , ipAddress = B.fieldNamed "ip_address"
    , merchantAccountId = B.fieldNamed "merchant_account_id"
    }

defaultIngressRule :: IngressRule
defaultIngressRule = IngressRule
  { id                = Just 1 -- :: Maybe Int
  , version           = 1 -- :: Int
  , dateCreated       = defaultDate -- :: LocalTime
  , lastUpdated       = defaultDate -- :: LocalTime
  , ipAddress         = "127.0.0.1" -- :: String
  , merchantAccountId = 1 -- :: Int
  }