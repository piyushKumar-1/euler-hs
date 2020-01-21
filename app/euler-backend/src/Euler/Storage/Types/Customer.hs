{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE DeriveGeneric #-}

module Euler.Storage.Types.Customer
  ( CustomerT(..)
  , Customer
  , Id
  , customerEMod
  , defaultCustomer
  ) where

import EulerHS.Prelude hiding (id)
import Data.Time
import Euler.Common.Types.DefaultDate
import qualified Database.Beam as B

data CustomerT f = Customer
  { id                :: B.C f (Maybe Text)
  , version           :: B.C f Int
  , dateCreated       :: B.C f LocalTime
  , emailAddress      :: B.C f (Maybe Text)
  , firstName         :: B.C f (Maybe Text)
  , lastName          :: B.C f (Maybe Text)
  , lastUpdated       :: B.C f LocalTime
  , merchantAccountId :: B.C f Int
  , mobileCountryCode :: B.C f Text
  , mobileNumber      :: B.C f Text
  , objectReferenceId :: B.C f (Maybe Text)
  }
  deriving (Generic, B.Beamable)

instance B.Table CustomerT where
  data PrimaryKey CustomerT f =
    Id (B.C f (Maybe Text)) deriving (Generic, B.Beamable)
  primaryKey = Id . id

type Customer = CustomerT Identity
type Id = B.PrimaryKey CustomerT Identity

deriving instance Show Customer
deriving instance Eq Customer
deriving instance ToJSON Customer
deriving instance FromJSON Customer
deriving instance Read Customer
deriving instance Ord Customer

customerEMod :: B.EntityModification
  (B.DatabaseEntity be db) be (B.TableEntity CustomerT)
customerEMod = B.modifyTableFields
  B.tableModification
    { id = B.fieldNamed "id"
    , version = B.fieldNamed "version"
    , dateCreated = B.fieldNamed "date_created"
    , emailAddress = B.fieldNamed "email_address"
    , firstName = B.fieldNamed "first_name"
    , lastName = B.fieldNamed "last_name"
    , lastUpdated = B.fieldNamed "last_updated"
    , merchantAccountId = B.fieldNamed "merchant_account_id"
    , mobileCountryCode = B.fieldNamed "mobile_country_code"
    , mobileNumber = B.fieldNamed "mobile_number"
    , objectReferenceId = B.fieldNamed "object_reference_id"
    }

defaultCustomer :: Customer
defaultCustomer = Customer
  { id                 = Nothing -- :: Maybe Text
  , version            = 1 -- :: Int
  , dateCreated        = defaultDate -- :: LocalTime
  , emailAddress       = Nothing -- :: Maybe Text
  , firstName          = Nothing -- :: Maybe Text
  , lastName           = Nothing -- :: Maybe Text
  , lastUpdated        = defaultDate -- :: LocalTime
  , merchantAccountId  = 1 -- :: Int
  , mobileCountryCode  = "+91" -- :: Text
  , mobileNumber       = "9000000000" -- :: Text
  , objectReferenceId  = Nothing -- :: Maybe Text
  }