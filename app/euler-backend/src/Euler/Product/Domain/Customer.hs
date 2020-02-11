{-# LANGUAGE DeriveAnyClass #-}

module Euler.Product.Domain.Customer
  ( Customer(..)
  , CreateCustomer(..)
  ) where

import EulerHS.Prelude

import Data.Time



data Customer = Customer
  { customerId                :: Text
  , customerVersion           :: Int
  , customerDateCreated       :: LocalTime
  , customerEmailAddress      :: Text
  , customerFirstName         :: Text
  , customerLastName          :: Text
  , customerLastUpdated       :: LocalTime
  , customerMerchantAccountId :: Int
  , customerMobileCountryCode :: Text
  , customerMobileNumber      :: Text
  , customerObjectReferenceId :: Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data CreateCustomer = CreateCustomer
  { crtCstObject_reference_id :: Text
  , crtCstMobile_number       :: Text
  -- email_address required in https://www.juspay.in/docs/api/ec/?shell#create-customer
  -- email_address not required in https://developer.juspay.in/reference#customer
  , crtCstEmail_address       :: Maybe Text
  , crtCstFirst_name          :: Maybe Text
  , crtCstLast_name           :: Maybe Text
  -- No need to prefix “+”.
  , crtCstMobile_country_code :: Text
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)
