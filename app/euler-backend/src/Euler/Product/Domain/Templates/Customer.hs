{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE RecordWildCards #-}

module Euler.Product.Domain.Templates.Customer where

import           EulerHS.Prelude

import           Euler.Common.Types.Customer (CustomerId)

data CustomerTemplate = CustomerTemplate
  { customerId   :: CustomerId
  , firstName    :: Maybe Text
  , lastName     :: Maybe Text
  , emailAddress :: Maybe Text
  , mobileNumber :: Text
  }
  deriving (Show, Eq, Ord, Generic, ToJSON)

data CustomerContacts = CustomerContacts
  { customerEmailAddress :: Maybe Text
  , customerMobileNumber :: Maybe Text
  }
  deriving (Show, Eq, Ord, Generic, ToJSON)
