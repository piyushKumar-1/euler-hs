{-# LANGUAGE DeriveAnyClass  #-}

module Euler.Product.Domain.Templates.Customer where

import           EulerHS.Prelude

import           Euler.Common.Types.Customer (CustomerId)

data CustomerTemplate = CustomerTemplate
  { customerId        :: CustomerId
  , firstName         :: Maybe Text
  , lastName          :: Maybe Text

  , email             :: Maybe Text
  , mobileNumber      :: Text
  }
  deriving (Show, Eq, Ord, Generic, ToJSON)
