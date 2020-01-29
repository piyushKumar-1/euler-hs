{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE RecordWildCards #-}

module Euler.Product.Domain.Templates.Customer where

import           EulerHS.Prelude

import           Euler.Common.Types.Customer (CustomerId)

data CustomerInfoTemplate = CustomerInfoTemplate
  { customerId :: Maybe CustomerId
  , firstName  :: Maybe Text
  , lastName   :: Maybe Text
  }
  deriving (Show, Eq, Ord, Generic, ToJSON)
