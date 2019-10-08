{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Euler.Product.Domain.PaymentMethod.UPI where

import EulerHS.Prelude
import Data.Data

data UPIPaymentMethod = UPI
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Generic, ToJSON, FromJSON, Data,Typeable)