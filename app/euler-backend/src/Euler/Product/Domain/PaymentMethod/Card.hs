{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Euler.Product.Domain.PaymentMethod.Card where

import EulerHS.Prelude
import Data.Data

data CardPaymentMethod
  = VISA
  | MASTERCARD
  | MAESTRO
  | AMEX
  | RUPAY
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Generic, ToJSON, FromJSON, Data,Typeable)
