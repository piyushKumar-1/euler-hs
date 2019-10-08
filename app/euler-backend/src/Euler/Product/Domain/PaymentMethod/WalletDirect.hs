{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Euler.Product.Domain.PaymentMethod.WalletDirect where

import EulerHS.Prelude
import Data.Data

data WalletDirectPaymentMethod
  = MOBIKWIK
  | PAYTM
  | FREECHARGE
  | OLAMONEY
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Generic, ToJSON, FromJSON, Data,Typeable)
