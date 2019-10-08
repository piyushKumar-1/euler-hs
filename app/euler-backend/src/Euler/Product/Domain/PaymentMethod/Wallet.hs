{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Euler.Product.Domain.PaymentMethod.Wallet where

import EulerHS.Prelude
import Data.Data

data WalletPaymentMethod
  = MOBIKWIK
  | PAYTM
  | FREECHARGE
  | OLAMONEY
  | PAYUMONEY
  | AIRTEL_MONEY
  | OXIGEN
  | PAYZAPP
  | JANACASH
  | JIOMONEY
  | PHONEPE
  | AMAZONPAY
  | PAYPAL
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Generic, ToJSON, FromJSON, Data,Typeable)
