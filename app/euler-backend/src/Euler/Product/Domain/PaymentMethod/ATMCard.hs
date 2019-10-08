{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Euler.Product.Domain.PaymentMethod.ATMCard where

import EulerHS.Prelude
import Data.Data 
data ATMCardPaymentMethod
  = ATM_CARD_BOB
  | ATM_CARD_IOB
  | ATM_CARD_RSB
  | ATM_CARD_UBI
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Generic, ToJSON, FromJSON, Data,Typeable)
