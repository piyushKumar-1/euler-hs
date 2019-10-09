{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Euler.API.Types where

import EulerHS.Prelude
import Data.Data


data PaymentMethodType
  = WALLET  -- ^ Wallet
  | UPI     -- ^ UPI (UPI application)
  | NB      -- ^ Network Banking
  | CARD    -- ^ Credit / Debit Card
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Generic, ToJSON, FromJSON, Data, Typeable)

data AuthType 
  = ATMPIN
  | THREE_DS
  | OTP
  | VISA_CHECKOUT
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Generic, ToJSON, FromJSON)

data TxnType
  = UPI_COLLECT
  | UPI_PAY
  | BHARAT_PAY
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Generic, ToJSON, FromJSON)