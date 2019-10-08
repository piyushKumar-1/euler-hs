{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Euler.Product.Domain.UPIPayment where

import EulerHS.Prelude
import Data.Data

import qualified Euler.Product.Domain.PaymentMethod.UPI as UPIPM 
 

data UPIPayment
  = UPICollect
  { payment_method :: UPIPM.UPIPaymentMethod
  , txn_type       :: UPITxnType
  , upi_vpa        :: Text
  }
  | UPIPay
  { payment_method :: UPIPM.UPIPaymentMethod
  , txn_type       :: UPITxnType
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

data UPITxnType
  = UPI_COLLECT
  | UPI_PAY
  | BHARAT_PAY
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Generic, ToJSON, FromJSON, Data,Typeable)