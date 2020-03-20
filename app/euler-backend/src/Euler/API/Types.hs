{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Euler.API.Types
  where

import EulerHS.Prelude
import Data.Data


data PaymentMethodType
  = WALLET  -- ^ Wallet
  | UPI     -- ^ UPI (UPI application)
  | NB      -- ^ Network Banking
  | CARD    -- ^ Credit / Debit Card
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Generic, ToJSON, FromJSON, Data, Typeable)

data TxnType
  = UPI_COLLECT
  | UPI_PAY
  | BHARAT_PAY
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Generic, ToJSON, FromJSON)



data ViesFlow
  = VIES_ENROLLMENT
  | VIES_REPEAT
  | INVALID
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Generic, ToJSON, FromJSON)

data ViesGatewayAuthReqParams = ViesGatewayAuthReqParams
  { viesReferenceId :: Maybe Text
  , viesInitError :: Maybe Text
  , errorCode :: Maybe Text
  , errorMessage :: Maybe Text
  , flowStatus :: Maybe Text
  , flow :: Maybe ViesFlow
  , crid :: Maybe Text
  , authFlow :: Maybe Text
  -- , errorDump :: Maybe Foreign
  }
  deriving (Show, Read, Eq, Generic, ToJSON, FromJSON)
