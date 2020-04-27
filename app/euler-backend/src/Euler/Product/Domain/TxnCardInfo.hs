{-# LANGUAGE DeriveAnyClass #-}

module Euler.Product.Domain.TxnCardInfo where


import EulerHS.Prelude

import Euler.Common.Types.External.Mandate (PaymentMethodType)

import Data.Time


newtype TxnCardInfoPId = TxnCardInfoPId
  { txnCardInfoPId :: Int
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)


data TxnCardInfo = TxnCardInfo
  { id                    :: TxnCardInfoPId
  , txnId                 :: Text
  , cardIsin              :: Maybe Text
  , cardIssuerBankName    :: Maybe Text
  , cardExpYear           :: Maybe Text
  , cardExpMonth          :: Maybe Text
  , cardSwitchProvider    :: Maybe Text
  , cardType              :: Maybe Text
  , cardLastFourDigits    :: Maybe Text
  , nameOnCard            :: Maybe Text
  , cardFingerprint       :: Maybe Text
  , cardReferenceId       :: Maybe Text
  , txnDetailId           :: Maybe Int
  , dateCreated           :: Maybe LocalTime
  , paymentMethodType     :: Maybe PaymentMethodType
  , paymentMethod         :: Maybe Text
  , cardGlobalFingerprint :: Maybe Text
  , paymentSource         :: Maybe Text
  , authType              :: Maybe Text
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)
