{-# LANGUAGE DeriveAnyClass #-}

module Euler.Product.Domain.TxnRiskCheck where


import           EulerHS.Prelude

import           Data.Time


newtype TxnRiskCheckPId = TxnRiskCheckPId
  { txnRiskCheckPId :: Int
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)


data TxnRiskCheck = TxnRiskCheck
  { id                              :: TxnRiskCheckPId
  , completeResponse                :: Text
  , dateCreated                     :: LocalTime
  , flagged                         :: Maybe Bool
  , lastUpdated                     :: LocalTime
  , recommendedAction               :: Maybe Text
  , resultJson                      :: Maybe Text
  , riskManagementAccountId         :: Int
  , txnDetailId                     :: Int
  , message                         :: Maybe Text
  , status                          :: Maybe Text
  , riskStatus                      :: Maybe Text
  , domestic                        :: Maybe Bool
  , invocationMode                  :: Maybe Text
  , paymentStatusUpdateResponseCode :: Maybe Text
  , paymentStatusUpdated            :: Maybe Bool
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)


data Risk = Risk
  { provider          :: Maybe Text
  , status            :: Maybe Text
  , message           :: Maybe Text
  , flagged           :: Maybe Bool
  , recommendedAction :: Maybe Text
  , ebsRiskLevel      :: Text
  , ebsPaymentStatus  :: Maybe Text
  , ebsBinCountry     :: Text
  , ebsRiskPercentage :: Text
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)
