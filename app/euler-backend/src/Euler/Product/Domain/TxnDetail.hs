{-# LANGUAGE DeriveAnyClass #-}

module Euler.Product.Domain.TxnDetail where


import           EulerHS.Prelude

import           Euler.Common.Types.Gateway
import           Euler.Common.Types.Money
import qualified Euler.Common.Types.TxnDetail as TDC

import           Data.Generics.Product.Fields
import           Data.Time


newtype TxnDetailId = TxnDetailId
  { txnDetail_id :: Int
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

data TxnDetail = TxnDetail
  { id                       :: TxnDetailId
  , version                  :: Int
  , errorMessage             :: Maybe Text
  , orderId                  :: Text
  , status                   :: TDC.TxnStatus
  , txnId                    :: Text
  , txdType                  :: Text
  , dateCreated              :: Maybe LocalTime
  , lastModified             :: Maybe LocalTime
  , successResponseId        :: Maybe Int
  , txnMode                  :: Maybe Text
  , addToLocker              :: Maybe Bool
  , merchantId               :: Maybe Text
  , bankErrorCode            :: Maybe Text
  , bankErrorMessage         :: Maybe Text
  , gateway                  :: Maybe Gateway
  , expressCheckout          :: Maybe Bool
  , redirect                 :: Maybe Bool
  , gatewayPayload           :: Maybe Text
  , isEmi                    :: Maybe Bool
  , emiBank                  :: Maybe Text
  , emiTenure                :: Maybe Int
  , username                 :: Maybe Text
  , txnUuid                  :: Maybe Text
  , merchantGatewayAccountId :: Maybe Int
  , txnAmount                :: Maybe Money
  , txnObjectType            :: Maybe Text
  , sourceObject             :: Maybe Text
  , sourceObjectId           :: Maybe Text
  , currency                 :: Maybe Text
  , netAmount                :: Maybe Money
  , surchargeAmount          :: Maybe Money
  , taxAmount                :: Maybe Money
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)
