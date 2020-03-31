{-# LANGUAGE DeriveAnyClass #-}

module Euler.Product.Domain.TxnDetail where


import           EulerHS.Prelude

import qualified Euler.Common.Types as C
import qualified Euler.Common.Types.TxnDetail as TDC

import           Data.Time


newtype TxnDetailPId = TxnDetailPId
  { txnDetailPId :: Int
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

data TxnDetail = TxnDetail
  { id                       :: TxnDetailPId
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
  , gateway                  :: Maybe C.Gateway
  , expressCheckout          :: Maybe Bool
  , redirect                 :: Maybe Bool
  , gatewayPayload           :: Maybe Text
  , isEmi                    :: Maybe Bool
  , emiBank                  :: Maybe Text
  , emiTenure                :: Maybe Int
  , username                 :: Maybe Text
  , txnUuid                  :: Maybe Text
  , merchantGatewayAccountId :: Maybe Int
  , txnAmount                :: Maybe C.Money
  , txnObjectType            :: Maybe Text
  , sourceObject             :: Maybe Text
  , sourceObjectId           :: Maybe Text
  , currency                 :: Maybe Text
  , netAmount                :: Maybe C.Money
  , surchargeAmount          :: Maybe C.Money
  , taxAmount                :: Maybe C.Money
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)
