{-# LANGUAGE DeriveAnyClass #-}

module Euler.Product.Domain.Refund where


import           EulerHS.Prelude

import           Euler.Common.Types.Refund (RefundStatus (..))
import           Euler.Common.Types.Money

import           Data.Time


newtype RefundPId = RefundPId
  { refundPId :: Int
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

data Refund = Refund
  { id                  :: RefundPId
  , amount              :: Money
  , authorizationId     :: Maybe Text
  , dateCreated         :: LocalTime
  , epgTxnId            :: Maybe Text
  , gateway             :: Text
  , processed           :: Bool
  , rootReferenceNumber :: Maybe Text
  , txnDetailId         :: Maybe Int
  , referenceId         :: Maybe Text
  , status              :: RefundStatus
  , uniqueRequestId     :: Maybe Text
  , errorMessage        :: Maybe Text
  , sentToGateway       :: Maybe Bool
  , responseCode        :: Maybe Text
  , internalReferenceId :: Maybe Text
  , refundArn           :: Maybe Text
  , initiatedBy         :: Maybe Text
  , refundType          :: Maybe Text
  , refundSource        :: Maybe Text
  , lastModified        :: Maybe LocalTime
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)
