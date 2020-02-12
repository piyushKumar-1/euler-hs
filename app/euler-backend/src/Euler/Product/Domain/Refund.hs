{-# LANGUAGE DeriveAnyClass #-}

module Euler.Product.Domain.Refund where


import           EulerHS.Prelude
import           Data.Time
import qualified Euler.Common.Types.Refund as RC
import           Euler.Product.Domain.Money

newtype RefundId = RefundId
  { refundId :: Text
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

data Refund = Refund
  { id                  :: RefundId
  , amount              :: Money
  , authorizationId     :: Maybe Text
  , dateCreated         :: LocalTime
  , epgTxnId            :: Maybe Text
  , gateway             :: Text
  , processed           :: Bool
  , rootReferenceNumber :: Maybe Text
  , txnDetailId         :: Maybe Text
  , referenceId         :: Maybe Text
  , status              :: RC.RefundStatus
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
