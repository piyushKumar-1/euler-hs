{-# LANGUAGE DeriveAnyClass #-}

module Euler.Product.Domain.Refund where


import           EulerHS.Prelude

import           Euler.Common.Types.Refund (RefundStatus (..))
import qualified Euler.Common.Types.Refund as RC
import           Euler.Product.Domain.Money

import           Data.Generics.Product.Fields
import           Data.Time


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


getStatus :: Refund -> Bool
getStatus refund = status == SUCCESS || processed || sentToGateway
  where
    status = getField @"status" refund
    processed = getField @"processed" refund
    sentToGateway = fromMaybe False (getField @"sentToGateway" refund)

getRefId :: Refund -> Text
getRefId refund
    | gateway == "HDFC" && sentToGateway = internalReferenceId
    | otherwise = mempty
  where
    gateway = getField @"gateway" refund
    sentToGateway = fromMaybe False (getField @"sentToGateway" refund)
    internalReferenceId = fromMaybe mempty $ getField @"internalReferenceId" refund
