{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}

module Euler.Storage.Types.Refund
  ( RefundT(..)
  , Refund
  -- , Id
  , refundEMod
  ) where

import           EulerHS.Prelude hiding (id)

import           Data.Time
import qualified Database.Beam as B
import qualified Euler.Common.Types.Refund as RC


data RefundT f = Refund
  { id                  :: B.C f (Maybe Text)
  , amount              :: B.C f Double
  , authorizationId     :: B.C f (Maybe Text)
  , dateCreated         :: B.C f LocalTime
  , epgTxnId            :: B.C f (Maybe Text)
  , gateway             :: B.C f Text
  , processed           :: B.C f Bool
  , rootReferenceNumber :: B.C f (Maybe Text)
  , txnDetailId         :: B.C f (Maybe Text)
  , referenceId         :: B.C f (Maybe Text)
  , status              :: B.C f RC.RefundStatus
  , uniqueRequestId     :: B.C f (Maybe Text)
  , errorMessage        :: B.C f (Maybe Text)
  , sentToGateway       :: B.C f (Maybe Bool)
  , responseCode        :: B.C f (Maybe Text)
  , internalReferenceId :: B.C f (Maybe Text)
  , refundArn           :: B.C f (Maybe Text)
  , initiatedBy         :: B.C f (Maybe Text)
  , refundType          :: B.C f (Maybe Text)
  , refundSource        :: B.C f (Maybe Text)
  , lastModified        :: B.C f (Maybe LocalTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table RefundT where
  data PrimaryKey RefundT f =
    Id (B.C f (Maybe Text)) deriving (Generic, B.Beamable)
  primaryKey = Id . id

type Refund = RefundT Identity
-- type Id = B.PrimaryKey RefundT Identity

deriving instance Show Refund
deriving instance Eq Refund
deriving instance ToJSON Refund
deriving instance FromJSON Refund
deriving instance Read Refund
deriving instance Ord Refund

refundEMod :: B.EntityModification (B.DatabaseEntity be db) be (B.TableEntity RefundT)
refundEMod = B.modifyTableFields
  B.tableModification
    { id = B.fieldNamed "id"
    , amount = B.fieldNamed "amount"
    , authorizationId = B.fieldNamed "authorization_id"
    , dateCreated = B.fieldNamed "date_created"
    , epgTxnId = B.fieldNamed "epg_txn_id"
    , gateway = B.fieldNamed "gateway"
    , processed = B.fieldNamed "processed"
    , rootReferenceNumber = B.fieldNamed "root_reference_number"
    , txnDetailId = B.fieldNamed "txn_detail_id"
    , referenceId = B.fieldNamed "reference_id"
    , status = B.fieldNamed "status"
    , uniqueRequestId = B.fieldNamed "unique_request_id"
    , errorMessage = B.fieldNamed "error_message"
    , sentToGateway = B.fieldNamed "sent_to_gateway"
    , responseCode = B.fieldNamed "response_code"
    , internalReferenceId = B.fieldNamed "internal_reference_id"
    , refundArn = B.fieldNamed "refund_arn"
    , initiatedBy = B.fieldNamed "initiated_by"
    , refundType = B.fieldNamed "refund_type"
    , refundSource = B.fieldNamed "refund_source"
    , lastModified = B.fieldNamed "last_modified"
    }
