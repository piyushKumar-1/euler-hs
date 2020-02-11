{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}

module Euler.Storage.Types.Refund
  ( RefundT(..)
  , Refund
  -- , Id
  , refundEMod
  ) where

import           EulerHS.Prelude hiding (id)

import           EulerHS.Extra.Validation
import           EulerHS.Language
import           EulerHS.Types

import           Data.Time
import qualified Database.Beam as B

import qualified Euler.Common.Types.Refund as C

import           Euler.Product.Domain.Money
import qualified Euler.Product.Domain.Refund as D

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
  , status              :: B.C f C.RefundStatus
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

type RefundId = Text

loadRefund :: RefundId -> Flow (Maybe D.Refund)
loadRefund = undefined

type TxnDetailId = Text

findRefunds :: TxnDetailId -> Flow [D.Refund]
findRefunds txnId = do
  rs <- withDB eulerDB $ do
  let predicate Refund {txnDetailId} = txnDetailId ==. B.just_ (B.val_ txnId)
  findRows
    $ B.select
    $ B.filter_ predicate
    -- TODO: $ B.orderBy_ (B.asc_ . ???dateCreated???)
    $ B.all_ (EDB.refund eulerDBSchema)
  pure $ transform <$> rs

textNotEmpty :: Validator Text
textNotEmpty = mkValidator "Can't be empty." (not . T.null)

transform :: Refund -> V D.Refund
transform r = D.Refund
  <$> withField @"id" r (extractJust >=> textNotEmpty)
  <*> (mkMoney <$> withField @"amount" r (extractJust >=> amountValidators))
  <*> withField @"authorizationId" r pure
  <*> withField @"dateCreated" r (extractJust >=> pure)
  <*> withField @"epgTxnId" r pure
  <*> withField @"gateway" r textNotEmpty
  <*> withField @"processed" r pure
  <*> withField @"rootReferenceNumber" r pure  
  <*> withField @"txnDetailId" r pure
  <*> withField @"referenceId" r pure
  <*> withField @"status" r pure
  <*> withField @"uniqueRequestId" r pure
  <*> withField @"errorMessage" r pure
  <*> withField @"sentToGateway" r pure
  <*> withField @"responseCode" r pure
  <*> withField @"internalReferenceId" r pure
  <*> withField @"refundArn" r pure
  <*> withField @"initiatedBy" r pure
  <*> withField @"refundType" r pure
  <*> withField @"refundSource" r pure
  <*> withField @"lastModified" r pure


-- EHS: move validators to separate module
notNegative :: Validator Int
notNegative = mkValidator "Should not be negative." (>= 0)

amountValidators :: Validator Double
amountValidators =
  parValidate
    [ max2DecimalDigits
    , gteOne
    ]

--Will accept double values with upto two decimal places.
max2DecimalDigits :: Validator Double
max2DecimalDigits = mkValidator
  "Will accept double values with upto two decimal places."
  ((<=3) . length . dropWhile (/='.') . show)

gteOne :: Validator Double
gteOne = mkValidator
  "Should be greater than or equal 1"
  (>=1)
