{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE DeriveGeneric #-}

module Euler.Storage.Types.TxnDetail
  ( TxnDetailT(..)
  , TxnDetail
  , TxnDetailId
  , txnDetailEMod
  , defaultTxnDetail
  ) where

import EulerHS.Prelude hiding (id)
import Data.Time
import qualified Database.Beam as B
import qualified Euler.Common.Types.TxnDetail as TDC

data TxnDetailT f = TxnDetail
  { id                       :: B.C f (Maybe Int) -- originaly String but Int in DB
  , version                  :: B.C f Int
  , errorMessage             :: B.C f (Maybe Text)
  , orderId                  :: B.C f Text
  , status                   :: B.C f TDC.TxnStatus
  , txnId                    :: B.C f Text
  , txdType                  :: B.C f Text  --- type wird is reserved
  , dateCreated              :: B.C f (Maybe LocalTime)
  , lastModified             :: B.C f (Maybe LocalTime)
  , successResponseId        :: B.C f (Maybe Int) -- originaly String but Int in DB
  , txnMode                  :: B.C f (Maybe Text)
  , addToLocker              :: B.C f (Maybe Bool)
  , merchantId               :: B.C f (Maybe Text)
  , bankErrorCode            :: B.C f (Maybe Text)
  , bankErrorMessage         :: B.C f (Maybe Text)
  , gateway                  :: B.C f (Maybe Text)
  , expressCheckout          :: B.C f (Maybe Bool)
  , redirect                 :: B.C f (Maybe Bool)
  , gatewayPayload           :: B.C f (Maybe Text)
  , isEmi                    :: B.C f (Maybe Bool)
  , emiBank                  :: B.C f (Maybe Text)
  , emiTenure                :: B.C f (Maybe Int)
  , username                 :: B.C f (Maybe Text)
  , txnUuid                  :: B.C f (Maybe Text)
  , merchantGatewayAccountId :: B.C f (Maybe Int)
  , txnAmount                :: B.C f (Maybe Double)
  , txnObjectType            :: B.C f (Maybe Text)
  , sourceObject             :: B.C f (Maybe Text)
  , sourceObjectId           :: B.C f (Maybe Text)
  , currency                 :: B.C f (Maybe Text)
  , netAmount                :: B.C f (Maybe Double)
  , surchargeAmount          :: B.C f (Maybe Double)
  , taxAmount                :: B.C f (Maybe Double)
  }
  deriving (Generic, B.Beamable)


instance B.Table TxnDetailT where
  data PrimaryKey TxnDetailT f =
    TxnDetailId (B.C f (Maybe Int)) deriving (Generic, B.Beamable)
  primaryKey = TxnDetailId . id

type TxnDetail = TxnDetailT Identity
type TxnDetailId = B.PrimaryKey TxnDetailT Identity

deriving instance Show TxnDetail
deriving instance Eq TxnDetail
deriving instance ToJSON TxnDetail
deriving instance FromJSON TxnDetail
deriving instance Read TxnDetail
deriving instance Ord TxnDetail

txnDetailEMod :: B.EntityModification
  (B.DatabaseEntity be db) be (B.TableEntity TxnDetailT)
txnDetailEMod = B.modifyTableFields
  B.tableModification
    { id = B.fieldNamed "id"
    , version = B.fieldNamed "version"
    , errorMessage = B.fieldNamed "error_message"
    , orderId = B.fieldNamed "order_id"
    , status = B.fieldNamed "status"
    , txnId = B.fieldNamed "txn_id"
    , txdType = B.fieldNamed "type"
    , dateCreated = B.fieldNamed "date_created"
    , lastModified = B.fieldNamed "last_modified"
    , successResponseId = B.fieldNamed "success_response_id"
    , txnMode = B.fieldNamed "txn_mode"
    , addToLocker = B.fieldNamed "add_to_locker"
    , merchantId = B.fieldNamed "merchant_id"
    , bankErrorCode = B.fieldNamed "bank_error_code"
    , bankErrorMessage = B.fieldNamed "bank_error_message"
    , gateway = B.fieldNamed "gateway"
    , expressCheckout = B.fieldNamed "express_checkout"
    , redirect = B.fieldNamed "redirect"
    , gatewayPayload = B.fieldNamed "gateway_payload"
    , isEmi = B.fieldNamed "is_emi"
    , emiBank = B.fieldNamed "emi_bank"
    , emiTenure = B.fieldNamed "emi_tenure"
    , username = B.fieldNamed "username"
    , txnUuid = B.fieldNamed "txn_uuid"
    , merchantGatewayAccountId = B.fieldNamed "merchant_gateway_account_id"
    , txnAmount = B.fieldNamed "tax_amount"
    , txnObjectType = B.fieldNamed "txn_object_type"
    , sourceObject = B.fieldNamed "source_object"
    , sourceObjectId = B.fieldNamed "source_object_id"
    , currency = B.fieldNamed "currency"
    , netAmount = B.fieldNamed "net_amount"
    , surchargeAmount = B.fieldNamed "surcharge_amount"
    , taxAmount = B.fieldNamed "txn_amount"
    }




defaultTxnDetail :: TxnDetail
defaultTxnDetail = TxnDetail
  { id                       = Nothing -- :: B.C f (Maybe Int) -- originaly String but Int in DB
  , version                  = 0 -- :: B.C f Int
  , errorMessage             = Nothing -- :: B.C f (Maybe Text)
  , orderId                  = "" -- :: B.C f Text
  , status                   = TDC.NOP --dunno why NOP -- :: B.C f TxnStatus
  , txnId                    = "" -- :: B.C f Text
  , txdType                  = "" -- :: B.C f Text
  , dateCreated              = Nothing -- :: B.C f (Maybe LocalTime)
  , lastModified             = Nothing -- :: B.C f (Maybe LocalTime)
  , successResponseId        = Nothing -- :: B.C f (Maybe Int) -- originaly String but Int in DB
  , txnMode                  = Nothing -- :: B.C f (Maybe Text)
  , addToLocker              = Nothing -- :: B.C f (Maybe Bool)
  , merchantId               = Nothing -- :: B.C f (Maybe Text)
  , bankErrorCode            = Nothing -- :: B.C f (Maybe Text)
  , bankErrorMessage         = Nothing -- :: B.C f (Maybe Text)
  , gateway                  = Nothing -- :: B.C f (Maybe Text)
  , expressCheckout          = Nothing -- :: B.C f (Maybe Bool)
  , redirect                 = Nothing -- :: B.C f (Maybe Bool)
  , gatewayPayload           = Nothing -- :: B.C f (Maybe Text)
  , isEmi                    = Nothing -- :: B.C f (Maybe Bool)
  , emiBank                  = Nothing -- :: B.C f (Maybe Text)
  , emiTenure                = Nothing -- :: B.C f (Maybe Int)
  , username                 = Nothing -- :: B.C f (Maybe Text)
  , txnUuid                  = Nothing -- :: B.C f (Maybe Text)
  , merchantGatewayAccountId = Nothing -- :: B.C f (Maybe Int)
  , txnAmount                = Nothing -- :: B.C f (Maybe Double)
  , txnObjectType            = Nothing -- :: B.C f (Maybe Text)
  , sourceObject             = Nothing -- :: B.C f (Maybe Text)
  , sourceObjectId           = Nothing -- :: B.C f (Maybe Text)
  , currency                 = Nothing -- :: B.C f (Maybe Text)
  , netAmount                = Nothing -- :: B.C f (Maybe Double)
  , surchargeAmount          = Nothing -- :: B.C f (Maybe Double)
  , taxAmount                = Nothing -- :: B.C f (Maybe Double)
  }