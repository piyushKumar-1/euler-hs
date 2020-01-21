{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE DeriveGeneric #-}

module Euler.Storage.Types.Mandate
  ( MandateT(..)
  , Mandate
  , Id
  , mandateEMod
  , defaultMandate
  ) where

import EulerHS.Prelude hiding (id)
import Data.Time
import Euler.Common.Types.DefaultDate
import Euler.Common.Types.Gateway (Gateway)
import qualified Database.Beam as B
import qualified Euler.Common.Types.Mandate as M (PaymentMethodType, MandateStatus(CREATED))

-- from src/Types/Storage/EC/Mandate/Types.purs
data MandateT f = Mandate
  {  id                       :: B.C f (Maybe Int)
  ,  merchantId               :: B.C f Text
  ,  endDate                  :: B.C f (Maybe LocalTime)
  ,  startDate                :: B.C f (Maybe LocalTime)
  ,  maxAmount                :: B.C f (Maybe Double)
  ,  merchantCustomerId       :: B.C f (Maybe Text)
  ,  paymentMethod            :: B.C f (Maybe Text)
  ,  paymentMethodType        :: B.C f (Maybe M.PaymentMethodType)
  ,  status                   :: B.C f M.MandateStatus
  ,  token                    :: B.C f Text
  ,  mandateId                :: B.C f Text
  ,  paymentMethodId          :: B.C f (Maybe Text)
  ,  gateway                  :: B.C f (Maybe Gateway)
  ,  gatewayParams            :: B.C f (Maybe Text)
  ,  authOrderId              :: B.C f (Maybe Int)
  ,  activatedAt              :: B.C f (Maybe LocalTime)
  ,  dateCreated              :: B.C f LocalTime
  ,  lastModified             :: B.C f LocalTime
  ,  authTxnCardInfo          :: B.C f (Maybe Text)
  ,  currency                 :: B.C f (Maybe Text)
  ,  merchantGatewayAccountId :: B.C f (Maybe Int)
  ,  metadata                 :: B.C f (Maybe Text)
  }
  deriving (Generic, B.Beamable)

instance B.Table MandateT where
  data PrimaryKey MandateT f =
    Id (B.C f (Maybe Int)) deriving (Generic, B.Beamable)
  primaryKey = Id . id

type Mandate = MandateT Identity
type Id = B.PrimaryKey MandateT Identity

deriving instance Show Mandate
deriving instance Eq Mandate
deriving instance ToJSON Mandate
deriving instance FromJSON Mandate
deriving instance Read Mandate
deriving instance Ord Mandate

mandateEMod :: B.EntityModification
  (B.DatabaseEntity be db) be (B.TableEntity MandateT)
mandateEMod = B.modifyTableFields
  B.tableModification
    { id = B.fieldNamed "id"
    , merchantId = B.fieldNamed "merchant_id"
    , endDate = B.fieldNamed "end_date"
    , startDate = B.fieldNamed "start_date"
    , maxAmount = B.fieldNamed "max_amount"
    , merchantCustomerId = B.fieldNamed "merchant_customer_id"
    , paymentMethod = B.fieldNamed "payment_method"
    , paymentMethodType = B.fieldNamed "payment_method_type"
    , status = B.fieldNamed "status"
    , token = B.fieldNamed "token"
    , mandateId = B.fieldNamed "mandate_id"
    , paymentMethodId = B.fieldNamed "payment_method_id"
    , gateway = B.fieldNamed "gateway"
    , gatewayParams = B.fieldNamed "gateway_params"
    , authOrderId = B.fieldNamed "auth_order_id"
    , activatedAt = B.fieldNamed "activated_at"
    , dateCreated = B.fieldNamed "date_created"
    , lastModified = B.fieldNamed "last_modified"
    , authTxnCardInfo = B.fieldNamed "auth_txn_card_info"
    , currency = B.fieldNamed "currency"
    , merchantGatewayAccountId = B.fieldNamed "merchant_gateway_account_id"
    , metadata = B.fieldNamed "metadata"
    }

defaultMandate :: Mandate
defaultMandate = Mandate
  {  id                        = Nothing -- :: Maybe Int
  ,  merchantId                = "" -- :: Text
  ,  endDate                   = Nothing -- :: Maybe LocalTime
  ,  startDate                 = Nothing -- :: Maybe LocalTime
  ,  maxAmount                 = Nothing -- :: Maybe Double
  ,  merchantCustomerId        = Nothing -- :: Maybe Text
  ,  paymentMethod             = Nothing -- :: Maybe Text
  ,  paymentMethodType         = Nothing -- :: Maybe PaymentMethodType
  ,  status                    = M.CREATED -- :: MandateStatus
  ,  token                     = "" -- :: Text
  ,  mandateId                 = "" -- :: Text
  ,  paymentMethodId           = Nothing -- :: Maybe Text
  ,  gateway                   = Nothing -- :: Maybe Gateway
  ,  gatewayParams             = Nothing -- :: Maybe Text
  ,  authOrderId               = Nothing -- :: Maybe Int
  ,  activatedAt               = Nothing -- :: Maybe LocalTime
  ,  dateCreated               = defaultDate -- :: LocalTime
  ,  lastModified              = defaultDate -- :: LocalTime
  ,  authTxnCardInfo           = Nothing -- :: Maybe Text
  ,  currency                  = Nothing -- :: Maybe Text
  ,  merchantGatewayAccountId  = Nothing -- :: Maybe Int
  ,  metadata                  = Nothing -- :: Maybe Text
  }