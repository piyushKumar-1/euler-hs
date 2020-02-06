{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE DeriveGeneric #-}

module Euler.Storage.Types.OrderReference
  ( OrderReferenceT(..)
  , OrderReference
  , Id
  , orderReferenceEMod
  , defaultOrderReference
  ) where

import EulerHS.Prelude hiding (id)
import Data.Time
import Euler.Common.Types.Currency
import Euler.Common.Types.DefaultDate (defaultDate)
import Euler.Common.Types.Order (OrderStatus(NEW), MandateFeature, OrderType)
import qualified Database.Beam as B


data OrderReferenceT f = OrderReference
  { id                :: B.C f (Maybe Int)
  , version           :: B.C f Int
  , amount            :: B.C f (Maybe Double)
  , currency          :: B.C f (Maybe Currency)
  , dateCreated       :: B.C f LocalTime
  , lastModified      :: B.C f LocalTime
  , merchantId        :: B.C f (Maybe Text)
  , orderId           :: B.C f (Maybe Text)
  , status            :: B.C f OrderStatus
  , customerEmail     :: B.C f (Maybe Text)
  , customerId        :: B.C f (Maybe Text)
  , browser           :: B.C f (Maybe Text)
  , browserVersion    :: B.C f (Maybe Text)
  , popupLoaded       :: B.C f (Maybe Bool)
  , popupLoadedTime   :: B.C f (Maybe LocalTime)
  , description       :: B.C f (Maybe Text)
  , udf1              :: B.C f (Maybe Text)
  , udf2              :: B.C f (Maybe Text)
  , udf3              :: B.C f (Maybe Text)
  , udf4              :: B.C f (Maybe Text)
  , udf5              :: B.C f (Maybe Text)
  , udf6              :: B.C f (Maybe Text)
  , udf7              :: B.C f (Maybe Text)
  , udf8              :: B.C f (Maybe Text)
  , udf9              :: B.C f (Maybe Text)
  , udf10             :: B.C f (Maybe Text)
  , returnUrl         :: B.C f (Maybe Text)
  , amountRefunded    :: B.C f (Maybe Double)
  , refundedEntirely  :: B.C f (Maybe Bool)
  , preferredGateway  :: B.C f (Maybe Text)
  , customerPhone     :: B.C f (Maybe Text)
  , productId         :: B.C f (Maybe Text)
  , billingAddressId  :: B.C f (Maybe Int)
  , shippingAddressId :: B.C f (Maybe Int)
  , orderUuid         :: B.C f (Maybe Text)
  , lastSynced        :: B.C f (Maybe LocalTime)
  , orderType         :: B.C f (Maybe OrderType)
  , mandateFeature    :: B.C f (Maybe MandateFeature)
  , autoRefund        :: B.C f (Maybe Bool)
  }
  deriving (Generic, B.Beamable)

instance B.Table OrderReferenceT where
  data PrimaryKey OrderReferenceT f =
    Id (B.C f (Maybe Int)) deriving (Generic, B.Beamable)
  primaryKey = Id . id

type OrderReference = OrderReferenceT Identity
type Id = B.PrimaryKey OrderReferenceT Identity

deriving instance Show OrderReference
deriving instance Eq OrderReference
deriving instance ToJSON OrderReference
deriving instance FromJSON OrderReference
deriving instance Read OrderReference
deriving instance Ord OrderReference

orderReferenceEMod :: B.EntityModification
  (B.DatabaseEntity be db) be (B.TableEntity OrderReferenceT)
orderReferenceEMod = B.modifyTableFields
  B.tableModification
    { id = B.fieldNamed "id"
    , version = B.fieldNamed "version"
    , amount = B.fieldNamed "amount"
    , currency = B.fieldNamed "currency"
    , dateCreated = B.fieldNamed "date_created"
    , lastModified = B.fieldNamed "last_modified"
    , merchantId = B.fieldNamed "merchant_id"
    , orderId = B.fieldNamed "order_id"
    , status = B.fieldNamed "status"
    , customerEmail = B.fieldNamed "customer_email"
    , customerId = B.fieldNamed "customer_id"
    , browser = B.fieldNamed "browser"
    , browserVersion = B.fieldNamed "browser_version"
    , popupLoaded = B.fieldNamed "popup_loaded"
    , popupLoadedTime = B.fieldNamed "popup_loaded_time"
    , description = B.fieldNamed "description"
    , udf1 = B.fieldNamed "udf1"
    , udf10 = B.fieldNamed "udf10"
    , udf2 = B.fieldNamed "udf2"
    , udf3 = B.fieldNamed "udf3"
    , udf4 = B.fieldNamed "udf4"
    , udf5 = B.fieldNamed "udf5"
    , udf6 = B.fieldNamed "udf6"
    , udf7 = B.fieldNamed "udf7"
    , udf8 = B.fieldNamed "udf8"
    , udf9 = B.fieldNamed "udf9"
    , returnUrl = B.fieldNamed "return_url"
    , amountRefunded = B.fieldNamed "amount_refunded"
    , refundedEntirely = B.fieldNamed "refunded_entirely"
    , preferredGateway = B.fieldNamed "preferred_gateway"
    , customerPhone = B.fieldNamed "customer_phone"
    , productId = B.fieldNamed "product_id"
    , billingAddressId = B.fieldNamed "billing_address_id"
    , shippingAddressId = B.fieldNamed "shipping_address_id"
    , orderUuid = B.fieldNamed "order_uuid"
    , lastSynced = B.fieldNamed "last_synced"
    , orderType = B.fieldNamed "order_type"
    , mandateFeature = B.fieldNamed "mandate_feature"
    , autoRefund = B.fieldNamed "auto_refund"
    }

defaultOrderReference :: OrderReference
defaultOrderReference = OrderReference
  { id                = Nothing  -- :: Int
  , version           = 0  -- :: Int
  , amount            = Just 55  -- :: Double
  , currency          = Nothing  -- :: Maybe Text
  , dateCreated       = defaultDate  -- :: LocalTime
  , lastModified      = defaultDate  -- :: LocalTime
  , merchantId        = Nothing  -- :: Maybe Text
  , orderId           = Nothing  -- :: Maybe Text
  , status            = NEW  -- :: Maybe OrderStatus
  , customerEmail     = Nothing  -- :: Maybe Text
  , customerId        = Nothing  -- :: Maybe Text
  , browser           = Nothing  -- :: Maybe Text
  , browserVersion    = Nothing  -- :: Maybe Text
  , popupLoaded       = Nothing  -- :: Maybe Bool
  , popupLoadedTime   = Nothing  -- :: Maybe LocalTime
  , description       = Nothing  -- :: Maybe Text
  , udf1              = Nothing  -- :: Maybe Text
  , udf10             = Nothing  -- :: Maybe Text
  , udf2              = Nothing  -- :: Maybe Text
  , udf3              = Nothing  -- :: Maybe Text
  , udf4              = Nothing  -- :: Maybe Text
  , udf5              = Nothing  -- :: Maybe Text
  , udf6              = Nothing  -- :: Maybe Text
  , udf7              = Nothing  -- :: Maybe Text
  , udf8              = Nothing  -- :: Maybe Text
  , udf9              = Nothing  -- :: Maybe Text
  , returnUrl         = Nothing  -- :: Maybe Text
  , amountRefunded    = Nothing  -- :: Maybe Double
  , refundedEntirely  = Nothing  -- :: Maybe Bool
  , preferredGateway  = Nothing  -- :: Maybe Text
  , customerPhone     = Nothing  -- :: Maybe Text
  , productId         = Nothing  -- :: Maybe Text
  , billingAddressId  = Nothing  -- :: Maybe Int
  , shippingAddressId = Nothing  -- :: Maybe Int
  , orderUuid         = Just "orderUuid" -- Nothing  -- :: Maybe Text
  , lastSynced        = Nothing  -- :: Maybe LocalTime
  , orderType         = Nothing  -- :: Maybe OrderType
  , mandateFeature    = Nothing  -- :: Maybe MandateFeature
  , autoRefund        = Nothing  -- :: Maybe Bool
  }