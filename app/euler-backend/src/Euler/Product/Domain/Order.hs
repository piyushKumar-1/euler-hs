{-# LANGUAGE DeriveAnyClass #-}

module Euler.Product.Domain.Order where

import EulerHS.Prelude
import Data.Time
import Euler.Common.Types.Order (OrderStatus(NEW), MandateFeature)
import Euler.Common.Types.DefaultDate

-- Previously: OrderReference
-- should fields be "Maybe" ?
data Order = Order
  { id                :: Maybe Int
  , version           :: Int
  , amount            :: Maybe Double
  , currency          :: Maybe Text
  , dateCreated       :: LocalTime
  , lastModified      :: LocalTime
  , merchantId        :: Maybe Text
  , orderId           :: Maybe Text
  , status            :: OrderStatus
  , customerEmail     :: Maybe Text
  , customerId        :: Maybe Text
  , browser           :: Maybe Text
  , browserVersion    :: Maybe Text
  , popupLoaded       :: Maybe Bool
  , popupLoadedTime   :: Maybe LocalTime
  , description       :: Maybe Text
  , udf1              :: Maybe Text
  , udf10             :: Maybe Text
  , udf2              :: Maybe Text
  , udf3              :: Maybe Text
  , udf4              :: Maybe Text
  , udf5              :: Maybe Text
  , udf6              :: Maybe Text
  , udf7              :: Maybe Text
  , udf8              :: Maybe Text
  , udf9              :: Maybe Text
  , returnUrl         :: Maybe Text
  , amountRefunded    :: Maybe Double
  , refundedEntirely  :: Maybe Bool
  , preferredGateway  :: Maybe Text
  , customerPhone     :: Maybe Text
  , productId         :: Maybe Text
  , billingAddressId  :: Maybe Int
  , shippingAddressId :: Maybe Int
  , orderUuid         :: Maybe Text
  , lastSynced        :: Maybe LocalTime
  , orderType         :: Maybe OrderType
  , mandateFeature    :: Maybe MandateFeature
  , autoRefund        :: Maybe Bool
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

defaultOrder :: Order
defaultOrder = Order
  { id                = Just 1  -- :: Int
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



data OrderType
  = MANDATE_REGISTER
  | MANDATE_PAYMENT
  | ORDER_PAYMENT
  deriving (Show, Read, Eq, Ord, Generic, ToJSON, FromJSON)



-- data OrderAddress = OrderAddress
--   { id :: Maybe Int
--   , version :: Int
--   , firstName :: Maybe Text
--   , lastName :: Maybe Text
--   , line1 :: Maybe Text
--   , line2 :: Maybe Text
--   , line3 :: Maybe Text
--   , city :: Maybe Text
--   , state :: Maybe Text
--   , country :: Maybe Text
--   , countryCodeIso :: Maybe Text
--   , postalCode :: Maybe Text
--   , phone :: Maybe Text
--   }
--   deriving (Show, Read, Eq, Ord, Generic, ToJSON, FromJSON)
--
-- defaultOrderAddress = OrderAddress
--   { id             = Just 1 -- :: Maybe Int
--   , version        = 1 -- :: Int
--   , firstName      = Nothing -- :: Maybe Text
--   , lastName       = Nothing -- :: Maybe Text
--   , line1          = Nothing -- :: Maybe Text
--   , line2          = Nothing -- :: Maybe Text
--   , line3          = Nothing -- :: Maybe Text
--   , city           = Nothing -- :: Maybe Text
--   , state          = Nothing -- :: Maybe Text
--   , country        = Nothing -- :: Maybe Text
--   , countryCodeIso = Nothing -- :: Maybe Text
--   , postalCode     = Nothing -- :: Maybe Text
--   , phone          = Nothing -- :: Maybe Text
--   }

-- duplicated field with different case types camel/snake
-- data OrderStatusRequest = OrderStatusRequest
--   { txn_uuid :: Maybe Text
--   , merchant_id :: Maybe Text
--   , order_id :: Maybe Text
--   , txnUuid :: Maybe Text
--   , merchantId :: Maybe Text
--   , orderId :: Maybe Text
--  -- , "options.add_full_gateway_response" :: Maybe Text
--   -- why not options_add_full_gateway_response
--   }

-- from src/Types/Communication/OLTP/OrderStatus.purs
-- data Promotion' = Promotion'
--   { id              :: Maybe Text
--   , order_id        :: Maybe Text
--   , rules           :: Maybe [Rules]
--   , created         :: Maybe Text
--   , discount_amount :: Maybe Double
--   , status          :: Maybe Text
--   }
--
-- defaultPromotion' = Promotion'
--   { id              = Nothing -- :: Maybe Text
--   , order_id        = Nothing -- :: Maybe Text
--   , rules           = Nothing -- :: Maybe [Rules]
--   , created         = Nothing -- :: Maybe Text
--   , discount_amount = Nothing -- :: Maybe Double
--   , status          = Nothing -- :: Maybe Text
--   }

-- from src/Types/Storage/EC/Promotions.purs
data Promotions = Promotions
  { id               :: Int
  , dateCreated      :: LocalTime
  , discountAmount   :: Double
  , lastModified     :: LocalTime
  , orderId          :: Maybe Int
  , rules            :: Text
  , status           :: Text
  , orderReferenceId :: Maybe Int
  }
  deriving (Show, Read, Eq, Ord, Generic, ToJSON, FromJSON)
-- from src/Types/Storage/EC/Promotions.purs
--data Rules = Rules
--  { dimension :: Text
--  , value :: Text
--  }
