{-# LANGUAGE DeriveAnyClass #-}

module Euler.Product.Domain.Order where

import EulerHS.Prelude
import Data.Time
import Euler.Common.Types.Merchant(MerchantId)
import Euler.Common.Types.Money
import Euler.Common.Types.Order (OrderStatus(OrderStatusNew), OrderMandate, UDF, OrderType, OrderId, OrderPId)
import Euler.Common.Types.Currency (Currency)
import Euler.Common.Types.DefaultDate
import Euler.Common.Types.GatewayMetadata (GatewayMetadata)
--import Euler.Common.Types.Promotion
--import Euler.Common.Types.Address


-- Previously: OrderReference
data Order = Order
  { id                :: OrderPId
  , version           :: Int
  , amount            :: Money
  , currency          :: Currency
  , merchantId        :: MerchantId
  , orderId           :: OrderId
  , orderUuid         :: Text
  , orderType         :: OrderType
  , orderStatus       :: OrderStatus
  , customerId        :: Maybe Text
  , customerEmail     :: Maybe Text
  , customerPhone     :: Maybe Text

  , billingAddressId  :: Maybe Int
  , shippingAddressId :: Maybe Int

  , udf               :: UDF
  -- -- , browser           :: Maybe Text       -- EHS: Not a fields for order, should not be here
  -- -- , browserVersion    :: Maybe Text       -- EHS: Not a fields for order, should not be here
  -- -- , popupLoaded       :: Maybe Bool       -- EHS: Not a fields for order, should not be here
  -- -- , popupLoadedTime   :: Maybe LocalTime  -- EHS: Not a fields for order, should not be here
  , description       :: Maybe Text
  , returnUrl         :: Maybe Text

  , amountRefunded    :: Maybe Double
  , refundedEntirely  :: Maybe Bool
  , autoRefund        :: Bool

  , productId         :: Maybe Text


  , mandate            :: OrderMandate         -- EHS: not a domain field        -- ^ Default: MandateDisabled
  , acquireOrderToken  :: Bool                 -- EHS: not a domain field                    -- ^ Depends on version and passed param.

  , lastSynced        :: LocalTime          -- EHS: Not a domain fields
  , dateCreated       :: LocalTime          -- EHS: Not a domain fields
  , lastModified      :: LocalTime          -- EHS: Not a domain fields
  --
  -- , preferredGateway  :: Maybe Text         -- EHS: Not a field for order, should not be here.
  -- , gatewayMetadata   :: GatewayMetadata    -- EHS: Not a field for order, should not be here.

  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

-- defaultOrder = Order
--   { id                = Just 1  -- :: Int
--   , version           = 0  -- :: Int
--   , amount            = Just 55  -- :: Double
--   , currency          = Nothing  -- :: Maybe Text
--   , dateCreated       = defaultDate  -- :: LocalTime
--   , lastModified      = defaultDate  -- :: LocalTime
--   , merchantId        = Nothing  -- :: Maybe Text
--   , orderId           = Nothing  -- :: Maybe Text
--   , status            = NEW  -- :: Maybe OrderStatus
--   , customerEmail     = Nothing  -- :: Maybe Text
--   , customerId        = Nothing  -- :: Maybe Text
--   , browser           = Nothing  -- :: Maybe Text
--   , browserVersion    = Nothing  -- :: Maybe Text
--   , popupLoaded       = Nothing  -- :: Maybe Bool
--   , popupLoadedTime   = Nothing  -- :: Maybe LocalTime
--   , description       = Nothing  -- :: Maybe Text
--   , udf1              = Nothing  -- :: Maybe Text
--   , udf10             = Nothing  -- :: Maybe Text
--   , udf2              = Nothing  -- :: Maybe Text
--   , udf3              = Nothing  -- :: Maybe Text
--   , udf4              = Nothing  -- :: Maybe Text
--   , udf5              = Nothing  -- :: Maybe Text
--   , udf6              = Nothing  -- :: Maybe Text
--   , udf7              = Nothing  -- :: Maybe Text
--   , udf8              = Nothing  -- :: Maybe Text
--   , udf9              = Nothing  -- :: Maybe Text
--   , returnUrl         = Nothing  -- :: Maybe Text
--   , amountRefunded    = Nothing  -- :: Maybe Double
--   , refundedEntirely  = Nothing  -- :: Maybe Bool
--   , preferredGateway  = Nothing  -- :: Maybe Text
--   , customerPhone     = Nothing  -- :: Maybe Text
--   , productId         = Nothing  -- :: Maybe Text
--   , billingAddressId  = Nothing  -- :: Maybe Int
--   , shippingAddressId = Nothing  -- :: Maybe Int
--   , orderUuid         = Just "orderUuid" -- Nothing  -- :: Maybe Text
--   , lastSynced        = Nothing  -- :: Maybe LocalTime
--   , orderType         = Nothing  -- :: Maybe OrderType
--   , mandateFeature    = Nothing  -- :: Maybe MandateFeature
--   , autoRefund        = Nothing  -- :: Maybe Bool
--   }







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
