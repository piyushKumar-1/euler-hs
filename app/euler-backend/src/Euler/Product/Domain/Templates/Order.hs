{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE RecordWildCards #-}

module Euler.Product.Domain.Templates.Order where

import           EulerHS.Prelude

import           Euler.Common.Types.Currency (Currency)
import           Euler.Common.Types.Customer (CustomerId)
import           Euler.Common.Types.Order     (OrderMandate (..), OrderStatus (..), OrderType (..), UDF)
import           Euler.Common.Types.Promotion
import           Euler.Common.Types.Gateway (GatewayId)
import           Euler.Common.Types.Money (Money)

import           Euler.Product.Domain.Templates.Address
import           Euler.Product.Domain.Templates.Customer


data OrderUpdateTemplate = OrderUpdateTemplate
  { amount             :: Maybe Money
  , udf                :: UDF
  , billingAddrHolder  :: AddressHolderTemplate   -- EHS: previously CustomerInfo
  , billingAddr        :: AddressTemplate
  , shippingAddrHolder :: AddressHolderTemplate
  , shippingAddr       :: AddressTemplate
  }
  deriving (Show, Eq, Ord, Generic, ToJSON)

data OrderCreateTemplate = OrderCreateTemplate
  { orderId                        :: Text
  , currency                       :: Maybe Currency          -- Default value: MerchantIframePreferences defaultCurrency or INR
  , amount                         :: Money
  , mandate                        :: OrderMandate            -- Default: MandateDisabled

  , orderType                      :: OrderType               -- depends on mandate feature
  , gatewayId                      :: Maybe GatewayId

  , customerId                     :: Maybe Text              -- EHS: cases with discrepancy are not handled.
                                                              -- What if customerId specified, but first/last name
                                                              -- is different than in the DB? What data has a higher priority?
                                                              -- Should we update it in the DB?
                                                              -- EHS: what is relation between customer info & address holder name?

  , customerEmail                  :: Maybe Text
  , customerPhone                  :: Maybe Text

  , billingAddrHolder              :: AddressHolderTemplate   -- EHS: previously CustomerInfo
  , billingAddr                    :: AddressTemplate
  , shippingAddrHolder             :: AddressHolderTemplate
  , shippingAddr                   :: AddressTemplate
  -- EHS: shipping address country_code_iso Default: "IND"
  -- EHS: seems we always write the same first & last names for billing & shipping addresses.
  , metaData                       :: Maybe Text
  , description                    :: Maybe Text
  , productId                      :: Maybe Text

  , returnUrl                      :: Maybe Text
  }
  deriving (Show, Eq, Ord, Generic, ToJSON)
