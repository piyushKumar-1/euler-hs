{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE RecordWildCards #-}

module Euler.Product.Domain.Templates.Order where

import           EulerHS.Prelude

import           Euler.Common.Types.Currency (Currency)
import           Euler.Common.Types.Customer (CustomerId)
import           Euler.Common.Types.Order     (MandateFeature, OrderStatus (..))
import           Euler.Common.Types.Promotion
import           Euler.Common.Types.Money (Money)

import           Euler.Product.Domain.Templates.Address
import           Euler.Product.Domain.Templates.Customer


data OrderCreateTemplate = OrderCreateTemplate
  { orderId                        :: Text
  , currency                       :: Currency             -- Default: INR
  , amount                         :: Money
  -- , customer_id                       :: Maybe Text        -- EHS: fill billing_addr_customer_info instead
 -- , shipping_address_country_code_iso :: Text               -- Default: "IND"
  , optionsCreateMandate           :: MandateFeature       -- Default: DISABLED
  , billingAddrCustomerInfo        :: CustomerInfoTemplate
  , billingAddr                    :: AddressTemplate
  }
  deriving (Show, Eq, Ord, Generic, ToJSON)
