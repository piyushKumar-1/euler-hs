{-# LANGUAGE DeriveAnyClass #-}

module Euler.Product.Domain.Operational.Order where

import           Data.Time
import           Euler.Common.Types.Currency        (Currency)
import           Euler.Common.Types.DefaultDate
import           Euler.Common.Types.GatewayMetadata (GatewayMetadata)
import           Euler.Common.Types.Order           (MandateFeature,
                                                     OrderStatus (NEW),
                                                     OrderType, UDF)
import           EulerHS.Prelude


data OrderInfo = OrderInfo
  { order_id                          :: Text
  , amount                            :: Double
  , currency                          :: Maybe Text -- EUR, USD, GBP,  Default value: INR
  , customer_id                       :: Maybe Text
  , customer_email                    :: Maybe Text
  , customer_phone                    :: Maybe Text
  , description                       :: Maybe Text
  , return_url                        :: Maybe Text
  , product_id                        :: Maybe Text
  , billing_address_first_name        :: Maybe Text
  , billing_address_last_name         :: Maybe Text
  , billing_address_line1             :: Maybe Text
  , billing_address_line2             :: Maybe Text
  , billing_address_line3             :: Maybe Text
  , billing_address_city              :: Maybe Text
  , billing_address_state             :: Maybe Text
  , billing_address_country           :: Maybe Text
  , billing_address_postal_code       :: Maybe Text
  , billing_address_phone             :: Maybe Text
  , billing_address_country_code_iso  :: Maybe Text -- Default value: IND
  , shipping_address_first_name       :: Maybe Text
  , shipping_address_last_name        :: Maybe Text
  , shipping_address_line1            :: Maybe Text
  , shipping_address_line2            :: Maybe Text
  , shipping_address_line3            :: Maybe Text
  , shipping_address_city             :: Maybe Text
  , shipping_address_state            :: Maybe Text
  , shipping_address_country          :: Maybe Text
  , shipping_address_postal_code      :: Maybe Text
  , shipping_address_phone            :: Maybe Text
  , shipping_address_country_code_iso :: Maybe Text -- Default value: IND
  , udf1                              :: Maybe Text
  , udf2                              :: Maybe Text
  , udf3                              :: Maybe Text
  , udf4                              :: Maybe Text
  , udf5                              :: Maybe Text
  , udf6                              :: Maybe Text
  , udf7                              :: Maybe Text
  , udf8                              :: Maybe Text
  , udf9                              :: Maybe Text
  , udf10                             :: Maybe Text
  , metaData                          :: Maybe Text
  , gateway_id                        :: Maybe Text -- converted to Int, why Text?
  , options_create_mandate            :: Maybe MandateFeature
  , mandate_max_amount                :: Maybe Text
  , auto_refund                       :: Maybe Bool
  , options_get_client_auth_token     :: Maybe Bool
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, ToForm)
