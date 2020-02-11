{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Euler.API.Validators.Order where

import EulerHS.Prelude
import EulerHS.Extra.Validation as V
import qualified Data.Map  as Map
import qualified Data.Text as T

import           Euler.Common.Types.Currency (Currency(..))
import           Euler.Common.Types.Gateway (gatewayRMap)
import           Euler.Common.Types.Mandate     (MandateFeature(..))
import           Euler.Common.Types.Order     (UDF(..))
import           Euler.Common.Types.Money (mkMoney)

import qualified Euler.API.Order as API
import qualified Euler.Product.Domain.Templates.Address as Ts
import qualified Euler.Product.Domain.Templates.Order as Ts


apiOrderUpdToOrderUpdT :: API.OrderUpdateRequest -> V Ts.OrderUpdateTemplate
apiOrderUpdToOrderUpdT req = Ts.OrderUpdateTemplate
  <$> ( (fmap . fmap) mkMoney $ withField @"amount" req (insideJust amountValidators))
  <*> apiOrderUpdToUDF req
  <*> apiOrderUpdToBillingAddrHolderT req
  <*> apiOrderUpdToBillingAddrT req
  <*> apiOrderUpdToShippingAddrHolderT req
  <*> apiOrderUpdToShippingAddrT req

apiOrderUpdToBillingAddrHolderT :: API.OrderUpdateRequest -> V Ts.AddressHolderTemplate
apiOrderUpdToBillingAddrHolderT req = Ts.AddressHolderTemplate
  <$> withField @"billing_address_first_name" req pure
  <*> withField @"billing_address_last_name" req pure

apiOrderUpdToBillingAddrT :: API.OrderUpdateRequest -> V Ts.AddressTemplate
apiOrderUpdToBillingAddrT req = Ts.AddressTemplate
  <$> withField @"billing_address_line1" req pure
  <*> withField @"billing_address_line2" req pure
  <*> withField @"billing_address_line3" req pure
  <*> withField @"billing_address_city" req pure
  <*> withField @"billing_address_state" req pure
  <*> withField @"billing_address_country" req pure
  <*> withField @"billing_address_postal_code" req pure
  <*> withField @"billing_address_phone" req pure
  <*> withField @"billing_address_country_code_iso" req pure

apiOrderUpdToShippingAddrHolderT :: API.OrderUpdateRequest -> V Ts.AddressHolderTemplate
apiOrderUpdToShippingAddrHolderT req = Ts.AddressHolderTemplate
  <$> withField @"shipping_address_first_name" req pure
  <*> withField @"shipping_address_last_name" req pure

apiOrderUpdToShippingAddrT :: API.OrderUpdateRequest -> V Ts.AddressTemplate
apiOrderUpdToShippingAddrT req = Ts.AddressTemplate
  <$> withField @"shipping_address_line1" req pure
  <*> withField @"shipping_address_line2" req pure
  <*> withField @"shipping_address_line3" req pure
  <*> withField @"shipping_address_city" req pure
  <*> withField @"shipping_address_state" req pure
  <*> withField @"shipping_address_country" req pure
  <*> withField @"shipping_address_postal_code" req pure
  <*> withField @"shipping_address_phone" req pure
  <*> withField @"shipping_address_country_code_iso" req pure

apiOrderUpdToUDF :: API.OrderUpdateRequest -> V UDF
apiOrderUpdToUDF req = UDF
  <$> withField @"udf1" req pure
  <*> withField @"udf2" req pure
  <*> withField @"udf3" req pure
  <*> withField @"udf4" req pure
  <*> withField @"udf5" req pure
  <*> withField @"udf6" req pure
  <*> withField @"udf7" req pure
  <*> withField @"udf8" req pure
  <*> withField @"udf9" req pure
  <*> withField @"udf10" req pure


transApiOrdCreateToOrdCreateT :: API.OrderCreateRequest -> V Ts.OrderCreateTemplate
transApiOrdCreateToOrdCreateT sm = Ts.OrderCreateTemplate
    <$> withField @"order_id" sm textNotEmpty
    <*> withField @"currency" sm pure -- (extractMaybeWithDefault INR)
    <*> (mkMoney    <$> withField @"amount"    sm amountValidators)

    -- EHS: these types have changed. Rework validator
    <*> withField @"options_create_mandate" sm (extractMaybeWithDefault DISABLED)
    <*> (Ts.getOrderType <$> (withField @"options_create_mandate" sm (extractMaybeWithDefault DISABLED))) -- order_type

    <*> withField @"gateway_id" sm pure -- need validator?
    <*> withField @"customer_id" sm (insideJust >=> customerIdValidators)
    <*> withField @"customer_email" sm pure
    <*> withField @"customer_phone" sm pure
    <*> apiOrderCreateToBillingAddrHolderT sm -- billingAddrHolder
    <*> apiOrderCreateToBillingAddrT sm -- billingAddr
    <*> apiOrderCreateToShippingAddrHolderT sm -- shippingAddrHolder
    <*> apiOrderCreateToShippingAddrT sm -- shippingAddr
    <*> withField @"metaData" sm pure
    <*> withField @"description" sm pure -- description
    <*> withField @"product_id" sm pure -- productId

    -- EHS: add mandateMaxAmount validator.
    -- Error on invalid madate fields is in `invalidMandateFields`.

apiOrderCreateToBillingAddrHolderT :: API.OrderCreateRequest -> V Ts.AddressHolderTemplate
apiOrderCreateToBillingAddrHolderT req = Ts.AddressHolderTemplate
  <$> withField @"billing_address_first_name" req pure
  <*> withField @"billing_address_last_name" req pure

apiOrderCreateToBillingAddrT :: API.OrderCreateRequest -> V Ts.AddressTemplate
apiOrderCreateToBillingAddrT req = Ts.AddressTemplate
  <$> withField @"billing_address_line1" req pure
  <*> withField @"billing_address_line2" req pure
  <*> withField @"billing_address_line3" req pure
  <*> withField @"billing_address_city" req pure
  <*> withField @"billing_address_state" req pure
  <*> withField @"billing_address_country" req pure
  <*> withField @"billing_address_postal_code" req pure
  <*> withField @"billing_address_phone" req pure
  <*> withField @"billing_address_country_code_iso" req pure

apiOrderCreateToShippingAddrHolderT :: API.OrderCreateRequest -> V Ts.AddressHolderTemplate
apiOrderCreateToShippingAddrHolderT req = Ts.AddressHolderTemplate
  <$> withField @"shipping_address_first_name" req pure
  <*> withField @"shipping_address_last_name" req pure

apiOrderCreateToShippingAddrT :: API.OrderCreateRequest -> V Ts.AddressTemplate
apiOrderCreateToShippingAddrT req = Ts.AddressTemplate
  <$> withField @"shipping_address_line1" req pure
  <*> withField @"shipping_address_line2" req pure
  <*> withField @"shipping_address_line3" req pure
  <*> withField @"shipping_address_city" req pure
  <*> withField @"shipping_address_state" req pure
  <*> withField @"shipping_address_country" req pure
  <*> withField @"shipping_address_postal_code" req pure
  <*> withField @"shipping_address_phone" req pure
  <*> withField @"shipping_address_country_code_iso" req pure
-- EHS: add converter for desctiption:
-- when (isJust description) then use description
-- when (isNothing description) then (Just "").
-- (There was code `(description <|> Just "")` ).
-- EHS: doesn't seem valid. Why not just write null?

-- EHS: fill customerEmail & phone

-- EHS: DRY for validators. A lot of them is repeated many times.
gatewayIdValidator :: Validator Int
gatewayIdValidator = mkValidator
   ("Should be in " <> show gatewayRMap)
   (`Map.member` (gatewayRMap))

amountValidators :: Validator Double
amountValidators =
  parValidate
    [ max2DecimalDigits
    , gteOne
    ]

customerIdValidators :: Validator Text
customerIdValidators =
  parValidate
    [ notBlank
    ]

notBlank :: Validator Text
notBlank = mkValidator "Can't be blank" (not . T.null . T.strip)

notNegative :: Validator Int
notNegative = mkValidator "Should not be negative." (>= 0)

textNotEmpty :: Validator Text
textNotEmpty = mkValidator "Can't be empty" (not . T.null)

--Will accept double values with upto two decimal places.
max2DecimalDigits :: Validator Double
max2DecimalDigits = mkValidator
  "Will accept double values with upto two decimal places."
  ((<=3) . length . dropWhile (/='.') . show)

gteOne :: Validator Double
gteOne = mkValidator
  "Should be greater than or equal 1"
  (>=1)
