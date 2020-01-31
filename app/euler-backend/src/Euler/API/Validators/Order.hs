{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Euler.API.Validators.Order where

import EulerHS.Prelude
import EulerHS.Extra.Validation as V

import           Euler.Common.Types.Currency (Currency(..))
import           Euler.Common.Types.Order     (MandateFeature(..))
import           Euler.Common.Types.Money (mkMoney)

import qualified Euler.Product.Domain.Templates.Order as Ts


import qualified Data.Text as T

-- EHS: OrderCreateTemplate has changed, update the validator
instance Transform Ts.OrderCreateRequest Ts.OrderCreateTemplate where
  transform sm = Ts.OrderCreateTemplate
    <$> withField @"order_id" sm textNotEmpty
    <*> (mkMoney    <$> withField @"amount"    sm amountValidators)
    <*> withField @"currency" sm pure -- (extractMaybeWithDefault INR)
    <*> withField @"customer_id" sm (insideJust customerIdValidators)
    <*> withField @"options_create_mandate" sm (extractMaybeWithDefault DISABLED)
    -- <*> -- fill order_type with getOrderType & mandate feature
--    <*> withField @"billing_address_country_code_iso" sm (extractMaybeWithDefault "IND")
--    <*> withField @"shipping_address_country_code_iso" sm (extractMaybeWithDefault "IND")


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
