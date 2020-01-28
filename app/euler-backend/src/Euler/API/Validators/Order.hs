{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Euler.API.Validators.Order where

import EulerHS.Prelude
import EulerHS.Extra.Validation as V

import           Euler.Common.Types.Currency (Currency(..))
import           Euler.Common.Types.Order     (MandateFeature(..))

import qualified Euler.API.Order as APIO

import           Euler.Product.Domain.Money (mkMoney)

import qualified Data.Text as T


instance Transform APIO.OrderCreateRequest APIO.OrderCreateTemplate where
  transform sm = APIO.OrderCreateTemplate
    <$> withField @"order_id" sm textNotEmpty
    <*> (mkMoney    <$> withField @"amount"    sm amountValidators)
    <*> withField @"currency" sm (extractMaybeWithDefault "INR" >=> decode)
    <*> withField @"options_create_mandate" sm (extractMaybeWithDefault DISABLED)


notNegative :: Validator Int
notNegative = mkValidator "Should not be negative." (>= 0)

textNotEmpty :: Validator Text
textNotEmpty = mkValidator "can't be empty" (not . T.null)

amountValidators :: Validator Double
amountValidators =
  parValidate
    [ max2DecimalDigits
    , gteOne
    ]

--Will accept double values with upto two decimal places.
max2DecimalDigits :: Validator Double
max2DecimalDigits = mkValidator
  "Will accept double values with upto two decimal places."
  ((<=3) . length . dropWhile (/='.') . show)

gteOne :: Validator Double
gteOne = mkValidator
  "Should be greater than or equal 1"
  (>=1)
