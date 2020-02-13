{-# LANGUAGE AllowAmbiguousTypes #-}

module Euler.Storage.Validators.MerchantAccount where

import EulerHS.Prelude
import EulerHS.Extra.Validation
import qualified Data.Text as T

import qualified Euler.Storage.Types.MerchantAccount  as SM
import qualified Euler.Product.Domain.MerchantAccount as DM

transSMaccToDomMacc :: SM.MerchantAccount -> V DM.MerchantAccount
transSMaccToDomMacc sm = DM.MerchantAccount
    <$> withField @"id" sm (extractJust >=> notNegative)
    <*> withField @"merchantId" sm (extractJust >=> textNotEmpty)
    <*> withField @"resellerId" sm (insideJust textNotEmpty)


notNegative :: Validator Int
notNegative = mkValidator "Should not be negative." (>= 0)

textNotEmpty :: Validator Text
textNotEmpty = mkValidator "can't be empty" (not . T.null)
