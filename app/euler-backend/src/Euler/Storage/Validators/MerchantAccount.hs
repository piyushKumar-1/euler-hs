{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Euler.Storage.Validators.MerchantAccount where

import EulerHS.Prelude
import EulerHS.Extra.Validation as V

import qualified Euler.Storage.Types.MerchantAccount  as SM
import qualified Euler.Product.Domain.MerchantAccount as DM

instance Transform SM.MerchantAccount DM.MerchantAccount where
  transform sm = DM.MerchantAccount
    <$> withField @"id" sm (fromJust >=> notNegative)
    <*> withField @"merchantId" sm (fromJust >=> textNotEmpty)


notNegative :: Validator Int
notNegative = mkValidator "Should not be negative." (>= 0)

textNotEmpty :: Validator Text
textNotEmpty = mkValidator "can't be empty" (not . T.null)