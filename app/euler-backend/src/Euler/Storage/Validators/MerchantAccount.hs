{-# LANGUAGE AllowAmbiguousTypes #-}

module Euler.Storage.Validators.MerchantAccount where

import EulerHS.Prelude
import EulerHS.Extra.Validation

import           Euler.Common.Validators

import qualified Euler.Storage.Types.MerchantAccount  as SM
import qualified Euler.Product.Domain.MerchantAccount as DM

transSMaccToDomMacc :: SM.MerchantAccount -> V DM.MerchantAccount
transSMaccToDomMacc sm = DM.MerchantAccount
    <$> withField @"id" sm (extractJust >=> notNegative)
    <*> withField @"merchantId" sm (extractJust >=> textNotEmpty)
    <*> withField @"resellerId" sm (insideJust textNotEmpty)
