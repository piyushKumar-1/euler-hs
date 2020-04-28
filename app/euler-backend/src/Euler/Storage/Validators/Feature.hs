{-# LANGUAGE AllowAmbiguousTypes #-}

module Euler.Storage.Validators.Feature where

import EulerHS.Prelude
import EulerHS.Extra.Validation

import           Euler.Common.Validators

import qualified Euler.Product.Domain.Feature as D
import qualified Euler.Storage.Types.Feature as S


transformFeature :: S.Feature -> V D.Feature
transformFeature r = D.Feature
  <$> (D.FeaturePId <$> withField @"id" r (extractJust >=> notNegative))
  <*> withField @"version" r pure
  <*> withField @"enabled" r pure
  <*> withField @"name" r pure
  <*> withField @"merchantId" r pure
  <*> withField @"disabledUntil" r pure