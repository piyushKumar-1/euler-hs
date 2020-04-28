{-# LANGUAGE AllowAmbiguousTypes #-}

module Euler.Storage.Validators.OrderMetadataV2 where

import EulerHS.Prelude
import EulerHS.Extra.Validation

import           Euler.Common.Validators

import qualified Euler.Product.Domain.OrderMetadataV2 as D
import qualified Euler.Storage.Types.OrderMetadataV2 as S


transformOrderMetadataV2 :: S.OrderMetadataV2 -> V D.OrderMetadataV2
transformOrderMetadataV2 r = D.OrderMetadataV2
  <$> (D.OrderMetadataV2PId <$> withField @"id" r (extractJust >=> notNegative))
  <*> withField @"browser" r pure
  <*> withField @"browserVersion" r pure
  <*> withField @"dateCreated" r pure
  <*> withField @"device" r pure
  <*> withField @"lastUpdated" r pure
  <*> withField @"metadata" r pure
  <*> withField @"mobile" r pure
  <*> withField @"operatingSystem" r pure
  <*> withField @"orderReferenceId" r notNegative
  <*> withField @"ipAddress" r pure
  <*> withField @"referer" r pure
  <*> withField @"userAgent" r pure