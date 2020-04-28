{-# LANGUAGE AllowAmbiguousTypes #-}

module Euler.Storage.Validators.SecondFactorResponse where

import EulerHS.Prelude
import EulerHS.Extra.Validation

import           Euler.Common.Validators

import qualified Euler.Product.Domain.SecondFactorResponse as D
import qualified Euler.Storage.Types.SecondFactorResponse as S


transformSecondFactorResponse :: S.SecondFactorResponse -> V D.SecondFactorResponse
transformSecondFactorResponse sfr = D.SecondFactorResponse
  <$> (D.SecondFactorResponsePId <$> withField @"id" sfr (extractJust >=> notNegative))
  <*> withField @"version" sfr pure
  <*> withField @"cavv" sfr pure
  <*> withField @"currency" sfr pure
  <*> withField @"eci" sfr textNotEmpty
  <*> withField @"mpiErrorCode" sfr pure
  <*> withField @"purchaseAmount" sfr pure
  <*> withField @"responseId" sfr pure
  <*> withField @"shoppingContext" sfr textNotEmpty
  <*> withField @"status" sfr textNotEmpty
  <*> withField @"xid" sfr textNotEmpty
  <*> withField @"dateCreated" sfr pure
  <*> withField @"secondFactorId" sfr pure
  <*> withField @"gatewayAuthResData" sfr pure