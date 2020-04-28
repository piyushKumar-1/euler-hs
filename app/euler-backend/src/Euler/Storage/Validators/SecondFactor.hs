{-# LANGUAGE AllowAmbiguousTypes #-}

module Euler.Storage.Validators.SecondFactor where

import EulerHS.Prelude
import EulerHS.Extra.Validation

import           Euler.Common.Validators

import qualified Euler.Product.Domain.SecondFactor as D
import qualified Euler.Storage.Types.SecondFactor as S

transformSecondFactor :: S.SecondFactor -> V D.SecondFactor
transformSecondFactor sf = D.SecondFactor
  <$> (D.SecondFactorPId <$> withField @"id" sf (extractJust >=> notNegative))
  <*> withField @"version" sf pure
  <*> withField @"otp" sf pure
  <*> withField @"status" sf textNotEmpty
  <*> withField @"txnId" sf textNotEmpty
  <*> withField @"sfType" sf textNotEmpty
  <*> withField @"url" sf pure
  <*> withField @"secondFactorResponse" sf pure
  <*> withField @"dateCreated" sf pure
  <*> withField @"epgTxnId" sf pure
  <*> withField @"lastUpdated" sf pure
  <*> withField @"txnDetailId" sf pure
  <*> withField @"gatewayAuthReqParams" sf pure
  <*> withField @"authenticationAccountId" sf pure
  <*> withField @"canAcceptResponse" sf pure
  <*> withField @"challengesAttempted" sf pure
  <*> withField @"responseAttempted" sf pure