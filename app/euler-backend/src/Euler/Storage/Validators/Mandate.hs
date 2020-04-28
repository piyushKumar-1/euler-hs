{-# LANGUAGE AllowAmbiguousTypes #-}

module Euler.Storage.Validators.Mandate where

import EulerHS.Prelude
import EulerHS.Extra.Validation

import           Euler.Common.Validators

import qualified Euler.Common.Types as C
import qualified Euler.Product.Domain.Mandate as D
import qualified Euler.Storage.Types.Mandate as S


transformMandate :: S.Mandate -> V D.Mandate
transformMandate r = D.Mandate
  <$> (D.MandatePId <$> withField @"id" r (extractJust >=> notNegative))
  <*> withField @"merchantId" r textNotEmpty
  <*> withField @"endDate" r pure
  <*> withField @"startDate" r pure
  <*> (fmap C.mkMoney <$> withField @"maxAmount" r (insideJust amountValidators))
  <*> withField @"merchantCustomerId" r pure
  <*> withField @"paymentMethod" r pure
  <*> withField @"paymentMethodType" r pure
  <*> withField @"status" r pure
  <*> withField @"token" r pure
  <*> withField @"mandateId" r pure
  <*> withField @"paymentMethodId" r pure
  <*> withField @"gateway" r pure
  <*> withField @"gatewayParams" r pure
  <*> withField @"authOrderId" r pure
  <*> withField @"activatedAt" r pure
  <*> withField @"dateCreated" r pure
  <*> withField @"lastModified" r pure
  <*> withField @"authTxnCardInfo" r pure
  <*> withField @"currency" r pure
  <*> withField @"merchantGatewayAccountId" r pure
  <*> withField @"metadata" r pure