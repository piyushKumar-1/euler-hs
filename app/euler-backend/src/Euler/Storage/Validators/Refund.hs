{-# LANGUAGE AllowAmbiguousTypes #-}

module Euler.Storage.Validators.Refund where

import EulerHS.Prelude
import EulerHS.Extra.Validation

import           Euler.Common.Validators
import           Euler.Common.Types.Money
import qualified Euler.Product.Domain.Refund as D
import qualified Euler.Storage.Types.Refund as S

transformRefund :: S.Refund -> V D.Refund
transformRefund r = D.Refund
  <$> (D.RefundPId <$> withField @"id" r (extractJust >=> notNegative))
  <*> (mkMoney <$> withField @"amount" r amountValidators)
  <*> withField @"authorizationId" r pure
  <*> withField @"dateCreated" r pure
  <*> withField @"epgTxnId" r pure
  <*> withField @"gateway" r textNotEmpty
  <*> withField @"processed" r pure
  <*> withField @"rootReferenceNumber" r pure
  <*> withField @"txnDetailId" r pure
  <*> withField @"referenceId" r pure
  <*> withField @"status" r pure
  <*> withField @"uniqueRequestId" r pure
  <*> withField @"errorMessage" r pure
  <*> withField @"sentToGateway" r pure
  <*> withField @"responseCode" r pure
  <*> withField @"internalReferenceId" r pure
  <*> withField @"refundArn" r pure
  <*> withField @"initiatedBy" r pure
  <*> withField @"refundType" r pure
  <*> withField @"refundSource" r pure
  <*> withField @"lastModified" r pure