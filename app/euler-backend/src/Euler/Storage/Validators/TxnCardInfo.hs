{-# LANGUAGE AllowAmbiguousTypes #-}

module Euler.Storage.Validators.TxnCardInfo where

import EulerHS.Prelude
import EulerHS.Extra.Validation

import           Euler.Common.Validators

import qualified Euler.Product.Domain.TxnCardInfo as D
import qualified Euler.Storage.Types.TxnCardInfo as S

transformTxnCardInfo :: S.TxnCardInfo -> V D.TxnCardInfo
transformTxnCardInfo r = D.TxnCardInfo
  <$> (D.TxnCardInfoPId <$> withField @"id" r (extractJust >=> notNegative))
  <*> withField @"txnId" r textNotEmpty
  <*> withField @"cardIsin" r pure
  <*> withField @"cardIssuerBankName" r pure
  <*> withField @"cardExpYear" r pure
  <*> withField @"cardExpMonth" r pure
  <*> withField @"cardSwitchProvider" r pure
  <*> withField @"cardType" r pure
  <*> withField @"cardLastFourDigits" r pure
  <*> withField @"nameOnCard" r pure
  <*> withField @"cardFingerprint" r pure
  <*> withField @"cardReferenceId" r pure
  <*> withField @"txnDetailId" r (insideJust notNegative)
  <*> withField @"dateCreated" r pure
  <*> withField @"paymentMethodType" r pure
  <*> withField @"paymentMethod" r pure
  <*> withField @"cardGlobalFingerprint" r pure
  <*> withField @"paymentSource" r pure
  <*> withField @"authType" r pure