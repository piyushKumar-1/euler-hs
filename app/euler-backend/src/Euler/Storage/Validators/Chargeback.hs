{-# LANGUAGE AllowAmbiguousTypes #-}

module Euler.Storage.Validators.Chargeback where

import EulerHS.Prelude
import EulerHS.Extra.Validation

import           Euler.Common.Types.Money
import           Euler.Common.Validators


import qualified Euler.Product.Domain.Chargeback as D
import qualified Euler.Storage.Types.Chargeback as S

transformChargeback :: S.Chargeback -> V D.Chargeback
transformChargeback r = D.Chargeback
  <$> (D.ChargebackPId <$> withField @"id" r textNotEmpty)
  <*> withField @"version" r pure -- TODO: validate version?
  <*> (mkMoney <$> withField @"amount" r amountValidators)
  <*> withField @"dateCreated" r pure
  <*> withField @"dateResolved" r pure
  <*> withField @"disputeStatus" r textNotEmpty
  <*> withField @"lastUpdated" r pure
  <*> withField @"merchantAccountId" r pure -- TODO: validate merchantAccountId?
  <*> withField @"txnDetailId" r (insideJust notNegative)
  <*> withField @"objectReferenceId" r textNotEmpty