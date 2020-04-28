{-# LANGUAGE AllowAmbiguousTypes #-}

module Euler.Storage.Validators.TxnRiskCheck where

import EulerHS.Prelude
import EulerHS.Extra.Validation

import           Euler.Common.Validators

import qualified Euler.Product.Domain.TxnRiskCheck as D
import qualified Euler.Storage.Types.TxnRiskCheck as S


transformTxnRiskCheck :: S.TxnRiskCheck -> V D.TxnRiskCheck
transformTxnRiskCheck r = D.TxnRiskCheck
  <$> (D.TxnRiskCheckPId <$> withField @"id" r notNegative)
  <*> withField @"completeResponse" r textNotEmpty
  <*> withField @"dateCreated" r pure
  <*> withField @"flagged" r pure
  <*> withField @"lastUpdated" r pure
  <*> withField @"recommendedAction" r pure
  <*> withField @"resultJson" r pure
  <*> withField @"riskManagementAccountId" r notNegative
  <*> withField @"txnDetailId" r notNegative
  <*> withField @"message" r pure
  <*> withField @"status" r pure
  <*> withField @"riskStatus" r pure
  <*> withField @"domestic" r pure
  <*> withField @"invocationMode" r pure
  <*> withField @"paymentStatusUpdateResponseCode" r pure
  <*> withField @"paymentStatusUpdated" r pure