{-# LANGUAGE AllowAmbiguousTypes #-}

module Euler.Storage.Validators.RiskManagementAccount where

import EulerHS.Prelude
import EulerHS.Extra.Validation

import           Euler.Common.Validators
import           Euler.Common.Types.Money
import qualified Euler.Product.Domain.RiskManagementAccount as D
import qualified Euler.Storage.Types.RiskManagementAccount as S

transformRiskManagementAccount :: S.RiskManagementAccount -> V D.RiskManagementAccount
transformRiskManagementAccount r = D.RiskManagementAccount
  <$> (D.RiskManagementAccountPId <$> withField @"id" r notNegative)
  <*> withField @"version" r pure
  <*> withField @"accountDetailsJson" r textNotEmpty
  <*> withField @"merchantAccountId" r pure
  <*> withField @"provider" r pure
  <*> withField @"dateCreated" r pure
  <*> withField @"lastUpdated" r pure
  <*> withField @"riskDsl" r pure