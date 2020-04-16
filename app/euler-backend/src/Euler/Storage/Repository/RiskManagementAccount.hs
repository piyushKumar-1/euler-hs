module Euler.Storage.Repository.RiskManagementAccount where

import           EulerHS.Prelude hiding (id)

import           EulerHS.Extra.Validation
import           EulerHS.Language
import           WebService.Language

import           Euler.Common.Errors.PredefinedErrors
import           Euler.Common.Validators (textNotEmpty, notNegative)
import qualified Euler.Product.Domain.RiskManagementAccount as D
import           Euler.Storage.Repository.EulerDB
import qualified Euler.Storage.Types as DB

import           Database.Beam ((==.))
import qualified Database.Beam as B


loadRiskManagementAccount :: Int -> Flow (Maybe D.RiskManagementAccount)
loadRiskManagementAccount riskMAId = do
  riskMA <- withEulerDB $ do
    let predicate DB.RiskManagementAccount {id} = id ==. B.val_ riskMAId
    findRow
      $ B.select
      $ B.limit_ 1
      $ B.filter_ predicate
      $ B.all_ (DB.risk_management_account DB.eulerDBSchema)
  case (traverse transformRiskManagementAccount riskMA) of
    Success risk -> pure risk
    Failure e -> do
      logErrorT "Incorrect RiskManagementAccount in DB"
        $  "RiskManagementAccount id: " <> show riskMAId <> "error: " <> show e
      throwException internalError

transformRiskManagementAccount :: DB.RiskManagementAccount -> V D.RiskManagementAccount
transformRiskManagementAccount r = D.RiskManagementAccount
  <$> (D.RiskManagementAccountPId <$> withField @"id" r notNegative)
  <*> withField @"version" r pure
  <*> withField @"accountDetailsJson" r textNotEmpty
  <*> withField @"merchantAccountId" r pure
  <*> withField @"provider" r pure
  <*> withField @"dateCreated" r pure
  <*> withField @"lastUpdated" r pure
  <*> withField @"riskDsl" r pure
