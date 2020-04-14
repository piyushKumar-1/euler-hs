module Euler.Storage.Repository.TxnRiskCheck where

import           EulerHS.Prelude hiding (id)

import           Euler.Storage.DBConfig
import           EulerHS.Extra.Validation
import           EulerHS.Language

import           Euler.Common.Errors.PredefinedErrors
import           Euler.Common.Validators (textNotEmpty, notNegative)

import qualified Euler.Product.Domain.TxnRiskCheck as D

import qualified Euler.Storage.Types as DB

import           Database.Beam ((==.))
import qualified Database.Beam as B


loadTxnRiskCheck :: Int -> Flow (Maybe D.TxnRiskCheck)
loadTxnRiskCheck txnId = do
  risk <- withDB eulerDB $ do
    let predicate DB.TxnRiskCheck {txnDetailId} = txnDetailId ==. B.val_ txnId
    findRow
      $ B.select
      $ B.limit_ 1
      $ B.filter_ predicate
      $ B.all_ (DB.txn_risk_check DB.eulerDBSchema)
  case (traverse transformTxnRiskCheck risk) of
    Success rsk -> pure rsk
    Failure e -> do
      logError "Incorrect txnRiskCheck(s) in DB"
        $  "txnDetailId: " <> show txnId <> "error: " <> show e
      throwException internalError

transformTxnRiskCheck :: DB.TxnRiskCheck -> V D.TxnRiskCheck
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
