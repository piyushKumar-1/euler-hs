module Euler.Storage.Repository.TxnRiskCheck where

import           EulerHS.Prelude hiding (id)

import           EulerHS.Extra.Validation
import           EulerHS.Language
import           WebService.Language

import           Euler.Common.Errors.PredefinedErrors
import qualified Euler.Product.Domain.TxnRiskCheck as D
import           Euler.Storage.Repository.EulerDB
import qualified Euler.Storage.Types as DB
import           Euler.Storage.Validators.TxnRiskCheck

import           Database.Beam ((==.))
import qualified Database.Beam as B


loadTxnRiskCheck :: Int -> Flow (Maybe D.TxnRiskCheck)
loadTxnRiskCheck txnId = do
  risk <- withEulerDB $ do
    let predicate DB.TxnRiskCheck {txnDetailId} = txnDetailId ==. B.val_ txnId
    findRow
      $ B.select
      $ B.limit_ 1
      $ B.filter_ predicate
      $ B.all_ (DB.txn_risk_check DB.eulerDBSchema)
  case (traverse transformTxnRiskCheck risk) of
    Success rsk -> pure rsk
    Failure e -> do
      logErrorT "Incorrect txnRiskCheck(s) in DB"
        $  "txnDetailId: " <> show txnId <> "error: " <> show e
      throwException internalError