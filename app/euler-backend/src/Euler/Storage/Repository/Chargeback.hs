module Euler.Storage.Repository.Chargeback where

import           EulerHS.Prelude hiding (id)

import           EulerHS.Extra.Validation
import           EulerHS.Language
import           WebService.Language

import           Euler.Common.Errors.PredefinedErrors
import qualified Euler.Product.Domain.Chargeback as D
import qualified Euler.Storage.Types.Chargeback as S
import           Euler.Storage.Repository.EulerDB
import           Euler.Storage.Validators.Chargeback (transformChargeback)
import           Euler.Storage.Types.EulerDB as EDB

import           Database.Beam ((==.))
import qualified Database.Beam as B


findChargebacks :: Int -> Flow [D.Chargeback]
findChargebacks txnId = do
  chargebacks <- withEulerDB $ do
    let predicate S.Chargeback {txnDetailId}
          = txnDetailId ==. B.just_ (B.val_ txnId)
    findRows
      $ B.select
      $ B.filter_ predicate
      $ B.all_ (chargeback eulerDBSchema)

  case (traverse transformChargeback chargebacks) of
    Success chargebacks' -> pure chargebacks'
    Failure e -> do
      logErrorT "Incorrect chargeback(s) in DB"
        $  "txnDetailId: " <> show txnId <> "error: " <> show e
      throwException internalError