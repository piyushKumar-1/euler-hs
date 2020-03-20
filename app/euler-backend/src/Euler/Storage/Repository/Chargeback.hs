module Euler.Storage.Repository.Chargeback where

import           EulerHS.Prelude hiding (id)

import           Euler.Storage.DBConfig
import           EulerHS.Extra.Validation
import           EulerHS.Language

import           Euler.Common.Errors.PredefinedErrors
import           Euler.Common.Types.Money
import           Euler.Common.Validators (textNotEmpty, amountValidators, notNegative)
import qualified Euler.Product.Domain.Chargeback as D
import           Euler.Common.Types.Money
import qualified Euler.Storage.Types.Chargeback as S
import           Euler.Storage.Types.EulerDB as EDB

import qualified Data.Text as T
import           Database.Beam ((==.))
import qualified Database.Beam as B


findChargebacks :: Int -> Flow [D.Chargeback]
findChargebacks txnId = do
  chargebacks <- withDB eulerDB $ do
    let predicate S.Chargeback {txnDetailId}
          = txnDetailId ==. B.just_ (B.val_ txnId)
    findRows
      $ B.select
      $ B.filter_ predicate
      $ B.all_ (chargeback eulerDBSchema)

  case (traverse transformChargeback chargebacks) of
    Success chargebacks' -> pure chargebacks'
    Failure e -> do
      logError "Incorrect chargeback(s) in DB"
        $  "txnDetailId: " <> show txnId <> "error: " <> show e
      throwException internalError



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
