module Euler.Storage.Repository.Chargeback where

import           EulerHS.Prelude hiding (id)

import           Euler.Storage.DBConfig
import           EulerHS.Extra.Validation
import           EulerHS.Language

import           Euler.Common.Errors.PredefinedErrors
import           Euler.Common.Types.TxnDetail (TxnDetailId)
import qualified Euler.Product.Domain.Chargeback as D
import           Euler.Product.Domain.Money
import qualified Euler.Storage.Types.Chargeback as S
import           Euler.Storage.Types.EulerDB as EDB

import qualified Data.Text as T
import           Database.Beam ((==.))
import qualified Database.Beam as B


findChargebacks :: TxnDetailId -> Flow [D.Chargeback]
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
        $  "txnDetailId: " <> txnId <> "error: " <> show e
      throwException internalError



transformChargeback :: S.Chargeback -> V D.Chargeback
transformChargeback r = D.Chargeback
  <$> (D.ChargebackId <$> withField @"id" r textNotEmpty)
  <*> withField @"version" r pure -- TODO: validate version?
  <*> (mkMoney <$> withField @"amount" r amountValidators)
  <*> withField @"dateCreated" r pure
  <*> withField @"dateResolved" r pure
  <*> withField @"disputeStatus" r textNotEmpty
  <*> withField @"lastUpdated" r pure
  <*> withField @"merchantAccountId" r pure -- TODO: validate merchantAccountId?
  <*> withField @"txnDetailId" r (insideJust textNotEmpty)
  <*> withField @"objectReferenceId" r textNotEmpty


-- Validators

-- EHS: move validators to separate module

textNotEmpty :: Validator Text
textNotEmpty = mkValidator "Can't be empty." (not . T.null)

amountValidators :: Validator Double
amountValidators =
  parValidate
    [ max2DecimalDigits
    , gteOne
    ]

--Will accept double values with upto two decimal places.
max2DecimalDigits :: Validator Double
max2DecimalDigits = mkValidator
  "Will accept double values with upto two decimal places."
  ((<=3) . length . dropWhile (/='.') . show)

gteOne :: Validator Double
gteOne = mkValidator
  "Should be greater than or equal 1"
  (>=1)
