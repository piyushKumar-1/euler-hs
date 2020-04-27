module Euler.Storage.Repository.TxnCardInfo where

import           EulerHS.Prelude hiding (id)

import           EulerHS.Extra.Validation
import           EulerHS.Language
import           WebService.Language

import           Euler.Common.Errors.PredefinedErrors
import           Euler.Common.Validators (textNotEmpty, notNegative)
import qualified Euler.Product.Domain.TxnCardInfo as D
import           Euler.Storage.Repository.EulerDB
import qualified Euler.Storage.Types as DB

import           Database.Beam ((==.))
import qualified Database.Beam as B


loadTxnCardInfo :: Int -> Flow (Maybe D.TxnCardInfo)
loadTxnCardInfo txnId = do
  cardInfo <- withEulerDB $ do
    let predicate DB.TxnCardInfo {txnDetailId} = txnDetailId ==. B.just_ (B.val_ txnId)
    findRow
      $ B.select
      $ B.limit_ 1
      $ B.filter_ predicate
      $ B.all_ (DB.txn_card_info DB.eulerDBSchema)
  case (traverse transformTxnCardInfo cardInfo) of
    Success card -> pure card
    Failure e -> do
      logErrorT "Incorrect TxnCardInfo in DB"
        $  "txnDetailId: " <> show txnId <> "error: " <> show e
      throwException internalError

transformTxnCardInfo :: DB.TxnCardInfo -> V D.TxnCardInfo
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
