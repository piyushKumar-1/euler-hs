module Euler.Storage.Repository.TxnCardInfo where

import           EulerHS.Prelude hiding (id)

import           EulerHS.Extra.Validation
import           EulerHS.Language
import           WebService.Language

import           Euler.Common.Errors.PredefinedErrors
import qualified Euler.Product.Domain.TxnCardInfo as D
import           Euler.Storage.Repository.EulerDB
import qualified Euler.Storage.Types as DB
import           Euler.Storage.Validators.TxnCardInfo

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