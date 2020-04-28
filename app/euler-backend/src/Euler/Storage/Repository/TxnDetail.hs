module Euler.Storage.Repository.TxnDetail where

import           EulerHS.Prelude hiding (id)

import           EulerHS.Extra.Validation
import           EulerHS.Language
import           WebService.Language

import qualified Euler.Common.Types as C
import           Euler.Common.Errors.PredefinedErrors
import qualified Euler.Product.Domain as D
import           Euler.Storage.Repository.EulerDB
import qualified Euler.Storage.Types.TxnDetail as TDB
import qualified Euler.Storage.Types as DB
import           Euler.Storage.Validators.TxnDetail

import           Database.Beam ((&&.), (==.))
import qualified Database.Beam as B



loadTxnDetail
  :: C.OrderId
  -> C.MerchantId
  -> Text
  -> Flow (Maybe D.TxnDetail)
loadTxnDetail orderId' merchantId' txnUuid' = do
  td <- withEulerDB $ do
    let predicate DB.TxnDetail {orderId, merchantId, txnUuid} =
          orderId ==. B.val_ orderId'
          &&. merchantId ==. B.just_ (B.val_ merchantId')
          &&. txnUuid ==. B.just_ (B.val_ txnUuid')
    findRow
      $ B.select
      $ B.limit_ 1
      $ B.filter_ predicate
      $ B.all_ (DB.txn_detail DB.eulerDBSchema)
  case traverse transformTxnDetail td of
    Success td' -> pure td'
    Failure e -> do
      logErrorT "Incorrect txnDetail(s) in DB"
        $  "orderId: " <> orderId'
        <>  " merchantId: " <> merchantId'
        <>  " txnUuid: " <> txnUuid'
        <> " error: " <> show e
      throwException internalError


loadTxnDetails :: C.OrderId -> C.MerchantId -> Flow [D.TxnDetail]
loadTxnDetails orderId' merchantId' = do
  td <- withEulerDB $ do
    let predicate DB.TxnDetail {orderId, merchantId} =
          orderId ==. B.val_ orderId'
            &&. merchantId ==. B.just_ (B.val_ merchantId')
    findRows
      $ B.select
      $ B.filter_ predicate
      $ B.orderBy_ (B.desc_ . TDB.dateCreated)
      $ B.all_ (DB.txn_detail DB.eulerDBSchema)
  case traverse transformTxnDetail td of
    Success td' -> pure td'
    Failure e -> do
      logErrorT "Incorrect txnDetail(s) in DB"
        $  "orderId: " <> orderId'
        <>  " merchantId: " <> merchantId'
        <> " error: " <> show e
      throwException internalError