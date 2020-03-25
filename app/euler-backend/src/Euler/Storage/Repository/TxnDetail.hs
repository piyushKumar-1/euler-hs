module Euler.Storage.Repository.TxnDetail where

import           EulerHS.Prelude hiding (id)

import           EulerHS.Extra.Validation
import           EulerHS.Language

import qualified Euler.Common.Types as C

import           Euler.Common.Errors.PredefinedErrors
import           Euler.Common.Validators (amountValidators, isGateway, notNegative, textNotEmpty)

import qualified Euler.Product.Domain as D

import           Euler.Storage.DBConfig
import qualified Euler.Storage.Types.TxnDetail as TDB
import qualified Euler.Storage.Types as DB

import qualified Data.Text as T
import           Database.Beam ((&&.), (==.))
import qualified Database.Beam as B



loadTxnDetail
  :: C.OrderId
  -> C.MerchantId
  -> Text
  -> Flow (Maybe D.TxnDetail)
loadTxnDetail orderId' merchantId' txnUuid' = do
  td <- withDB eulerDB $ do
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
      logError "Incorrect txnDetail(s) in DB"
        $  "orderId: " <> orderId'
        <>  " merchantId: " <> merchantId'
        <>  " txnUuid: " <> txnUuid'
        <> " error: " <> show e
      throwException internalError


loadTxnDetails :: C.OrderId -> C.MerchantId -> Flow [D.TxnDetail]
loadTxnDetails orderId' merchantId' = do
  td <- withDB eulerDB $ do
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
      logError "Incorrect txnDetail(s) in DB"
        $  "orderId: " <> orderId'
        <>  " merchantId: " <> merchantId'
        <> " error: " <> show e
      throwException internalError


transformTxnDetail :: DB.TxnDetail -> V D.TxnDetail
transformTxnDetail r = D.TxnDetail
  <$> (D.TxnDetailPId<$> withField @"id" r (extractJust >=> notNegative))
  <*> withField @"version" r pure
  <*> withField @"errorMessage" r pure
  <*> withField @"orderId" r textNotEmpty
  <*> withField @"status" r pure
  <*> withField @"txnId" r textNotEmpty
  <*> withField @"txdType" r textNotEmpty
  <*> withField @"dateCreated" r pure
  <*> withField @"lastModified" r pure
  <*> withField @"successResponseId" r pure
  <*> withField @"txnMode" r pure
  <*> withField @"addToLocker" r pure
  <*> withField @"merchantId" r pure
  <*> withField @"bankErrorCode" r pure
  <*> withField @"bankErrorMessage" r pure
  <*> withField @"gateway" r (insideJust $ decode >=> isGateway)
  <*> withField @"expressCheckout" r pure
  <*> withField @"redirect" r pure
  <*> withField @"gatewayPayload" r pure
  <*> withField @"isEmi" r pure
  <*> withField @"emiBank" r pure
  <*> withField @"emiTenure" r pure
  <*> withField @"username" r pure
  <*> withField @"txnUuid" r pure
  <*> withField @"merchantGatewayAccountId" r pure
  <*> (fmap C.mkMoney <$> withField @"txnAmount" r (insideJust amountValidators))
  <*> withField @"txnObjectType" r pure
  <*> withField @"sourceObject" r pure
  <*> withField @"sourceObjectId" r pure
  <*> withField @"currency" r pure
  <*> (fmap C.mkMoney <$> withField @"netAmount" r (insideJust amountValidators))
  <*> (fmap C.mkMoney <$> withField @"surchargeAmount" r (insideJust amountValidators))
  <*> (fmap C.mkMoney <$> withField @"taxAmount" r (insideJust amountValidators))
