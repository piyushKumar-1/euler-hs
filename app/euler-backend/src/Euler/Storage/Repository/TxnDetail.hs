module Euler.Storage.Repository.TxnDetail where

import           EulerHS.Prelude hiding (id)

import           Euler.Storage.DBConfig
import           EulerHS.Extra.Validation
import           EulerHS.Language

import           Euler.Common.Errors.PredefinedErrors
import           Euler.Common.Types.Gateway
import           Euler.Common.Types.Money
import           Euler.Common.Validators (amountValidators, isGateway, notNegative, textNotEmpty)
-- import           Euler.Common.Types.TxnDetail (TxnDetId)

import qualified Euler.Product.Domain as D

import qualified Euler.Storage.Types as DB

import qualified Data.Text as T
import           Database.Beam ((&&.), (==.))
import qualified Database.Beam as B



findTxnByOrderIdMerchantIdTxnuuidId
  :: Maybe Text
  -> Maybe Text
  -> Maybe Text
  -> Flow (Maybe D.TxnDetail)
findTxnByOrderIdMerchantIdTxnuuidId (Just orderId') (Just merchantId') (Just txnUuid') = do
  -- EHS: TODO should we throw exceptions?
      -- orderId' <- whenNothing (getField @"orderId" order) (throwException err500)
      -- merchantId' <- whenNothing (getField @"merchantId" order) (throwException err500)
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
findTxnByOrderIdMerchantIdTxnuuidId _ _ _ = pure Nothing

-- EHS: TODO add sorting by dateCreated!!!
findTxnByOrderIdMerchantId :: Maybe Text -> Maybe Text -> Flow [D.TxnDetail]
findTxnByOrderIdMerchantId (Just orderId') (Just merchantId') = do
  -- EHS: TODO should we throw exceptions?
  -- orderId' <- whenNothing (getField @"orderId" orderRef) (throwException err500)
  -- merchantId' <- whenNothing (getField @"merchantId" orderRef) (throwException err500)
  td <- withDB eulerDB $ do
    let predicate DB.TxnDetail {orderId, merchantId} =
          orderId ==. B.val_ orderId'
            &&. merchantId ==. B.just_ (B.val_ merchantId')
    findRows
      $ B.select
      $ B.filter_ predicate
      $ B.all_ (DB.txn_detail DB.eulerDBSchema)
  case traverse transformTxnDetail td of
    Success td' -> pure td'
    Failure e -> do
      logError "Incorrect txnDetail(s) in DB"
        $  "orderId: " <> orderId'
        <>  " merchantId: " <> merchantId'
        <> " error: " <> show e
      throwException internalError
findTxnByOrderIdMerchantId _ _ = pure []


transformTxnDetail :: DB.TxnDetail -> V D.TxnDetail
transformTxnDetail r = D.TxnDetail
  <$> (D.TxnDetailId<$> withField @"id" r (extractJust >=> notNegative))
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
  <*> (fmap mkMoney <$> withField @"txnAmount" r (insideJust amountValidators))
  <*> withField @"txnObjectType" r pure
  <*> withField @"sourceObject" r pure
  <*> withField @"sourceObjectId" r pure
  <*> withField @"currency" r pure
  <*> (fmap mkMoney <$> withField @"netAmount" r (insideJust amountValidators))
  <*> (fmap mkMoney <$> withField @"surchargeAmount" r (insideJust amountValidators))
  <*> (fmap mkMoney <$> withField @"taxAmount" r (insideJust amountValidators))
