module Euler.Storage.Repository.Refund where

import           EulerHS.Prelude hiding (id)

import           Euler.Storage.DBConfig
import           EulerHS.Extra.Validation
import           EulerHS.Language

import           Euler.Common.Errors.PredefinedErrors
import           Euler.Common.Types.Money
import           Euler.Common.Validators (amountValidators, textNotEmpty, notNegative)

import qualified Euler.Product.Domain.Refund as D

import qualified Euler.Storage.Types.Refund as RDB
import qualified Euler.Storage.Types as DB

import qualified Data.Text as T
import           Database.Beam ((==.))
import qualified Database.Beam as B



loadRefunds :: Int -> Flow [D.Refund]
loadRefunds txnId = do
  rs <- withDB eulerDB $ do
    let predicate DB.Refund {txnDetailId} = txnDetailId ==. B.just_ (B.val_ txnId)
    findRows
      $ B.select
      $ B.filter_ predicate
      $ B.orderBy_ (B.asc_ . RDB.dateCreated)
      $ B.all_ (DB.refund DB.eulerDBSchema)
  case (traverse transformRefund rs) of
    Success rs' -> pure rs'
    Failure e -> do
      logError "Incorrect refund(s) in DB"
        $  "txnDetailId: " <> show txnId <> "error: " <> show e
      throwException internalError


transformRefund :: DB.Refund -> V D.Refund
transformRefund r = D.Refund
  <$> (D.RefundPId <$> withField @"id" r (extractJust >=> notNegative))
  <*> (mkMoney <$> withField @"amount" r amountValidators)
  <*> withField @"authorizationId" r pure
  <*> withField @"dateCreated" r pure
  <*> withField @"epgTxnId" r pure
  <*> withField @"gateway" r textNotEmpty
  <*> withField @"processed" r pure
  <*> withField @"rootReferenceNumber" r pure
  <*> withField @"txnDetailId" r pure
  <*> withField @"referenceId" r pure
  <*> withField @"status" r pure
  <*> withField @"uniqueRequestId" r pure
  <*> withField @"errorMessage" r pure
  <*> withField @"sentToGateway" r pure
  <*> withField @"responseCode" r pure
  <*> withField @"internalReferenceId" r pure
  <*> withField @"refundArn" r pure
  <*> withField @"initiatedBy" r pure
  <*> withField @"refundType" r pure
  <*> withField @"refundSource" r pure
  <*> withField @"lastModified" r pure


