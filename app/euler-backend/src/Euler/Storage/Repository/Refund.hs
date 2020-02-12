module Euler.Storage.Repository.Refund where

import           EulerHS.Prelude hiding (id)

import           Euler.Storage.DBConfig
import           EulerHS.Extra.Validation
import           EulerHS.Language

import           Euler.Common.Errors.PredefinedErrors
import           Euler.Product.Domain.Money
import qualified Euler.Product.Domain.Refund as D
import           Euler.Storage.Types.EulerDB as EDB
import qualified Euler.Storage.Types.Refund as S

import qualified Data.Text as T
import           Database.Beam ((==.))
import qualified Database.Beam as B



type TxnDetailId = Text

findRefunds :: TxnDetailId -> Flow [D.Refund]
findRefunds txnId = do
  rs <- withDB eulerDB $ do
    let predicate S.Refund {txnDetailId} = txnDetailId ==. B.just_ (B.val_ txnId)
    findRows
      $ B.select
      $ B.filter_ predicate
      -- TODO: $ B.orderBy_ (B.asc_ . ???dateCreated???)
      $ B.all_ (EDB.refund eulerDBSchema)
  case (traverse transformRefund rs) of
    Success rs' -> pure rs'
    Failure e -> do
      logError "Incorrect refund(s) in DB"
        $  "txnDetailId: " <> txnId <> "error: " <> show e
      throwException internalError


transformRefund :: S.Refund -> V D.Refund
transformRefund r = D.Refund
  <$> (D.RefundId <$> withField @"id" r (extractJust >=> textNotEmpty))
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

