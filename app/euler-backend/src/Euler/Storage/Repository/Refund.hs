module Euler.Storage.Repository.Refund where

import           EulerHS.Prelude hiding (id)

import           EulerHS.Extra.Validation
import           EulerHS.Language
import           WebService.Language

import           Euler.Common.Errors.PredefinedErrors

import qualified Euler.Product.Domain.Refund as D

import           Euler.Storage.Repository.EulerDB
import qualified Euler.Storage.Types.Refund as RDB
import qualified Euler.Storage.Types as DB
import           Euler.Storage.Validators.Refund

import           Database.Beam ((==.))
import qualified Database.Beam as B



loadRefunds :: Int -> Flow [D.Refund]
loadRefunds txnId = do
  rs <- withEulerDB $ do
    let predicate DB.Refund {txnDetailId} = txnDetailId ==. B.just_ (B.val_ txnId)
    findRows
      $ B.select
      $ B.filter_ predicate
      $ B.orderBy_ (B.asc_ . RDB.dateCreated)
      $ B.all_ (DB.refund DB.eulerDBSchema)
  case (traverse transformRefund rs) of
    Success rs' -> pure rs'
    Failure e -> do
      logErrorT "Incorrect refund(s) in DB"
        $  "txnDetailId: " <> show txnId <> "error: " <> show e
      throwException internalError