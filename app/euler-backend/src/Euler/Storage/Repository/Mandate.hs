module Euler.Storage.Repository.Mandate where

import           EulerHS.Prelude hiding (id)

import           EulerHS.Extra.Validation
import           EulerHS.Language
import           WebService.Language

import           Euler.Common.Errors.PredefinedErrors
import qualified Euler.Common.Types as C
import qualified Euler.Product.Domain.Mandate as D
import           Euler.Storage.Types.EulerDB
import qualified Euler.Storage.Types.Mandate as S
import           Euler.Storage.Repository.EulerDB
import           Euler.Storage.Validators.Mandate

import           Database.Beam ((&&.), (==.))
import qualified Database.Beam as B



loadMandate :: C.OrderPId -> Text -> Flow (Maybe D.Mandate)
loadMandate orderPId merchId = do
  res <- withEulerDB $ do
    let predicate S.Mandate {authOrderId, merchantId} =
          authOrderId ==. B.just_ (B.val_ orderPId) &&. merchantId ==. B.val_ merchId
    findRow
      $ B.select
      $ B.limit_ 1
      $ B.filter_ predicate
      $ B.all_ (mandate eulerDBSchema)
  case (traverse transformMandate res) of
    Success m -> pure m
    Failure e -> do
      logErrorT "Incorrect mandate in DB"
        $ "OrderReference id: " <> show orderPId
        <> " merchantId " <> merchId
        <> " error: " <> show e
      throwException internalError