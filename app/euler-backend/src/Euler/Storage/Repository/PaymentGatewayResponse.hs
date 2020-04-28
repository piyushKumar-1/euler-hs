module Euler.Storage.Repository.PaymentGatewayResponse
       ( loadPGR
       ) where

import           EulerHS.Prelude hiding (id)

import           EulerHS.Language
import           WebService.Language

import           Euler.Storage.Repository.EulerDB
import qualified Euler.Storage.Types                  as DB
import           Euler.Storage.Validators.PaymentGatewayResponse
import qualified Euler.Product.Domain as D
import           EulerHS.Extra.Validation

import           Euler.Common.Errors.PredefinedErrors

import           Database.Beam ((==.))
import qualified Database.Beam as B



loadPGR :: Maybe Int -> Flow (Maybe D.PaymentGatewayResponse)
loadPGR Nothing = pure Nothing
loadPGR (Just successResponseId) = do
  mPgr <- withEulerDB $ do
    let predicate DB.PaymentGatewayResponse {id} =
          id ==. B.just_ (B.val_ $ successResponseId)
    findRow
      $ B.select
      $ B.limit_ 1
      $ B.filter_ predicate
      $ B.all_ (DB.payment_gateway_response DB.eulerDBSchema)
  case traverse transformPaymentGatewayResponse mPgr of
    Success pgr -> pure pgr
    Failure e -> do
      logErrorT "Incorrect PaymentGatewayResponse in DB"
        $  "successResponseId: " <> show successResponseId
        <> " error: " <> show e
      throwException internalError