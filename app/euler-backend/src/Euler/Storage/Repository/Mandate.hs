module Euler.Storage.Repository.Mandate where

import           EulerHS.Prelude hiding (id)

import qualified Prelude as P (show)

import           Euler.Storage.DBConfig
import           EulerHS.Extra.Validation
import           EulerHS.Language

import           Euler.Common.Errors.PredefinedErrors
import qualified Euler.Common.Types as C
import           Euler.Common.Validators (amountValidators, notNegative, textNotEmpty)

import qualified Euler.Product.Domain.Mandate as D
import           Euler.Storage.Types.EulerDB
import qualified Euler.Storage.Types.Mandate as S

import           Database.Beam ((&&.), (==.))
import qualified Database.Beam as B
import           Servant.Server (err500)


loadMandate :: C.OrderPId -> Text -> Flow (Maybe D.Mandate)
loadMandate orderPId merchId = do
  conn <- getConn eulerDB
  let predicate S.Mandate {authOrderId, merchantId} =
        authOrderId ==. B.just_ (B.val_ orderPId) &&. merchantId ==. B.val_ merchId
  res <- runDB conn $
    findRow
      $ B.select
      $ B.limit_ 1
      $ B.filter_ predicate
      $ B.all_ (mandate eulerDBSchema)
  case res of
    Right mMandate -> do
        case (traverse transformMandate mMandate) of
          Success m -> pure m
          Failure e -> do
            logError "Incorrect mandate in DB"
              $ "OrderReference id: " <> show orderPId
              <> " merchantId " <> merchId
              <> " error: " <> show e
            throwException internalError
    Left err -> do
      logError "Find Mandate" $ toText $ P.show err
      throwException err500


transformMandate :: S.Mandate -> V D.Mandate
transformMandate r = D.Mandate
  <$> (D.MandatePId <$> withField @"id" r (extractJust >=> notNegative))
  <*> withField @"merchantId" r textNotEmpty
  <*> withField @"endDate" r pure
  <*> withField @"startDate" r pure
  <*> (fmap C.mkMoney <$> withField @"maxAmount" r (insideJust amountValidators))
  <*> withField @"merchantCustomerId" r pure
  <*> withField @"paymentMethod" r pure
  <*> withField @"paymentMethodType" r pure
  <*> withField @"status" r pure
  <*> withField @"token" r pure
  <*> withField @"mandateId" r pure
  <*> withField @"paymentMethodId" r pure
  <*> withField @"gateway" r pure
  <*> withField @"gatewayParams" r pure
  <*> withField @"authOrderId" r pure
  <*> withField @"activatedAt" r pure
  <*> withField @"dateCreated" r pure
  <*> withField @"lastModified" r pure
  <*> withField @"authTxnCardInfo" r pure
  <*> withField @"currency" r pure
  <*> withField @"merchantGatewayAccountId" r pure
  <*> withField @"metadata" r pure
