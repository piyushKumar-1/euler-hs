{-# LANGUAGE NamedFieldPuns #-}

module Euler.Storage.Repository.Promotion where

import           EulerHS.Prelude hiding (Key, id)
import           EulerHS.Prelude as P

import           EulerHS.Extra.Validation
import           EulerHS.Language

import           Euler.Common.Errors.PredefinedErrors
import qualified Euler.Common.Types as C
import           Euler.Common.Validators (amountValidators, notNegative, textNotEmpty)

import qualified Euler.Product.Domain as D

import           Euler.Storage.DBConfig
import qualified Euler.Storage.Types as DB

import           Database.Beam ((==.))
import qualified Database.Beam as B



loadPromotions :: C.OrderPId -> Flow [D.Promotion]
loadPromotions orderPId = do
  proms  <- withDB eulerDB $ do
    let predicate DB.Promotion{orderReferenceId} =
          orderReferenceId ==. B.just_ (B.val_ orderPId)
    findRows
      $ B.select
      $ B.filter_ predicate
      $ B.all_ (DB.promotions DB.eulerDBSchema)
  case traverse transformPromotions proms of
    Success pr -> pure pr
    Failure e -> do
      logError "Incorrect Promotion(s) in DB"
        $  "orderPId: " <> show orderPId
        <> " error: " <> show e
      throwException internalError


transformPromotions :: DB.Promotion -> V D.Promotion
transformPromotions r = D.Promotion
  <$> (D.PromotionPId <$> withField @"id" r notNegative)
  <*> withField @"dateCreated" r pure
  <*> (C.mkMoney <$> withField @"discountAmount" r amountValidators)
  <*> withField @"lastModified" r pure
  <*> withField @"orderId" r (insideJust notNegative)
  <*> withField @"rules" r textNotEmpty
  <*> withField @"status" r textNotEmpty
  <*> withField @"orderReferenceId" r (insideJust notNegative)


