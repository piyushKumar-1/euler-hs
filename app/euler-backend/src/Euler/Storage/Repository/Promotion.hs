{-# LANGUAGE NamedFieldPuns #-}

module Euler.Storage.Repository.Promotion where

import           EulerHS.Prelude hiding (Key, id)

import           EulerHS.Extra.Validation
import           EulerHS.Language
import           WebService.Language

import           Euler.Common.Errors.PredefinedErrors
import qualified Euler.Common.Types as C
import qualified Euler.Product.Domain as D
import           Euler.Storage.Repository.EulerDB
import qualified Euler.Storage.Types as DB
import           Euler.Storage.Validators.Promotion


import           Database.Beam ((==.))
import qualified Database.Beam as B



loadPromotions :: C.OrderPId -> Flow [D.Promotion]
loadPromotions orderPId = do
  proms  <- withEulerDB $ do
    let predicate DB.Promotion{orderReferenceId} =
          orderReferenceId ==. B.just_ (B.val_ orderPId)
    findRows
      $ B.select
      $ B.filter_ predicate
      $ B.all_ (DB.promotions DB.eulerDBSchema)
  case traverse transformPromotions proms of
    Success pr -> pure pr
    Failure e -> do
      logErrorT "Incorrect Promotion(s) in DB"
        $  "orderPId: " <> show orderPId
        <> " error: " <> show e
      throwException internalError