module Euler.Storage.Repository.Feature where

import           EulerHS.Prelude hiding (id)

import           EulerHS.Extra.Validation
import           EulerHS.Language
import           WebService.Language

import qualified Euler.Constant.Feature               as Const
import           Euler.Common.Errors.PredefinedErrors
import qualified Euler.Common.Types as C

import qualified Euler.Product.Domain.Feature as D


import           Euler.Storage.Repository.EulerDB
import qualified Euler.Storage.Types as DB
import           Euler.Storage.Validators.Feature (transformFeature)

import           Database.Beam ((==.), (&&.))
import qualified Database.Beam as B


loadFeature :: Const.Feature -> C.MerchantId -> Flow (Maybe D.Feature)
loadFeature feat merchantId' = do
  feature <- withEulerDB $ do
    let predicate DB.Feature {name, merchantId} =
          name ==. B.val_ (Const.showFeature feat)
          &&. merchantId ==. B.just_ (B.val_ merchantId')
    findRow
      $ B.select
      $ B.limit_ 1
      $ B.filter_ predicate
      $ B.all_ (DB.feature DB.eulerDBSchema)
  case (traverse transformFeature feature) of
    Success f -> pure f
    Failure e -> do
      logErrorT "Incorrect Feature in DB"
        $  "merchantId: " <> merchantId' <> ", error: " <> show e
      throwException internalError