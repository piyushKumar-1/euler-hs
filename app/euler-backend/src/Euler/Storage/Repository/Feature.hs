module Euler.Storage.Repository.Feature where

import           EulerHS.Prelude hiding (id)

import           EulerHS.Extra.Validation
import           EulerHS.Language

import qualified Euler.Constant.Feature               as Const
import           Euler.Common.Errors.PredefinedErrors
import           Euler.Common.Validators (notNegative)
import qualified Euler.Common.Types as C

import qualified Euler.Product.Domain.Feature as D


import           Euler.Storage.Repository.EulerDB
import qualified Euler.Storage.Types as DB

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
      logError "Incorrect Feature in DB"
        $  "merchantId: " <> merchantId' <> ", error: " <> show e
      throwException internalError

transformFeature :: DB.Feature -> V D.Feature
transformFeature r = D.Feature
  <$> (D.FeaturePId <$> withField @"id" r (extractJust >=> notNegative))
  <*> withField @"version" r pure
  <*> withField @"enabled" r pure
  <*> withField @"name" r pure
  <*> withField @"merchantId" r pure
  <*> withField @"disabledUntil" r pure

