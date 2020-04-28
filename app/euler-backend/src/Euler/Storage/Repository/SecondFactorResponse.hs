module Euler.Storage.Repository.SecondFactorResponse where

import           EulerHS.Prelude hiding (id)

import           EulerHS.Extra.Validation
import           EulerHS.Language
import           WebService.Language

import           Euler.Common.Errors.PredefinedErrors

import qualified Euler.Product.Domain as D

import           Euler.Storage.Repository.EulerDB
import qualified Euler.Storage.Types as DB
import           Euler.Storage.Validators.SecondFactorResponse

import           Database.Beam ((==.))
import qualified Database.Beam as B


findSecondFactorResponse :: Int -> Flow (Maybe D.SecondFactorResponse)
findSecondFactorResponse second_factor_id = do
  sfr <- withEulerDB $ do
    let predicate DB.SecondFactorResponse{secondFactorId} =
          secondFactorId ==. B.just_ (B.val_ second_factor_id)
    findRow
      $ B.select
      $ B.limit_ 1
      $ B.filter_ predicate
      $ B.all_ (DB.second_factor_response DB.eulerDBSchema)

  case (traverse transformSecondFactorResponse sfr) of
    Success sfr' -> pure sfr'
    Failure e -> do
      logErrorT "Incorrect secondFactorResponse in DB"
        $  "txnDetailId: " <> show second_factor_id <> "error: " <> show e
      throwException internalError