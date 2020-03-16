module Euler.Storage.Repository.SecondFactorResponse where

import           EulerHS.Prelude hiding (id)

import           Euler.Storage.DBConfig
import           EulerHS.Extra.Validation
import           EulerHS.Language

import           Euler.Common.Errors.PredefinedErrors
import           Euler.Common.Validators (textNotEmpty, notNegative)

import qualified Euler.Product.Domain as D

import qualified Euler.Storage.Types as DB

import qualified Data.Text as T
import           Database.Beam ((==.))
import qualified Database.Beam as B


findSecondFactorResponse :: Int -> Flow (Maybe D.SecondFactorResponse)
findSecondFactorResponse second_factor_id = do
  sfr <- withDB eulerDB $ do
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
      logError "Incorrect secondFactorResponse in DB"
        $  "txnDetailId: " <> show second_factor_id <> "error: " <> show e
      throwException internalError

transformSecondFactorResponse :: DB.SecondFactorResponse -> V D.SecondFactorResponse
transformSecondFactorResponse sfr = D.SecondFactorResponse
  <$> (D.SecondFactorResponsePId <$> withField @"id" sfr (extractJust >=> notNegative))
  <*> withField @"version" sfr pure
  <*> withField @"cavv" sfr pure
  <*> withField @"currency" sfr pure
  <*> withField @"eci" sfr textNotEmpty
  <*> withField @"mpiErrorCode" sfr pure
  <*> withField @"purchaseAmount" sfr pure
  <*> withField @"responseId" sfr pure
  <*> withField @"shoppingContext" sfr textNotEmpty
  <*> withField @"status" sfr textNotEmpty
  <*> withField @"xid" sfr textNotEmpty
  <*> withField @"dateCreated" sfr pure
  <*> withField @"secondFactorId" sfr pure
  <*> withField @"gatewayAuthResData" sfr pure
