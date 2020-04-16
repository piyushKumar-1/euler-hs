module Euler.Storage.Repository.SecondFactor where

import           EulerHS.Prelude hiding (id)

import           EulerHS.Extra.Validation
import           EulerHS.Language
import           WebService.Language

import           Euler.Common.Errors.PredefinedErrors
import           Euler.Common.Validators (textNotEmpty, notNegative)
import qualified Euler.Product.Domain as D
import           Euler.Storage.Repository.EulerDB
import qualified Euler.Storage.Types as DB

import           Database.Beam ((==.))
import qualified Database.Beam as B


findSecondFactor :: Int -> Flow (Maybe D.SecondFactor)
findSecondFactor txnDetail_id = do
  sf <- withEulerDB $ do
    let predicate DB.SecondFactor{txnDetailId} =
          txnDetailId ==. B.just_ (B.val_ txnDetail_id)
    findRow
      $ B.select
      $ B.limit_ 1
      $ B.filter_ predicate
      $ B.all_ (DB.second_factor DB.eulerDBSchema)

  case (traverse transformSecondFactor sf) of
    Success sf' -> pure sf'
    Failure e -> do
      logErrorT "Incorrect secondFactor in DB"
        $  "txnDetailId: " <> show txnDetail_id
        <> "error: " <> show e
      throwException internalError

transformSecondFactor :: DB.SecondFactor -> V D.SecondFactor
transformSecondFactor sf = D.SecondFactor
  <$> (D.SecondFactorPId <$> withField @"id" sf (extractJust >=> notNegative))
  <*> withField @"version" sf pure
  <*> withField @"otp" sf pure
  <*> withField @"status" sf textNotEmpty
  <*> withField @"txnId" sf textNotEmpty
  <*> withField @"sfType" sf textNotEmpty
  <*> withField @"url" sf pure
  <*> withField @"secondFactorResponse" sf pure
  <*> withField @"dateCreated" sf pure
  <*> withField @"epgTxnId" sf pure
  <*> withField @"lastUpdated" sf pure
  <*> withField @"txnDetailId" sf pure
  <*> withField @"gatewayAuthReqParams" sf pure
  <*> withField @"authenticationAccountId" sf pure
  <*> withField @"canAcceptResponse" sf pure
  <*> withField @"challengesAttempted" sf pure
  <*> withField @"responseAttempted" sf pure
