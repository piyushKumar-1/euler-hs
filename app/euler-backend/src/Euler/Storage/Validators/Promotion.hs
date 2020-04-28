{-# LANGUAGE AllowAmbiguousTypes #-}

module Euler.Storage.Validators.Promotion where

import EulerHS.Prelude
import EulerHS.Extra.Validation

import           Euler.Common.Validators
import qualified Euler.Common.Types as C
import qualified Euler.Product.Domain.Promotion as D
import qualified Euler.Storage.Types.Promotion as S

transformPromotions :: S.Promotion -> V D.Promotion
transformPromotions r = D.Promotion
  <$> (D.PromotionPId <$> withField @"id" r notNegative)
  <*> withField @"dateCreated" r pure
  <*> (C.mkMoney <$> withField @"discountAmount" r amountValidators)
  <*> withField @"lastModified" r pure
  <*> withField @"orderId" r (insideJust notNegative)
  <*> withField @"rules" r textNotEmpty
  <*> withField @"status" r textNotEmpty
  <*> withField @"orderReferenceId" r (insideJust notNegative)