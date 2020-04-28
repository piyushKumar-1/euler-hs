{-# LANGUAGE AllowAmbiguousTypes #-}

module Euler.Storage.Validators.Customer where

import EulerHS.Prelude
import EulerHS.Extra.Validation

import           Euler.Common.Validators

import qualified Euler.Product.Domain.Customer as D
import qualified Euler.Storage.Types.Customer as S


validator :: S.Customer -> V D.Customer
validator v = D.Customer
  <$> withField @"id" v (extractJust >=> textNotEmpty)
  <*> withField @"version" v pure
  <*> withField @"dateCreated" v pure
  <*> withField @"emailAddress" v (extractJust >=> textNotEmpty)
  <*> withField @"firstName" v (extractJust >=> textNotEmpty)
  <*> withField @"lastName" v (extractJust >=> textNotEmpty)
  <*> withField @"lastUpdated" v pure
  <*> withField @"merchantAccountId" v pure
  <*> withField @"mobileCountryCode" v pure
  <*> withField @"mobileNumber" v pure
  <*> withField @"objectReferenceId" v (extractJust >=> textNotEmpty)