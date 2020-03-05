module Euler.Storage.Validators.ResellerAccount
  ( toDomResAcc
  )
  where

import EulerHS.Prelude
import EulerHS.Extra.Validation

import qualified Euler.Product.Domain.ResellerAccount as DR
import qualified Euler.Storage.Types.ResellerAccount  as SR

toDomResAcc :: SR.ResellerAccount -> V DR.ResellerAccount
toDomResAcc ra = DR.ResellerAccount
  <$> withField @"resellerId" ra pure
  <*> withField @"userId" ra extractJust
  <*> withField @"resellerName" ra extractJust
  <*> withField @"resellerApiEndpoint" ra pure