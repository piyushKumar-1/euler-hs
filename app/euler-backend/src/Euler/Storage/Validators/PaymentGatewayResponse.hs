{-# LANGUAGE AllowAmbiguousTypes #-}

module Euler.Storage.Validators.PaymentGatewayResponse where

import EulerHS.Prelude
import EulerHS.Extra.Validation

import           Euler.Common.Validators

import qualified Euler.Product.Domain.PaymentGatewayResponse as D
import qualified Euler.Storage.Types.PaymentGatewayResponse as S

transformPaymentGatewayResponse :: S.PaymentGatewayResponse -> V D.PaymentGatewayResponse
transformPaymentGatewayResponse r = D.PaymentGatewayResponse
  <$> (D.PaymentGatewayResponsePId <$> withField @"id" r (extractJust >=> notNegative))
  <*> withField @"version" r pure
  <*> withField @"bankCode" r pure
  <*> withField @"dateCreated" r pure
  <*> withField @"responseXml" r pure
  <*> withField @"txnId" r pure
  <*> withField @"iciciRespCode" r pure
  <*> withField @"iciciRespMessage" r pure
  <*> withField @"axisRespCode" r pure
  <*> withField @"axisRespMessage" r pure
  <*> withField @"hdfcRespCode" r pure
  <*> withField @"hdfcRespMessage" r pure
  <*> withField @"respCode" r pure
  <*> withField @"respMessage" r pure