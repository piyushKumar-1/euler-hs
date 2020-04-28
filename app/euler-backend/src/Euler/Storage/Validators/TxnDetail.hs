{-# LANGUAGE AllowAmbiguousTypes #-}

module Euler.Storage.Validators.TxnDetail where

import EulerHS.Prelude
import EulerHS.Extra.Validation
import qualified Euler.Common.Types as C
import           Euler.Common.Validators

import qualified Euler.Product.Domain.TxnDetail as D
import qualified Euler.Storage.Types.TxnDetail as S

transformTxnDetail :: S.TxnDetail -> V D.TxnDetail
transformTxnDetail r = D.TxnDetail
  <$> (D.TxnDetailPId<$> withField @"id" r (extractJust >=> notNegative))
  <*> withField @"version" r pure
  <*> withField @"errorMessage" r pure
  <*> withField @"orderId" r textNotEmpty
  <*> withField @"status" r pure
  <*> withField @"txnId" r textNotEmpty
  <*> withField @"txdType" r textNotEmpty
  <*> withField @"dateCreated" r pure
  <*> withField @"lastModified" r pure
  <*> withField @"successResponseId" r pure
  <*> withField @"txnMode" r pure
  <*> withField @"addToLocker" r pure
  <*> withField @"merchantId" r pure
  <*> withField @"bankErrorCode" r pure
  <*> withField @"bankErrorMessage" r pure
  <*> withField @"gateway" r (insideJust $ decode >=> isGateway)
  <*> withField @"expressCheckout" r pure
  <*> withField @"redirect" r pure
  <*> withField @"gatewayPayload" r pure
  <*> withField @"isEmi" r pure
  <*> withField @"emiBank" r pure
  <*> withField @"emiTenure" r pure
  <*> withField @"username" r pure
  <*> withField @"txnUuid" r pure
  <*> withField @"merchantGatewayAccountId" r pure
  <*> (fmap C.mkMoney <$> withField @"txnAmount" r (insideJust amountValidators))
  <*> withField @"txnObjectType" r pure
  <*> withField @"sourceObject" r pure
  <*> withField @"sourceObjectId" r pure
  <*> withField @"currency" r pure
  <*> (fmap C.mkMoney <$> withField @"netAmount" r (insideJust amountValidators))
  <*> (fmap C.mkMoney <$> withField @"surchargeAmount" r (insideJust amountValidators))
  <*> (fmap C.mkMoney <$> withField @"taxAmount" r (insideJust amountValidators))