{-# LANGUAGE AllowAmbiguousTypes #-}

module Euler.Storage.Validators.Order where

import EulerHS.Prelude
import EulerHS.Extra.Validation

import GHC.Records (getField)

import           Euler.Common.Types.Money     (mkMoney)
import qualified Euler.Common.Validators as Vs
import qualified Euler.Common.Types.Order as CO
import qualified Euler.Common.Types.External.Mandate as Mandate
import qualified Euler.Storage.Types.OrderReference  as S
import qualified Euler.Product.Domain.Order as DO



-- EHS: better naming
transSOrderToDOrder :: S.OrderReference -> V DO.Order
transSOrderToDOrder so = DO.Order
    <$> withField @"id" so (extractJust >=> Vs.notNegative)
    <*> withField @"version" so pure
    <*> ( mkMoney <$> withField @"amount" so (extractJust >=> Vs.amountValidators))
    <*> withField @"currency" so  extractJust
    <*> withField @"merchantId" so extractJust
    <*> withField @"orderId" so extractJust
    <*> withField @"orderUuid" so extractJust
    <*> runParser parseOrderType "orderType mandateFeature"
    <*> (CO.fromOrderStatusEx <$> withField @"status" so  pure)
    <*> withField @"customerId" so pure
    <*> withField @"customerEmail" so pure
    <*> withField @"customerPhone" so pure
    -- -- , browser           :: Maybe Text       EHS: ?
    -- -- , browserVersion    :: Maybe Text       EHS: ?
    -- -- , popupLoaded       :: Maybe Bool       EHS: ?
    -- -- , popupLoadedTime   :: Maybe LocalTime  EHS: ?
    -- -- , preferredGateway  :: Maybe Text  EHS: ?
    <*> withField @"billingAddressId" so pure
    <*> withField @"shippingAddressId" so pure
    <*> (Vs.cleanUpUDF <$> orderReferenceToUDF so)
    <*> withField @"description" so pure
    <*> withField @"returnUrl" so pure
    -- NULL value is treated as 0.0
    <*> (mkMoney <$> withField @"amountRefunded" so (extractMaybeWithDefault 0.0 >=> Vs.notNegativeAmount))
    <*> withField @"refundedEntirely" so (extractMaybeWithDefault False)
    <*> withField @"autoRefund" so (extractMaybeWithDefault False)
    <*> withField @"productId" so pure
    -- , gatewayMetadata   :: GatewayMetadata    -- EHS: Not a domain fields, should not be here.
    <*> withField @"mandateFeature" so (extractMaybeWithDefault Mandate.DISABLED)
    <*> withField @"lastSynced" so pure
    <*> withField @"dateCreated" so pure
    <*> withField @"lastModified" so pure
  where
    parseOrderType = do
      let mbOrderType = getField @"orderType" so
      case mbOrderType of
        Just orderType -> pure orderType
        Nothing        -> do
          mandate <- extractMaybeWithDefault Mandate.DISABLED $ getField @"mandateFeature" so
          pure $ CO.orderTypeMF mandate


orderReferenceToUDF :: S.OrderReference -> V CO.UDF
orderReferenceToUDF oRef = CO.UDF
  <$> withField @"udf1" oRef pure
  <*> withField @"udf2" oRef pure
  <*> withField @"udf3" oRef pure
  <*> withField @"udf4" oRef pure
  <*> withField @"udf5" oRef pure
  <*> withField @"udf6" oRef pure
  <*> withField @"udf7" oRef pure
  <*> withField @"udf8" oRef pure
  <*> withField @"udf9" oRef pure
  <*> withField @"udf10" oRef pure

