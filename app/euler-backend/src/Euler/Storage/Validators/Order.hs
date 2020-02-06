{-# LANGUAGE AllowAmbiguousTypes #-}

module Euler.Storage.Validators.Order where

import EulerHS.Prelude
import EulerHS.Extra.Validation

import qualified Euler.Storage.Types.OrderReference  as S
import qualified Euler.Product.Domain.Order as D



transSOrderToDOrder :: S.OrderReference -> V D.Order
transSOrderToDOrder so = DO.Order
  <$> withField @"id" so (fromJust >=> notNegative)
  <*> withField @"version" so pure
  <*> ( fmap mkMoney $ withField @"amount" req (fromJust >=> amountValidators))
  <*> withField @"currency" so  fromJust
  <*> withField @"merchantId" so fromJust
  <*> withField @"orderId" so fromJust
  <*> withField @"orderUuid" so fromJust
  <*> withField @"orderType" so fromJust
  <*> withField @"status" so  fromJust
  <*> withField @"customerId" so pure
  <*> withField @"customerEmail" so pure
  <*> withField @"customerPhone" so pure
  -- , udf               :: UDF
  -- -- , browser           :: Maybe Text       EHS: ?
  -- -- , browserVersion    :: Maybe Text       EHS: ?
  -- -- , popupLoaded       :: Maybe Bool       EHS: ?
  -- -- , popupLoadedTime   :: Maybe LocalTime  EHS: ?
  <*> withField @"description" so pure
  -- , returnUrl         :: Maybe Text
  -- , amountRefunded    :: Maybe Double
  -- , refundedEntirely  :: Maybe Bool
  -- -- , preferredGateway  :: Maybe Text  EHS: ?
  -- , productId         :: Maybe Text
  <*> withField @"billingAddressId" so pure
  <*> withField @"shippingAddressId" so pure
  -- , mandateFeature    :: MandateFeature
  -- , autoRefund        :: Bool
  -- , lastSynced        :: LocalTime
  -- , dateCreated       :: LocalTime          -- EHS: Not a domain fields
  -- , lastModified      :: LocalTime          -- EHS: Not a domain fields
  --
  -- , gatewayMetadata   :: GatewayMetadata    -- EHS: Not a domain fields, should not be here.
  }


-- EHS: move validators to separate module
notNegative :: Validator Int
notNegative = mkValidator "Should not be negative." (>= 0)

amountValidators :: Validator Double
amountValidators =
  parValidate
    [ max2DecimalDigits
    , gteOne
    ]

--Will accept double values with upto two decimal places.
max2DecimalDigits :: Validator Double
max2DecimalDigits = mkValidator
  "Will accept double values with upto two decimal places."
  ((<=3) . length . dropWhile (/='.') . show)

gteOne :: Validator Double
gteOne = mkValidator
  "Should be greater than or equal 1"
  (>=1)