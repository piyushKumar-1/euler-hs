{-# LANGUAGE AllowAmbiguousTypes #-}

module Euler.Storage.Validators.Order where

import EulerHS.Prelude
import EulerHS.Extra.Validation

import           Euler.Common.Types.Money     (mkMoney)
import qualified Euler.Common.Validators as Vs
import qualified Euler.Common.Types.External.Mandate as MEx
import qualified Euler.Common.Types.External.Order as OEx
import qualified Euler.Storage.Types.OrderReference  as S
import qualified Euler.Product.Domain.Order as DO

-- EHS: better naming
-- EHS: implement
transSOrderToDOrder :: S.OrderReference -> V DO.Order
transSOrderToDOrder so = error "Not implemented"
-- transSOrderToDOrder so = DO.Order
--   <$> withField @"id" so (extractJust >=> Vs.notNegative)
--   <*> withField @"version" so pure
--   <*> ( mkMoney <$> withField @"amount" so (extractJust >=> Vs.amountValidators))
--   <*> withField @"currency" so  extractJust
--   <*> withField @"merchantId" so extractJust
--   <*> withField @"orderId" so extractJust
--   <*> withField @"orderUuid" so extractJust
--   <*> withField @"orderType" so extractJust
--   <*> withField @"status" so  extractJust
--   <*> withField @"customerId" so pure
--   <*> withField @"customerEmail" so pure
--   <*> withField @"customerPhone" so pure
--   -- , udf               :: UDF
--   -- -- , browser           :: Maybe Text       EHS: ?
--   -- -- , browserVersion    :: Maybe Text       EHS: ?
--   -- -- , popupLoaded       :: Maybe Bool       EHS: ?
--   -- -- , popupLoadedTime   :: Maybe LocalTime  EHS: ?
--   -- , returnUrl         :: Maybe Text
--   -- , amountRefunded    :: Maybe Double
--   -- , refundedEntirely  :: Maybe Bool
--   -- -- , preferredGateway  :: Maybe Text  EHS: ?
--   -- , productId         :: Maybe Text
--   <*> withField @"billingAddressId" so pure
--   <*> withField @"shippingAddressId" so pure
--   -- , autoRefund        :: Bool
--   -- , lastSynced        :: LocalTime
--   -- , dateCreated       :: LocalTime          -- EHS: Not a domain fields
--   -- , lastModified      :: LocalTime          -- EHS: Not a domain fields
--   <*> withField @"description" so pure
--   --
--   -- , gatewayMetadata   :: GatewayMetadata    -- EHS: Not a domain fields, should not be here.
--
--   <*> withField @"mandateFeature" so (extractJust >=> mkMandate)
--   <*> pure False
--
--   where
--     mkMandate M.DISABLED = O.MandateDisabled
--     mkMandate M.REQUIRED = O.MandateReqUndefined      -- EHS: bug: we don't know what is max amount
--     mkMandate M.OPTIONAL = O.MandateOptUndefined      -- EHS: bug: we don't know what is max amount


-- EHS: move validators to separate module.
-- Don't duplicate validators.
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
