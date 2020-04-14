module Euler.Product.OLTP.Order.OrderStatusVersioningService
  ( mkHandle
  , doVersionTransformation
  ) where

import EulerHS.Prelude hiding (id)
import EulerHS.Prelude as P
import EulerHS.Language

import           Data.Generics.Product.Fields
import qualified Data.Text as T

-- EHS: rework imports. Use top level modules.
import qualified Euler.API.Order                   as API
import           Euler.API.MerchantPaymentGatewayResponse (MerchantPaymentGatewayResponse(..))
import           Euler.API.Refund (Refund'(..))

import qualified Euler.Common.Types                as C

import           Euler.Product.OLTP.Services.TokenService



-- |
data SHandle = SHandle
  { transformRefunds            :: PureTransformer
  , transformChargebacks        :: PureTransformer
  , transformTxnDetail          :: PureTransformer
  , transformGatewayResponse    :: C.GatewayId -> PureTransformer
  , transformGatewayReferenceId :: PureTransformer
  , transformAuthToken          :: Maybe Text -> Maybe Text -> ImpureTransformer
  }

-- |
doVersionTransformation :: SHandle -> API.OrderStatusResponse -> Flow API.OrderStatusResponse
doVersionTransformation SHandle {..} input = do
    let pureRes = transPure input
    transformAuthToken orderId merchantId pureRes
  where
    orderId = getField @"order_id" input
    merchantId = getField @"merchant_id" input
    transPure
      = transformRefunds
      . transformChargebacks
      . transformTxnDetail
      . transformGatewayReferenceId

-- ----------------------------------------------------------------------------

-- |
mkHandle :: Maybe Version -> TokenNeeded -> SHandle
mkHandle mbVer isTokenNeeded = SHandle
    { transformRefunds            = transformRefunds' mbVer
    , transformChargebacks        = clearChargebacks mbVer
    , transformTxnDetail          = clearTxnDetail mbVer
    , transformGatewayResponse    = transformGatewayResponse' mbVer
    , transformGatewayReferenceId = clearGatewayReferenceId mbVer
    , transformAuthToken          = addAuthToken mbVer isTokenNeeded
    }

-- ----------------------------------------------------------------------------

-- EHS: common type/enumeration?
type Version = Text

type TokenNeeded = Bool

type PureTransformer = API.OrderStatusResponse -> API.OrderStatusResponse

type ImpureTransformer = API.OrderStatusResponse -> Flow API.OrderStatusResponse

-- ----------------------------------------------------------------------------

transformRefunds' :: Maybe Version -> PureTransformer
transformRefunds' (Just ver) input =
  let refunds = fromMaybe [] $ getField @"refunds" input
      newRefunds = transformRefund ver <$> filterRefunds ver refunds
  in setField @"refunds" (Just newRefunds) input
transformRefunds' _ input = input

filterRefunds :: Version -> [Refund'] -> [Refund']
filterRefunds version
  | version < "2015-08-18"  || version == "" =
    filter (\refund -> getField @"status" refund /= C.FAILURE)
  | otherwise = P.id

transformRefund :: Version -> Refund' -> Refund'
transformRefund ver
  = setInitiatedBy ver
  . setType ver
  . setSource ver
  . setStatus ver

setInitiatedBy :: Version -> Refund' -> Refund'
setInitiatedBy version
  | version < "2018-09-20" || version == "" = setField @"initiated_by" ""
  | otherwise = P.id

setType :: Version -> Refund' -> Refund'
setType version
  | version < "2019-03-12" || version == "" = setField @"refund_type" ""
  | otherwise = P.id

setSource :: Version -> Refund' -> Refund'
setSource version
  | version < "2019-03-12" || version == "" = setField @"refund_source" ""
  | otherwise = P.id

setStatus :: Version -> Refund' -> Refund'
setStatus version refund
  | version < "2015-01-09" || version == "" = setField @"status" C.SUCCESS refund
  | version  < "2017-03-16" && status == C.MANUAL_REVIEW =
      setField @"status" C.PENDING refund
  | otherwise = refund
  where
    status = getField @"status" refund

-- ----------------------------------------------------------------------------

clearChargebacks :: Maybe Version -> PureTransformer
clearChargebacks (Just ver)
  -- EHS: could version be really empty?
  -- EHS: or does it correspond to Nothing?
  | (ver < "2017-07-26"  || ver == "") = setField @"chargebacks" Nothing
  | otherwise = P.id
clearChargebacks _ = P.id

-- ----------------------------------------------------------------------------

clearTxnDetail :: Maybe Version -> PureTransformer
clearTxnDetail (Just ver)
  -- EHS: could version be really empty?
  -- EHS: or does it correspond to Nothing?
  | ver < "2018-07-16"  || ver == "" = setField @"txn_detail" Nothing
  | otherwise = P.id
clearTxnDetail _ = P.id

-- ----------------------------------------------------------------------------

transformGatewayResponse' :: Maybe Version -> C.GatewayId -> PureTransformer
transformGatewayResponse' (Just v) gwId input =
  setField @"payment_gateway_response" mpgr input
      where
        mpgr = transformMPGR <$> getField @"payment_gateway_response" input
        transformMPGR =
          setOffer' v
          . setOfferAvailed' v
          . setOfferType' v
          . setOfferFailureReason' v
          . setDiscountAmount' v
          . setAuthIdCodeAndRRN' v gwId
transformGatewayResponse' _ _ input = input

setOffer' :: Version -> MerchantPaymentGatewayResponse -> MerchantPaymentGatewayResponse
setOffer' version
  | version < "2017-05-25" || version == "" = setField @"offer" Nothing
  | otherwise = P.id

setOfferAvailed' :: Version -> MerchantPaymentGatewayResponse -> MerchantPaymentGatewayResponse
setOfferAvailed' version
  | version < "2017-05-25" || version == "" = setField @"offer_availed" Nothing
  | otherwise = P.id

setOfferType' :: Version -> MerchantPaymentGatewayResponse -> MerchantPaymentGatewayResponse
setOfferType' version
  | version < "2017-05-25" || version == "" = setField @"offer_type" Nothing
  | otherwise = P.id

setOfferFailureReason' :: Version -> MerchantPaymentGatewayResponse -> MerchantPaymentGatewayResponse
setOfferFailureReason' version
  | version < "2017-05-25" || version == "" = setField @"offer_failure_reason" Nothing
  | otherwise = P.id

setDiscountAmount' :: Version -> MerchantPaymentGatewayResponse -> MerchantPaymentGatewayResponse
setDiscountAmount' version
  | version < "2017-05-25" || version == "" = setField @"discount_amount" Nothing
  | otherwise = P.id

setAuthIdCodeAndRRN' :: Version -> C.GatewayId -> MerchantPaymentGatewayResponse -> MerchantPaymentGatewayResponse
setAuthIdCodeAndRRN' version gwId mpgr
  | (version < "2017-10-26"  || version == "") && Just gwId == C.gatewayIdFromGateway C.PAYU =
      setField @"auth_id_code" newAuthIdCode
      $ setField @"rrn" newRRN mpgr
  | otherwise = mpgr
  where
    newAuthIdCode = getField @"rrn" mpgr
    newRRN = getField @"epg_txn_id" mpgr


-- ----------------------------------------------------------------------------

clearGatewayReferenceId :: Maybe Version -> PureTransformer
clearGatewayReferenceId (Just ver)
  -- EHS: could version be really empty?
  -- EHS: or does it correspond to Nothing?
  | (ver < "2018-10-25" || ver == "") = setField @"gateway_reference_id" Nothing
  | otherwise = P.id
clearGatewayReferenceId _ = P.id

-- ----------------------------------------------------------------------------

-- EHS: clarify types
addAuthToken
  :: Maybe Version
  -> TokenNeeded
  -> (Maybe Text)      -- ^ orderId
  -> (Maybe Text)      -- ^ merchantId
  -> ImpureTransformer
addAuthToken (Just ver) True (Just orderId) (Just merchantId) input
  -- EHS: really I don't like this style, too convoluted for such an easy action
  | ver >= "2018-07-01"      = flip (setField @"juspay") input
                                 <$> Just <$> (acquireOrderToken (read $ T.unpack orderId :: Int) merchantId)
  | otherwise                = pure input
addAuthToken _ _ _ _ input  = pure input
