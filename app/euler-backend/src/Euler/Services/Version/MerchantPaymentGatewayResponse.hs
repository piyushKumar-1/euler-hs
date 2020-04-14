-- EHS: remove
module Euler.Services.Version.MerchantPaymentGatewayResponse
  ( MerchantPGRService(..)
  , mkMerchantPGRService
  )
  where

import EulerHS.Prelude

import           Data.Generics.Product.Fields

import Euler.API.MerchantPaymentGatewayResponse (MerchantPaymentGatewayResponse(..))
import Euler.Common.Types.Gateway

-- move to common types?
-- change to newtype?
type Version = Text

transformMPGR' :: Version -> GatewayId -> MerchantPaymentGatewayResponse -> MerchantPaymentGatewayResponse
transformMPGR' v gwId
  = setOffer' v
  . setOfferAvailed' v
  . setOfferType' v
  . setOfferFailureReason' v
  . setDiscountAmount' v
  . setAuthIdCodeAndRRN' v gwId



data MerchantPGRService = MerchantPGRService
  { transformMPGR :: MerchantPaymentGatewayResponse -> MerchantPaymentGatewayResponse
  }

mkMerchantPGRService :: Version -> GatewayId -> MerchantPGRService
mkMerchantPGRService version gwId = MerchantPGRService
  { transformMPGR = transformMPGR' version gwId
  }


setOffer' :: Version -> MerchantPaymentGatewayResponse -> MerchantPaymentGatewayResponse
setOffer' version
  | version < "2017-05-25" || version == "" = setField @"offer" Nothing
  | otherwise = id

setOfferAvailed' :: Version -> MerchantPaymentGatewayResponse -> MerchantPaymentGatewayResponse
setOfferAvailed' version
  | version < "2017-05-25" || version == "" = setField @"offer_availed" Nothing
  | otherwise = id

setOfferType' :: Version -> MerchantPaymentGatewayResponse -> MerchantPaymentGatewayResponse
setOfferType' version
  | version < "2017-05-25" || version == "" = setField @"offer_type" Nothing
  | otherwise = id

setOfferFailureReason' :: Version -> MerchantPaymentGatewayResponse -> MerchantPaymentGatewayResponse
setOfferFailureReason' version
  | version < "2017-05-25" || version == "" = setField @"offer_failure_reason" Nothing
  | otherwise = id

setDiscountAmount' :: Version -> MerchantPaymentGatewayResponse -> MerchantPaymentGatewayResponse
setDiscountAmount' version
  | version < "2017-05-25" || version == "" = setField @"discount_amount" Nothing
  | otherwise = id

setAuthIdCodeAndRRN' :: Version -> GatewayId -> MerchantPaymentGatewayResponse -> MerchantPaymentGatewayResponse
setAuthIdCodeAndRRN' version gwId mpgr
  | (version < "2017-10-26"  || version == "") && Just gwId == gatewayIdFromGateway PAYU =
      setField @"auth_id_code" newAuthIdCode
      $ setField @"rrn" newRRN mpgr
  | otherwise = mpgr
  where
    newAuthIdCode = getField @"rrn" mpgr
    newRRN = getField @"epg_txn_id" mpgr
