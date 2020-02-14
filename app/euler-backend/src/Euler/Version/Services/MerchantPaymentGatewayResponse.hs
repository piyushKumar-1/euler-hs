module Euler.Version.Services.MerchantPaymentGatewayResponse
  ( MerchantPGRService
  , mkMerchantPGRService
  , transformMPGR
  )
  where

import EulerHS.Prelude

import           Data.Generics.Product.Fields

import Euler.API.Order (MerchantPaymentGatewayResponse(..))
import Euler.Common.Types.Gateway

-- move to common types?
-- change to newtype?
type Version = Text

transformMPGR :: MerchantPGRService -> MerchantPaymentGatewayResponse -> MerchantPaymentGatewayResponse
transformMPGR MerchantPGRService{..}
  = setOffer
  . setOfferAvailed
  . setOfferType
  . setOfferFailureReason
  . setDiscountAmount
  . setAuthIdCodeAndRRN



data MerchantPGRService = MerchantPGRService
  { setOffer :: MerchantPaymentGatewayResponse -> MerchantPaymentGatewayResponse
  , setOfferAvailed :: MerchantPaymentGatewayResponse -> MerchantPaymentGatewayResponse
  , setOfferType :: MerchantPaymentGatewayResponse -> MerchantPaymentGatewayResponse
  , setOfferFailureReason :: MerchantPaymentGatewayResponse -> MerchantPaymentGatewayResponse
  , setDiscountAmount :: MerchantPaymentGatewayResponse -> MerchantPaymentGatewayResponse
  , setAuthIdCodeAndRRN :: MerchantPaymentGatewayResponse -> MerchantPaymentGatewayResponse
  }

mkMerchantPGRService :: Version -> GatewayId -> MerchantPGRService
mkMerchantPGRService version gwId = MerchantPGRService
  { setOffer = setOffer' version
  , setOfferAvailed = setOfferAvailed' version
  , setOfferType = setOfferType' version
  , setOfferFailureReason = setOfferFailureReason' version
  , setDiscountAmount = setDiscountAmount' version
  , setAuthIdCodeAndRRN = setAuthIdCodeAndRRN' version gwId
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
setAuthIdCodeAndRRN' = undefined
-- setAuthIdCodeAndRRN' version gwId mpgr
--   | (version < "2017-10-26"  || version == "")
--     && gwId == gatewayIdFromGateway PAYU =
--       setField @"auth_id_code" newAuthIdCode
--       $ setField @"rrn" newRRN mpgr
--   | otherwise = mpgr
--   where
--     newAuthIdCode = getField @"rrn" mpgr
--     newRRN = getField @"epg_txn_id" mpgr
