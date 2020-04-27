{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE RecordWildCards #-}

module Euler.API.MerchantPaymentGatewayResponse where

import           EulerHS.Prelude as P

import           Data.Generics.Product.Fields

import qualified Euler.Product.Domain as D



data MerchantPaymentGatewayResponse = MerchantPaymentGatewayResponse
  { resp_code            :: Maybe Text
  , rrn                  :: Maybe Text
  , created              :: Maybe Text
  , epg_txn_id           :: Maybe Text
  , resp_message         :: Maybe Text
  , auth_id_code         :: Maybe Text
  , txn_id               :: Maybe Text
  , offer                :: Maybe Text
  , offer_type           :: Maybe Text
  , offer_availed        :: Maybe Text -- Foreign
  , discount_amount      :: Maybe Text -- Foreign
  , offer_failure_reason :: Maybe Text
  , gateway_response     :: Maybe Text -- Foreign
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

-- from src/Types/Communication/OLTP/OrderStatus.purs
-- former defaultPaymentGatewayResponse
-- defaultMerchantPaymentGatewayResponse' :: MerchantPaymentGatewayResponse'
-- defaultMerchantPaymentGatewayResponse' = MerchantPaymentGatewayResponse'
--   { resp_code = Nothing
--   , rrn = Nothing
--   , created = Nothing
--   , epg_txn_id = Nothing
--   , resp_message = Nothing
--   , auth_id_code = Nothing
--   , txn_id = Nothing
--   , offer = Nothing
--   , offer_type = Nothing
--   , offer_availed = Nothing
--   , discount_amount = Nothing
--   , offer_failure_reason = Nothing
--   , gateway_response = Nothing
--   }

-- from src/Types/Communication/OLTP/OrderStatus.purs
-- data MerchantPaymentGatewayResponse = MerchantPaymentGatewayResponse
--   { resp_code            :: Maybe Text -- Foreign
--   , rrn                  :: Maybe Text -- Foreign
--   , created              :: Maybe Text -- Foreign
--   , epg_txn_id           :: Maybe Text -- Foreign
--   , resp_message         :: Maybe Text -- Foreign
--   , auth_id_code         :: Maybe Text -- Foreign
--   , txn_id               :: Maybe Text -- Foreign
--   , offer                :: Maybe Text
--   , offer_type           :: Maybe Text
--   , offer_availed        :: Maybe Text -- Foreign
--   , discount_amount      :: Maybe Text -- Foreign
--   , offer_failure_reason :: Maybe Text
--   , gateway_response     :: Maybe Text -- Foreign
--   }  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

makeMerchantPaymentGatewayResponse
  :: D.MerchantPaymentGatewayResponse
  -> MerchantPaymentGatewayResponse
makeMerchantPaymentGatewayResponse pgr = MerchantPaymentGatewayResponse
  { resp_code = Just $ checkNull $ getField @"respCode" pgr
  , rrn = Just $ checkNull $ getField @"respCode" pgr
  , created = Just $ checkNull $ getField @"respCode" pgr
  , epg_txn_id = Just $ checkNull $ getField @"respCode" pgr
  , resp_message = Just $ checkNull $ getField @"respCode" pgr
  , auth_id_code = Just $ checkNull $ getField @"respCode" pgr
  , txn_id = Just $ checkNull $ getField @"respCode" pgr
  , offer = getField @"offer" pgr
  , offer_type = getField @"offerType" pgr
  , offer_availed = getField @"offerAvailed" pgr
  , discount_amount = getField @"discountAmount" pgr
  , offer_failure_reason = getField @"offerFailureReason" pgr
  , gateway_response = getField @"offerFailureReason" pgr
  }
    -- EHS: TODO move to common utils or sth to that effect
  where
    checkNull :: Maybe Text -> Text
    checkNull Nothing = mempty
    checkNull (Just resp)
      | resp == "null" = mempty
      | otherwise      = resp

