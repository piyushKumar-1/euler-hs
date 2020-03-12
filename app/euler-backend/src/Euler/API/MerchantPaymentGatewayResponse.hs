{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE RecordWildCards #-}

module Euler.API.MerchantPaymentGatewayResponse where

import           EulerHS.Prelude as P

import           Data.Aeson
import qualified Data.Char as C
import           Data.Generics.Product.Fields

import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL

import           Euler.API.Types
import           Euler.Common.Types.PaymentGatewayResponseXml
import           Euler.Product.Domain as D
import           Euler.Storage.Types.PaymentGatewayResponse
import           Euler.Storage.Types.TxnDetail



data MerchantPaymentGatewayResponse' = MerchantPaymentGatewayResponse'
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
defaultMerchantPaymentGatewayResponse' :: MerchantPaymentGatewayResponse'
defaultMerchantPaymentGatewayResponse' = MerchantPaymentGatewayResponse'
  { resp_code = Nothing
  , rrn = Nothing
  , created = Nothing
  , epg_txn_id = Nothing
  , resp_message = Nothing
  , auth_id_code = Nothing
  , txn_id = Nothing
  , offer = Nothing
  , offer_type = Nothing
  , offer_availed = Nothing
  , discount_amount = Nothing
  , offer_failure_reason = Nothing
  , gateway_response = Nothing
  }

-- from src/Types/Communication/OLTP/OrderStatus.purs
data MerchantPaymentGatewayResponse = MerchantPaymentGatewayResponse
  { resp_code            :: Maybe Text -- Foreign
  , rrn                  :: Maybe Text -- Foreign
  , created              :: Maybe Text -- Foreign
  , epg_txn_id           :: Maybe Text -- Foreign
  , resp_message         :: Maybe Text -- Foreign
  , auth_id_code         :: Maybe Text -- Foreign
  , txn_id               :: Maybe Text -- Foreign
  , offer                :: Maybe Text
  , offer_type           :: Maybe Text
  , offer_availed        :: Maybe Text -- Foreign
  , discount_amount      :: Maybe Text -- Foreign
  , offer_failure_reason :: Maybe Text
  , gateway_response     :: Maybe Text -- Foreign
  }  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

makeMerchantPaymentGatewayResponse
  :: Maybe Text
  -> MerchantPaymentGatewayResponse'
  -> MerchantPaymentGatewayResponse
makeMerchantPaymentGatewayResponse gatewayResponse pgr' = MerchantPaymentGatewayResponse
  { resp_code = Just $ checkNull $ getField @"resp_code" pgr'
  , rrn = Just $ checkNull $ getField @"resp_code" pgr'
  , created = Just $ checkNull $ getField @"resp_code" pgr'
  , epg_txn_id = Just $ checkNull $ getField @"resp_code" pgr'
  , resp_message = Just $ checkNull $ getField @"resp_code" pgr'
  , auth_id_code = Just $ checkNull $ getField @"resp_code" pgr'
  , txn_id = Just $ checkNull $ getField @"resp_code" pgr'
  , offer = getField @"offer" pgr'
  , offer_type = getField @"offer_type" pgr'
  , offer_availed = getField @"offer_availed" pgr'
  , discount_amount = getField @"discount_amount" pgr'
  , offer_failure_reason = getField @"offer_failure_reason" pgr'
  , gateway_response = gatewayResponse
  }
    -- EHS: TODO move to common utils or sth to that effect
  where
    checkNull :: Maybe Text -> Text
    checkNull Nothing = mempty
    checkNull (Just resp)
      | resp == "null" = mempty
      | otherwise      = resp

