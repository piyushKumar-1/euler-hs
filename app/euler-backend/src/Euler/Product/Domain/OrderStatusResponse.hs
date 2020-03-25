{-# LANGUAGE DeriveAnyClass #-}

module Euler.Product.Domain.OrderStatusResponse where

import           EulerHS.Prelude

import qualified Euler.Common.Types as C
import           Euler.Common.Types.External.Order
import           Euler.Common.Types.TxnDetail (TxnStatus)
import qualified Euler.Product.Domain as D

import           Data.Time (LocalTime)


data OrderStatusResponse = OrderStatusResponse
  {  id                       :: Text
  ,  merchant_id              :: Maybe Text
  ,  amount                   :: Maybe C.Money
  ,  currency                 :: Maybe Text
  ,  order_id                 :: Maybe Text
  ,  date_created             :: Text
  ,  return_url               :: Maybe Text
  ,  product_id               :: Text
  ,  customer_email           :: Maybe Text
  ,  customer_phone           :: Maybe Text
  ,  customer_id              :: Maybe Text
  ,  payment_links            :: D.Paymentlinks
  ,  udf1                     :: Text
  ,  udf2                     :: Text
  ,  udf3                     :: Text
  ,  udf4                     :: Text
  ,  udf5                     :: Text
  ,  udf6                     :: Text
  ,  udf7                     :: Text
  ,  udf8                     :: Text
  ,  udf9                     :: Text
  ,  udf10                    :: Text
  ,  txn_id                   :: Maybe Text
  ,  status_id                :: Int
  ,  status                   :: OrderTxnStatus
  ,  payment_method_type      :: Maybe Text
  ,  auth_type                :: Maybe Text
  ,  card                     :: Maybe D.Card
  ,  payment_method           :: Maybe Text
  ,  refunded                 :: Maybe Bool
  ,  amount_refunded          :: Maybe C.Money
  ,  chargebacks              :: Maybe [D.Chargeback]
  ,  refunds                  :: Maybe [D.Refund]
  ,  mandate                  :: Maybe D.Mandate
  ,  promotion                :: Maybe D.PromotionActive
  ,  risk                     :: Maybe D.Risk
  ,  bank_error_code          :: Maybe Text
  ,  bank_error_message       :: Maybe Text
  ,  txn_uuid                 :: Maybe Text
  ,  gateway_payload          :: Maybe Text
  ,  txn_detail               :: Maybe D.TxnDetail
  -- ,  payment_gateway_response' :: Maybe MerchantPaymentGatewayResponse'
  ,  payment_gateway_response :: Maybe D.MerchantPaymentGatewayResponse
  ,  gateway_id               :: Maybe Int
  ,  emi_bank                 :: Maybe Text
  ,  emi_tenure               :: Maybe Int
  ,  gateway_reference_id     :: Maybe Text
  ,  payer_vpa                :: Maybe Text
  ,  payer_app_name           :: Maybe Text
  ,  juspay                   :: Maybe D.OrderTokenResp
  ,  second_factor_response   :: Maybe D.SecondFactorResponse
  ,  txn_flow_info            :: Maybe D.TxnFlowInfo
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)


data OrderTxnStatus = OStatus OrderStatus | TStatus TxnStatus | DEFAULT
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)
