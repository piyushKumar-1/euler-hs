{-# LANGUAGE DeriveAnyClass #-}

module Euler.Product.Domain.OrderStatusResponse where

import qualified Prelude as P
import           EulerHS.Prelude hiding (show)

import qualified Euler.Common.Types as C
import           Euler.Common.Types.External.Order
import           Euler.Common.Types.TxnDetail (TxnStatus)

import qualified Euler.Product.Domain.Chargeback as D
import qualified Euler.Product.Domain.Refund as D
import qualified Euler.Product.Domain.Mandate as D
import qualified Euler.Product.Domain.Paymentlinks as D
import qualified Euler.Product.Domain.Card as D
import qualified Euler.Product.Domain.Promotion as D
import qualified Euler.Product.Domain.TxnRiskCheck as D
import qualified Euler.Product.Domain.TxnDetail as D
import qualified Euler.Product.Domain.MerchantPaymentGatewayResponse as D
import qualified Euler.Product.Domain.SecondFactorResponse as D
import qualified Euler.Product.Domain.MerchantPaymentGatewayResponse as D
import qualified Euler.Product.Domain.TxnFlowInfo as D


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
  ,  udf                      :: C.UDF
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
  ,  payment_gateway_response :: Maybe D.MerchantPaymentGatewayResponse
  ,  gateway_id               :: Maybe Int
  ,  emi_bank                 :: Maybe Text
  ,  emi_tenure               :: Maybe Int
  ,  gateway_reference_id     :: Maybe Text
  ,  payer_vpa                :: Maybe Text
  ,  payer_app_name           :: Maybe Text
  ,  second_factor_response   :: Maybe D.SecondFactorResponse
  ,  txn_flow_info            :: Maybe D.TxnFlowInfo
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)


data OrderTxnStatus = OStatus OrderStatus | TStatus TxnStatus | DEFAULT
  deriving (Eq, Ord, Generic, ToJSON, FromJSON)

instance Show OrderTxnStatus where
  show DEFAULT = "DEFAULT"
  show (OStatus os) = P.show os
  show (TStatus ts) = P.show ts


data OrderStatusRequest = OrderStatusRequest
  { orderId                 :: C.OrderId
  , merchantId              :: C.MerchantId
  , resellerId              :: Maybe Text
  , isAuthenticated         :: Bool
  , sendCardIsin            :: Bool
  , sendFullGatewayResponse :: Bool
  , sendAuthToken           :: Bool
  , version                 :: Maybe Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)
