module Euler.Product.OLTP.Services.OrderStatusBuilder where



import           EulerHS.Prelude hiding (First, Last, getFirst, getLast)

import           Euler.API.MerchantPaymentGatewayResponse
import           Euler.API.Order
import           Euler.API.Refund

import qualified Euler.Common.Types as C

import qualified Euler.Product.Domain as D


import           Control.Comonad hiding ((<<=))
import           Data.Semigroup
import qualified Data.Text as T
import           Generics.Deriving.Semigroup (gsappenddefault)


(<<=) :: Comonad w => w a -> (w a -> b) -> w b
(<<=) = (=>>)

type OrderStatusResponseBuilder = OrderStatusResponseCollector -> D.OrderStatusResponse

emptyBuilder :: OrderStatusResponseBuilder -> D.OrderStatusResponse
emptyBuilder builder = builder mempty

buildOrderStatusResponse :: OrderStatusResponseBuilder
buildOrderStatusResponse OrderStatusResponseCollector{..} = D.OrderStatusResponse
  { id                        = fromMaybe T.empty $ fmap getFirst idT
  , merchant_id               = fmap getFirst merchant_idT
  , amount                    = whenNothing (fmap getLast amountT) (Just mempty)
  , currency                  = fmap getLast currencyT
  , order_id                  = fmap getFirst order_idT
  , date_created              = fromMaybe T.empty $ fmap getLast date_createdT
  , return_url                = fmap getLast return_urlT
  , product_id                = fromMaybe T.empty $ fmap getLast product_idT
  , customer_email            = fmap getLast customer_emailT
  , customer_phone            = fmap getLast customer_phoneT
  , customer_id               = fmap getLast customer_idT
  , payment_links             = fromMaybe D.defaultPaymentlinks $ fmap getLast payment_linksT
  , udf                       = fromMaybe C.emptyUDF $ fmap getLast udfT
  , txn_id                    = fmap getLast txn_idT
  , status_id                 = fromMaybe 0 $ fmap getLast status_idT
  , status                    = fromMaybe D.DEFAULT $ fmap getLast statusT
  , payment_method_type       = fmap getLast payment_method_typeT
  , auth_type                 = fmap getLast auth_typeT
  , card                      = fmap getLast cardT
  , payment_method            = fmap getLast payment_methodT
  , refunded                  = fmap getLast refundedT
  , amount_refunded           = fmap getLast amount_refundedT
  , chargebacks               = fmap getLast chargebacksT
  , refunds                   = fmap getLast refundsT
  , mandate                   = fmap getLast mandateT
  , promotion                 = fmap getLast promotionT
  , risk                      = fmap getLast riskT
  , bank_error_code           = fmap getLast bank_error_codeT
  , bank_error_message        = fmap getLast bank_error_messageT
  , txn_uuid                  = fmap getLast txn_uuidT
  , gateway_payload           = fmap getLast gateway_payloadT
  , txn_detail                = fmap getLast txn_detailT
  , payment_gateway_response  = fmap getLast payment_gateway_responseT
  , gateway_id                = fmap getLast gateway_idT
  , emi_bank                  = fmap getLast emi_bankT
  , emi_tenure                = fmap getLast emi_tenureT
  , gateway_reference_id      = fmap getLast gateway_reference_idT
  , payer_vpa                 = fmap getLast payer_vpaT
  , payer_app_name            = fmap getLast payer_app_nameT
  , second_factor_response    = fmap getLast second_factor_responseT
  , txn_flow_info             = fmap getLast txn_flow_infoT
  }


data OrderStatusResponseCollector = OrderStatusResponseCollector
  {  idT                        :: Maybe (First Text)
  ,  merchant_idT               :: Maybe (First Text)
  ,  amountT                    :: Maybe (Last C.Money)
  ,  currencyT                  :: Maybe (Last Text)
  ,  order_idT                  :: Maybe (First Text)
  ,  date_createdT              :: Maybe (Last Text)
  ,  return_urlT                :: Maybe (Last Text)
  ,  product_idT                :: Maybe (Last Text)
  ,  customer_emailT            :: Maybe (Last Text)
  ,  customer_phoneT            :: Maybe (Last Text)
  ,  customer_idT               :: Maybe (Last Text)
  ,  payment_linksT             :: Maybe (Last D.Paymentlinks)
  ,  udfT                       :: Maybe (Last C.UDF)
  ,  txn_idT                    :: Maybe (Last Text)
  ,  status_idT                 :: Maybe (Last Int)
  ,  statusT                    :: Maybe (Last D.OrderTxnStatus)
  ,  payment_method_typeT       :: Maybe (Last Text)
  ,  auth_typeT                 :: Maybe (Last Text)
  ,  cardT                      :: Maybe (Last D.Card)
  ,  payment_methodT            :: Maybe (Last Text)
  ,  refundedT                  :: Maybe (Last Bool)
  ,  amount_refundedT           :: Maybe (Last C.Money)
  ,  chargebacksT               :: Maybe (Last [D.Chargeback])
  ,  refundsT                   :: Maybe (Last [D.Refund])
  ,  mandateT                   :: Maybe (Last D.Mandate)
  ,  promotionT                 :: Maybe (Last D.PromotionActive)
  ,  riskT                      :: Maybe (Last D.Risk)
  ,  bank_error_codeT           :: Maybe (Last Text)
  ,  bank_error_messageT        :: Maybe (Last Text)
  ,  txn_uuidT                  :: Maybe (Last Text)
  ,  gateway_payloadT           :: Maybe (Last Text)
  ,  txn_detailT                :: Maybe (Last D.TxnDetail)
  ,  payment_gateway_responseT  :: Maybe (Last D.MerchantPaymentGatewayResponse)
  ,  gateway_idT                :: Maybe (Last Int)
  ,  emi_bankT                  :: Maybe (Last Text)
  ,  emi_tenureT                :: Maybe (Last Int)
  ,  gateway_reference_idT      :: Maybe (Last Text)
  ,  payer_vpaT                 :: Maybe (Last Text)
  ,  payer_app_nameT            :: Maybe (Last Text)
  ,  juspayT                    :: Maybe (Last D.OrderTokenResp)
  ,  second_factor_responseT    :: Maybe (Last D.SecondFactorResponse)
  ,  txn_flow_infoT             :: Maybe (Last D.TxnFlowInfo)
  }
  deriving (Show, Eq, Ord, Generic)

defaultOrderStatusResponseCollector :: OrderStatusResponseCollector
defaultOrderStatusResponseCollector = OrderStatusResponseCollector
  {  idT                        = mempty
  ,  merchant_idT               = mempty
  ,  amountT                    = mempty
  ,  currencyT                  = mempty
  ,  order_idT                  = mempty
  ,  date_createdT              = mempty
  ,  return_urlT                = mempty
  ,  product_idT                = mempty
  ,  customer_emailT            = mempty
  ,  customer_phoneT            = mempty
  ,  customer_idT               = mempty
  ,  payment_linksT             = mempty
  ,  udfT                       = mempty
  ,  txn_idT                    = mempty
  ,  status_idT                 = mempty
  ,  statusT                    = mempty
  ,  payment_method_typeT       = mempty
  ,  auth_typeT                 = mempty
  ,  cardT                      = mempty
  ,  payment_methodT            = mempty
  ,  refundedT                  = mempty
  ,  amount_refundedT           = mempty
  ,  chargebacksT               = mempty
  ,  refundsT                   = mempty
  ,  mandateT                   = mempty
  ,  promotionT                 = mempty
  ,  riskT                      = mempty
  ,  bank_error_codeT           = mempty
  ,  bank_error_messageT        = mempty
  ,  txn_uuidT                  = mempty
  ,  gateway_payloadT           = mempty
  ,  txn_detailT                = mempty
  ,  payment_gateway_responseT  = mempty
  ,  gateway_idT                = mempty
  ,  emi_bankT                  = mempty
  ,  emi_tenureT                = mempty
  ,  gateway_reference_idT      = mempty
  ,  payer_vpaT                 = mempty
  ,  payer_app_nameT            = mempty
  ,  second_factor_responseT    = mempty
  ,  txn_flow_infoT             = mempty
  }

instance Semigroup OrderStatusResponseCollector where
  (<>) = gsappenddefault

instance Monoid OrderStatusResponseCollector where
  mempty = defaultOrderStatusResponseCollector
  mappend = (<>)



