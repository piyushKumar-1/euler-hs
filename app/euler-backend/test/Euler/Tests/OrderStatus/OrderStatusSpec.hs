module Euler.Tests.OrderStatus.OrderStatusSpec
       (
         spec
       ) where

import           EulerHS.Prelude hiding (show)
import qualified Prelude as P (show)

import           Control.Monad.Except (Except (..), runExcept)
import qualified Data.Text as T
import           Data.Time
import           Test.Hspec

import           Euler.API.Order (Chargeback' (..), Mandate' (..), OrderStatusQuery (..),
                                  OrderStatusResponse (..), Paymentlinks (..), Refund' (..),
                                  Risk (..), TxnDetail' (..))

import           Euler.Common.Types.Mandate (PaymentMethodType (..))
import           Euler.Common.Types.Order (MandateFeature (..), OrderStatus (..), OrderType (..))
import           Euler.Common.Types.Promotion (Promotion' (..), Rules (..))
import qualified Euler.Common.Types.Refund as Refund
import           Euler.Common.Types.TxnDetail (TxnStatus (..))

import           Euler.Product.Domain.Order (OrderId (..))
import           Euler.Product.OLTP.Order.OrderStatus (makeOrderStatusResponse)

import           Euler.Storage.Types.OrderReference (OrderReference, OrderReferenceT (..))
import           Euler.Storage.Types.TxnCardInfo (TxnCardInfo, TxnCardInfoT (..))
import           Euler.Storage.Types.TxnDetail (TxnDetail, TxnDetailT (..))




spec :: Spec
spec =
    describe "makeOrderStatusResponse" $ do
      it "success makeOrderStatusResponse" $ \rt -> do
        let statusResp = runExcept $ makeOrderStatusResponse
              orderRef
              paymentlinks
              mPromotion
              mMandate
              query
              mTxnDetailJust
              gatewayReferenceId
              mRisk
              txnCardInfo
              mCardBrand
              mRefunds
              mChargeback
        statusResp `shouldBe` Right orderStatusResponse


orderRef :: OrderReference
orderRef = OrderReference
  { id                = Just "orderRef_id"
  , version           = 1
  , amount            = Just 20
  , currency          = Just "USD"
  , dateCreated       = LocalTime (fromGregorian 2020 1 12) (TimeOfDay 2 13 0)
  , lastModified      = LocalTime (fromGregorian 2020 1 13) (TimeOfDay 12 14 0)
  , merchantId        = Just "merchantId"
  , orderId           = Just "orderId"
  , status            = SUCCESS
  , customerEmail     = Just "email@email.ru"
  , customerId        = Just "customerId"
  , browser           = Just "firefox"
  , browserVersion    = Just "72"
  , popupLoaded       = Just False
  , popupLoadedTime   = Just "1"
  , description       = Just "description"
  , udf1              = Just "udf1"
  , udf10             = Just "udf10"
  , udf2              = Just "udf2"
  , udf3              = Just "udf3"
  , udf4              = Just "udf4"
  , udf5              = Just "udf5"
  , udf6              = Just "udf6"
  , udf7              = Just "udf7"
  , udf8              = Just "udf8"
  , udf9              = Just "udf9"
  , returnUrl         = Just "returnUrl"
  , amountRefunded    = Just 0
  , refundedEntirely  = Just False
  , preferredGateway  = Just "preferredGateway"
  , customerPhone     = Just "911"
  , productId         = Just "productId"
  , billingAddressId  = Just "billingAddressId"
  , shippingAddressId = Just "shippingAddressId"
  , orderUuid         = Just "orderUuid"
  , lastSynced        = Just $ LocalTime (fromGregorian 2020 1 13) (TimeOfDay 20 31 0)
  , orderType         = Just MANDATE_REGISTER
  , mandateFeature    = Just OPTIONAL
  , autoRefund        = Just True
  }

paymentlinks :: Paymentlinks
paymentlinks = Paymentlinks
  { iframe = Just "iFrame"
  , web    = Just "web"
  , mobile = Just "mobile"
  }

mPromotion :: Maybe Promotion'
mPromotion = Just Promotion'
  { id              = Just "promotion_id"
  , order_id        = Just "odrer_id"
  , rules           = Just [rule]
  , created         = Just "2018-07-01"
  , discount_amount = Just (-9)
  , status          = Just "ACTIVE"
  }

rule :: Rules
rule = Rules
  { dimension = "dimension"
  , value     = "value"
  }

mMandate :: Maybe Mandate'
mMandate = Just Mandate'
  { mandate_token  = "mandate_token"
  , mandate_status = Just "ACTIVE"
  , mandate_id     = "mandate_id"
  }

query :: OrderStatusQuery
query = OrderStatusQuery
  { orderId                 = OrderId "orderId"
  , merchantId              = "merchantId"
  , resellerId              = Just "resellerId"
  , isAuthenticated         = True
  , sendCardIsin            = True
  , txnId                   = Just "txnId"
  , sendFullGatewayResponse = True
  }

mTxnDetailJust :: Maybe TxnDetail
mTxnDetailJust = Just TxnDetail
  { id                       = Just "txnCardInfo_id"
  , version                  = 2
  , errorMessage             = Just "error"
  , orderId                  = "orderId"
  , status                   = STARTED
  , txnId                    = "txnId"
  , txdType                  = "AUTH"
  , dateCreated              = Just $ LocalTime (fromGregorian 2020 1 14) (TimeOfDay 3 30 0)
  , lastModified             = Just $ LocalTime (fromGregorian 2020 1 14) (TimeOfDay 4 40 0)
  , successResponseId        = Just "successResponseId"
  , txnMode                  = Just "PROD"
  , addToLocker              = Just False
  , merchantId               = Just "merchantId"
  , bankErrorCode            = Just "bankErrorCode"
  , bankErrorMessage         = Just "bankErrorMessage"
  , gateway                  = Just "CYBERSOURCE"
  , expressCheckout          = Just False
  , redirect                 = Just False
  , gatewayPayload           = Just "gatewayPayload"
  , isEmi                    = Just True
  , emiBank                  = Just "emiBank"
  , emiTenure                = Just 3
  , username                 = Just "username"
  , txnUuid                  = Just "txnUuid"
  , merchantGatewayAccountId = Just 5
  , txnAmount                = Just 0
  , txnObjectType            = Just "txnObjectType"
  , sourceObject             = Just "sourceObject"
  , sourceObjectId           = Just "sourceObjectId"
  , currency                 = Just "EUR"
  , netAmount                = Just 0
  , surchargeAmount          = Just 0
  , taxAmount                = Just 0
  }

mTxnDetailNothing :: Maybe TxnDetail
mTxnDetailNothing = Nothing

gatewayReferenceId :: Text
gatewayReferenceId = "JUSPAY:gateway_reference_id"

mRisk :: Maybe Risk
mRisk = Just Risk
  { provider            = Just "provider"
  , status              = Just "ACTIVE"
  , message             = Just "message"
  , flagged             = Just "flagged"
  , recommended_action  = Just "recommended_action"
  , ebs_risk_level      = Just "ebs_risk_level"
  , ebs_payment_status  = Just "ebs_payment_status"
  , ebs_bin_country     = Just "ebs_bin_country"
  , ebs_risk_percentage = Just "ebs_risk_percentage"
  }

txnCardInfo :: Maybe TxnCardInfo
txnCardInfo = Just TxnCardInfo
  { id = Just "txnCardInfo_id"
  , txnId = "txnId"
  , cardIsin = Just "cardIsin"
  , cardIssuerBankName = Just "cardIssuerBankName"
  , cardExpYear = Just "2023"
  , cardExpMonth = Just "12"
  , cardSwitchProvider = Just "cardSwitchProvider"
  , cardType = Just "cardType"
  , cardLastFourDigits = Just "1234"
  , nameOnCard = Just "Ivanov"
  , cardFingerprint = Just "cardFingerprint"
  , cardReferenceId = Just "cardReferenceId"
  , txnDetailId = Just "txnCardInfo_id"
  , dateCreated = Just $ LocalTime (fromGregorian 2020 1 13) (TimeOfDay 12 43 0)
  , paymentMethodType = Just UPI
  , paymentMethod = Just "paymentMethod"
  , cardGlobalFingerprint = Just "cardGlobalFingerprint"
  , paymentSource = Just "paymentSource"
  , authType = Just "authType"
  }

mCardBrand :: Maybe Text
mCardBrand = Just "VISA"

mRefunds :: Maybe [Refund']
mRefunds = Just
  [
    Refund'
      {  id                    = "refund_id"
      ,  amount                = 0
      ,  unique_request_id     = "unique_request_id"
      ,  ref                   = "ref"
      ,  created               = "2020-01-21"
      ,  status                = Refund.SUCCESS
      ,  error_message         = "error_message"
      ,  sent_to_gateway       = True
      ,  arn                   = "arn"
      ,  initiated_by          = "initiated_by"
      ,  internal_reference_id = "internal_reference_id"
      ,  refund_source         = "refund_source"
      ,  refund_type           = "refund_type"
      }
  ]

mChargeback :: Maybe [Chargeback']
mChargeback = Just
  [ Chargeback'
    {  id                  = "chargeback_id"
    ,  amount              = 0
    ,  object_reference_id = "object_reference_id"
    ,  txn                 = txnDetailApi
    ,  date_resolved       = Just $ LocalTime (fromGregorian 2020 2 14) (TimeOfDay 5 33 0)
    ,  date_created        = LocalTime (fromGregorian 2020 1 13) (TimeOfDay 1 30 0)
    ,  last_updated        = LocalTime (fromGregorian 2020 1 13) (TimeOfDay 13 30 0)
    ,  object              = "object"
    ,  dispute_status      = "dispute_status"
    }
  ]
  where
    txnDetailApi = TxnDetail'
      { txn_id           = "txn_id"
      , order_id         = "order_id"
      , txn_uuid         = Just "txn_uuid"
      , gateway_id       = Just 2
      , status           = "STARTED"
      , gateway          = Just "CYBERSOURCE"
      , express_checkout = Just False
      , redirect         = Just False
      , net_amount       = Just "0"
      , surcharge_amount = Just "0"
      , tax_amount       = Just "0"
      , txn_amount       = Just "0"
      , currency         = Just "USD"
      , error_message    = Just "error_message"
      , error_code       = Just "error_code"
      , txn_object_type  = Just "txn_object_type"
      , source_object    = Just "source_object"
      , source_object_id = Just "source_object_id"
      , created          = Just $ LocalTime (fromGregorian 2020 1 13) (TimeOfDay 1 30 0)
      }

orderStatusResponse :: OrderStatusResponse
orderStatusResponse = OrderStatusResponse
  { id = "orderUuid"
  , merchant_id = Just "merchantId"
  , amount = Just 11.0
  , currency = Just "USD"
  , order_id = Just "orderId"
  , date_created = "2020-01-12 02:13:00"
  , return_url = Just "returnUrl"
  , product_id = "productId"
  , customer_email = Just "email@email.ru"
  , customer_phone = Just "911"
  , customer_id = Just "customerId"
  , payment_links = Paymentlinks {iframe = Just "iFrame", web = Just "web", mobile = Just "mobile"}
  , udf1 = "udf1"
  , udf2 = "udf2"
  , udf3 = "udf3"
  , udf4 = "udf4"
  , udf5 = "udf5"
  , udf6 = "udf6"
  , udf7 = "udf7"
  , udf8 = "udf8"
  , udf9 = "udf9"
  , udf10 = "udf10"
  , txn_id = Just "txnId"
  , status_id = 20
  , status = "STARTED"
  , payment_method_type = Nothing
  , auth_type = Just "authType"
  , card = Nothing
  , payment_method = Nothing
  , refunded = Just False
  , amount_refunded = Just 0.0
  , chargebacks = Just
    [ Chargeback'
      { id = "chargeback_id"
      , amount = 0.0
      , object_reference_id = "object_reference_id"
      , txn = TxnDetail'
          { txn_id = "txn_id"
          , order_id = "order_id"
          , txn_uuid = Just "txn_uuid"
          , gateway_id = Just 2
          , status = "STARTED"
          , gateway = Just "CYBERSOURCE"
          , express_checkout = Just False
          , redirect = Just False
          , net_amount = Just "0"
          , surcharge_amount = Just "0"
          , tax_amount = Just "0"
          , txn_amount = Just "0"
          , currency = Just "USD"
          , error_message = Just "error_message"
          , error_code = Just "error_code"
          , txn_object_type = Just "txn_object_type"
          , source_object = Just "source_object"
          , source_object_id = Just "source_object_id"
          , created = Just $ LocalTime (fromGregorian 2020 1 13) (TimeOfDay 1 30 0)
          }
      , date_resolved = Just $ LocalTime (fromGregorian 2020 2 14) (TimeOfDay 5 33 0)
      , date_created = LocalTime (fromGregorian 2020 1 13) (TimeOfDay 1 30 0)
      , last_updated = LocalTime (fromGregorian 2020 1 13) (TimeOfDay 13 30 0)
      , object = "object"
      , dispute_status = "dispute_status"
      }
    ]
  , refunds = Just
      [ Refund'
        { id = "refund_id"
        , amount = 0.0
        , unique_request_id = "unique_request_id"
        , ref = "ref"
        , created = "2020-01-21"
        , status = Refund.SUCCESS
        , error_message = "error_message"
        , sent_to_gateway = True
        , arn = "arn"
        , initiated_by = "initiated_by"
        , internal_reference_id = "internal_reference_id"
        , refund_source = "refund_source"
        , refund_type = "refund_type"
        }
      ]
    , mandate = Just
      ( Mandate'
        { mandate_token = "mandate_token"
        , mandate_status = Just "ACTIVE"
        , mandate_id = "mandate_id"
        }
      )
    , promotion = Just
      ( Promotion'
        { id = Just "promotion_id"
        , order_id = Just "odrer_id"
        , rules = Just [Rules {dimension = "dimension", value = "value"}]
        , created = Just "2018-07-01"
        , discount_amount = Just (-9.0)
        , status = Just "ACTIVE"
        }
      )
    , risk = Just
      ( Risk
        { provider = Just "provider"
        , status = Just "ACTIVE"
        , message = Just "message"
        , flagged = Just "flagged"
        , recommended_action = Just "recommended_action"
        , ebs_risk_level = Just "ebs_risk_level"
        , ebs_payment_status = Just "ebs_payment_status"
        , ebs_bin_country = Just "ebs_bin_country"
        , ebs_risk_percentage = Just "ebs_risk_percentage"
        }
      )
    , bank_error_code = Just "bankErrorCode"
    , bank_error_message = Just "bankErrorMessage"
    , txn_uuid = Just "txnUuid"
    , gateway_payload = Nothing
    , txn_detail = Just
      ( TxnDetail'
        { txn_id = "txnId"
        , order_id = "orderId"
        , txn_uuid = Just "txnUuid"
        , gateway_id = Just 6
        , status = "STARTED"
        , gateway = Just "CYBERSOURCE"
        , express_checkout = Just False
        , redirect = Just False
        , net_amount = Just "0.0"
        , surcharge_amount = Just "0.0"
        , tax_amount = Just "0.0"
        , txn_amount = Just "0.0"
        , currency = Just "EUR"
        , error_message = Just "bankErrorMessage"
        , error_code = Just "bankErrorCode"
        , txn_object_type = Just "txnObjectType"
        , source_object = Just "sourceObject"
        , source_object_id = Just "sourceObjectId"
        , created = Just $ LocalTime (fromGregorian 2020 1 14) (TimeOfDay 3 30 0)
        }
      )
    , payment_gateway_response' = Nothing
    , payment_gateway_response = Nothing
    , gateway_id = Just 6
    , emi_bank = Just "emiBank"
    , emi_tenure = Just 3
    , gateway_reference_id = Just "JUSPAY:gateway_reference_id"
    , payer_vpa = Nothing
    , payer_app_name = Nothing
    , juspay = Nothing
    , second_factor_response = Nothing
    , txn_flow_info = Nothing
    }

defaultPaymentlinks :: Paymentlinks
defaultPaymentlinks = Paymentlinks
  { iframe = Nothing
  , web    = Nothing
  , mobile = Nothing
  }

