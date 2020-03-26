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

import qualified Euler.Common.Types as C
import           Euler.Common.Types.External.Mandate (MandateFeature (..), MandateStatus (..),
                                                      PaymentMethodType (..))
import qualified Euler.Common.Types.External.Order as OEx
import           Euler.Common.Types.TxnDetail (TxnStatus (..))

import           Euler.Product.OLTP.Order.OrderStatus (makeOrderStatusResponse)

import qualified Euler.Product.Domain as D
import qualified Euler.Product.Domain.OrderStatusResponse as DO
import qualified Euler.Storage.Types as DB




spec :: Spec
spec =
    describe "makeOrderStatusResponse" $ do
      it "Success with txnDetailJust, promotionJust" $ \rt -> do
        let statusResp = runExcept $ makeOrderStatusResponse
              order
              paymentlinks
              promotionJust
              mMandate
              query
              txnDetailJust
              gatewayReferenceId
              mRisk
              txnCardInfo
              mCardBrand
              mRefunds
              mChargeback
              returnUrlGoogle
              (payMethod, payMethodType, payerVpa, payerAppName)
              (txnFlowInfo, secondFactorResponse)
              mMerchantPgr
        statusResp `shouldBe` Right orderStatusResponse1

      it "Success with txnDetailNothing, promotionJust" $ \rt -> do
        let statusResp = runExcept $ makeOrderStatusResponse
              order
              paymentlinks
              promotionJust
              mMandate
              query
              txnDetailNothing
              gatewayReferenceId
              mRisk
              txnCardInfo
              mCardBrand
              mRefunds
              mChargeback
              returnUrlGoogle
              (payMethod, payMethodType, payerVpa, payerAppName)
              (txnFlowInfo, secondFactorResponse)
              mMerchantPgr
        statusResp `shouldBe` Right orderStatusResponse2

      it "Success with txnDetailJust, promotionNothing" $ \rt -> do
        let statusResp = runExcept $ makeOrderStatusResponse
              order
              paymentlinks
              promotionNothing
              mMandate
              query
              txnDetailJust
              gatewayReferenceId
              mRisk
              txnCardInfo
              mCardBrand
              mRefunds
              mChargeback
              returnUrlGoogle
              (payMethod, payMethodType, payerVpa, payerAppName)
              (txnFlowInfo, secondFactorResponse)
              mMerchantPgr
        statusResp `shouldBe` Right orderStatusResponse3

mMerchantPgr :: Maybe D.MerchantPaymentGatewayResponse
mMerchantPgr = Just $ D.MerchantPaymentGatewayResponse
  { respCode           = Just "resp_code"
  , rrn                = Just "rrn"
  , created            = Just "created"
  , epgTxnId           = Just "epg_txn_id"
  , respMessage        = Just "resp_message"
  , authIdCode         = Just "auth_id_code"
  , txnId              = Just "txn_id"
  , offer              = Just "offer"
  , offerType          = Just "offer_type"
  , offerAvailed       = Just "offer_availed"
  , discountAmount     = Just "discount_amount"
  , offerFailureReason = Just "offer_failure_reason"
  , gatewayResponse    = Just "gateway_response"
  }

order :: D.Order
order = D.Order
  { id                = 13
  , version           = 1
  , amount            = C.mkMoney 20
  , currency          = C.USD
  , merchantId        = "merchantId"
  , orderId           = "orderId"
  , orderUuid         = "orderUuid"
  , orderType         = C.MANDATE_REGISTER
  , orderStatus       = C.OrderStatusSuccess
  , customerId        = Just "customerId"
  , customerEmail     = Just "email@email.ru"
  , customerPhone     = Just "911"
  , billingAddressId  = Just 14
  , shippingAddressId = Just 15
  , udf               = udfFull
  , description       = Just "description"
  , returnUrl         = Just "returnUrl"
  , amountRefunded    = Just $ C.mkMoney 0
  , refundedEntirely  = False
  , autoRefund        = True
  , productId         = Just "productId"
  , mandate           = REQUIRED
  , lastSynced        = Just $ LocalTime (fromGregorian 2020 1 13) (TimeOfDay 20 31 0)
  , dateCreated       = LocalTime (fromGregorian 2020 1 12) (TimeOfDay 2 13 0)
  , lastModified      = LocalTime (fromGregorian 2020 1 13) (TimeOfDay 12 14 0)
  -- , browser           = Just "firefox"
  -- , browserVersion    = Just "72"
  -- , popupLoaded       = Just False
  -- , popupLoadedTime   = Just "1"
  -- , preferredGateway  = Just "preferredGateway"
  -- , mandateFeature    = Just OPTIONAL
  }

udfFull :: C.UDF
udfFull = C.UDF
  { udf1  = Just "udf1"
  , udf2  = Just "udf2"
  , udf3  = Just "udf3"
  , udf4  = Just "udf4"
  , udf5  = Just "udf5"
  , udf6  = Just "udf6"
  , udf7  = Just "udf7"
  , udf8  = Just "udf8"
  , udf9  = Just "udf9"
  , udf10 = Just "udf10"
  }

secondFactorResponse :: Maybe D.SecondFactorResponse
secondFactorResponse = Just $ D.SecondFactorResponse
  { id = D.SecondFactorResponsePId 12
  , version = 13
  , cavv = Just "cavv"
  , currency =  Just "currency"
  , eci = "eci"
  , mpiErrorCode = Just "mpiErrorCode"
  , purchaseAmount = Just 34
  , responseId = Just "responseId"
  , shoppingContext = "shoppingContext"
  , status = "status"
  , xid = "xid"
  , dateCreated = LocalTime (fromGregorian 2019 3 23) (TimeOfDay 2 13 0)
  , secondFactorId = Just 120
  , gatewayAuthResData = Just "gatewayAuthResData"
  }

txnFlowInfo :: Maybe D.TxnFlowInfo
txnFlowInfo = Just $ D.TxnFlowInfo
  { flowType = Just C.VIES_ENROLLMENT
  , status = Just "status"
  , errorCode = Just "error_code"
  , errorMessage = Just "error_message"
  }

payMethod :: Maybe Text
payMethod = Just "payment_method"

payMethodType :: Maybe Text
payMethodType = Just "payment_method_type"

payerVpa :: Maybe Text
payerVpa = Just "payer_vpa"

payerAppName :: Maybe Text
payerAppName = Just "payer_app_name"

returnUrlGoogle :: Maybe Text
returnUrlGoogle = Just "http://google.ru"

paymentlinks :: D.Paymentlinks
paymentlinks = D.Paymentlinks
  { iframe = "iFrame"
  , web    = "web"
  , mobile = "mobile"
  }

promotionJust :: Maybe D.PromotionActive
promotionJust = Just D.PromotionActive
  { id               = D.PromotionPId 89
  , orderId          = "orderId"
  , rules            = rule
  , dateCreated      = LocalTime (fromGregorian 2019 2 4) (TimeOfDay 4 4 12)
  , discountAmount   = C.mkMoney (-10)
  , status           = "status"
  }

rule :: C.Rules
rule = C.Rules
  { dimension = "dimension"
  , value     = "value"
  }

promotionNothing :: Maybe D.PromotionActive
promotionNothing = Nothing

mMandate :: Maybe D.Mandate
mMandate = Just D.Mandate
  {  id                       = D.MandatePId 1000000
  ,  merchantId               = "merchantId"
  ,  endDate                  = Just $ LocalTime (fromGregorian 2019 5 14) (TimeOfDay 13 21 18)
  ,  startDate                = Just $ LocalTime (fromGregorian 2018 12 14) (TimeOfDay 10 21 18)
  ,  maxAmount                = Just $ C.mkMoney 200
  ,  merchantCustomerId       = Just "merchantCustomerId"
  ,  paymentMethod            = Just "paymentMethod"
  ,  paymentMethodType        = Just WALLET
  ,  status                   = ACTIVE
  ,  token                    = "token"
  ,  mandateId                = "mandateId"
  ,  paymentMethodId          = Just "paymentMethodId"
  ,  gateway                  = Just C.CYBERSOURCE
  ,  gatewayParams            = Just "gatewayParams"
  ,  authOrderId              = Just 65
  ,  activatedAt              = Just $ LocalTime (fromGregorian 2018 4 24) (TimeOfDay 12 20 10)
  ,  dateCreated              = LocalTime (fromGregorian 2019 2 16) (TimeOfDay 13 30 0)
  ,  lastModified             = LocalTime (fromGregorian 2021 1 14) (TimeOfDay 14 34 1)
  ,  authTxnCardInfo          = Just "authTxnCardInfo"
  ,  currency                 = Just C.USD
  ,  merchantGatewayAccountId = Just 33
  ,  metadata                 = Just "metadata"
  }


query :: DO.OrderStatusRequest
query = DO.OrderStatusRequest
  { orderId                 = "orderId"
  , merchantId              = "merchantId"
  , resellerId              = Just "resellerId"
  , isAuthenticated         = True
  , sendCardIsin            = True
  , sendFullGatewayResponse = True
  }

txnDetailJust :: Maybe D.TxnDetail
txnDetailJust = Just D.TxnDetail
  { id                       = D.TxnDetailPId 7
  , version                  = 10
  , errorMessage             = Just "errorMessage"
  , orderId                  = "orderId"
  , status                   = CHARGED
  , txnId                    = "txnId"
  , txdType                  = "txdType"
  , dateCreated              = Just $ LocalTime (fromGregorian 2020 1 14) (TimeOfDay 3 30 0)
  , lastModified             = Just $ LocalTime (fromGregorian 2020 1 15) (TimeOfDay 12 30 0)
  , successResponseId        = Just 9
  , txnMode                  = Just "txnMode"
  , addToLocker              = Just True
  , merchantId               = Just "merchantId"
  , bankErrorCode            = Just "bankErrorCode"
  , bankErrorMessage         = Just "bankErrorMessage"
  , gateway                  = Just C.CYBERSOURCE
  , expressCheckout          = Just True
  , redirect                 = Just False
  , gatewayPayload           = Just "gatewayPayload"
  , isEmi                    = Just True
  , emiBank                  = Just "emiBank"
  , emiTenure                = Just 11
  , username                 = Just "username"
  , txnUuid                  = Just "txnUuid"
  , merchantGatewayAccountId = Just 34
  , txnAmount                = Just $ C.mkMoney 55
  , txnObjectType            = Just "txnObjectType"
  , sourceObject             = Just "sourceObject"
  , sourceObjectId           = Just "sourceObjectId"
  , currency                 = Just "currency"
  , netAmount                = Just $ C.mkMoney 22
  , surchargeAmount          = Just $ C.mkMoney 33
  , taxAmount                = Just $ C.mkMoney 44
  }

txnDetailNothing :: Maybe D.TxnDetail
txnDetailNothing = Nothing

gatewayReferenceId :: Text
gatewayReferenceId = "JUSPAY:gateway_reference_id"

mRisk :: Maybe D.Risk
mRisk = Just D.Risk
  { provider          = Just "provider"
  , status            = Just "ACTIVE"
  , message           = Just "message"
  , flagged           = Just True
  , recommendedAction = Just "recommended_action"
  , ebsRiskLevel      = "ebs_risk_level"
  , ebsPaymentStatus  = Just "ebs_payment_status"
  , ebsBinCountry     = "ebs_bin_country"
  , ebsRiskPercentage = "ebs_risk_percentage"
  }

txnCardInfo :: Maybe D.TxnCardInfo
txnCardInfo = Just D.TxnCardInfo
  { id = D.TxnCardInfoPId 200
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
  , txnDetailId = Just 200
  , dateCreated = Just $ LocalTime (fromGregorian 2020 1 13) (TimeOfDay 12 43 0)
  , paymentMethodType = Just UPI
  , paymentMethod = Just "paymentMethod"
  , cardGlobalFingerprint = Just "cardGlobalFingerprint"
  , paymentSource = Just "paymentSource"
  , authType = Just "authType"
  }

mCardBrand :: Maybe Text
mCardBrand = Just "VISA"

mRefunds :: Maybe [D.Refund]
mRefunds = Just
  [ D.Refund
      { id                  = D.RefundPId 832
      , amount              = C.mkMoney 700
      , authorizationId     = Just "authorizationId"
      , dateCreated         = LocalTime (fromGregorian 2019 12 16) (TimeOfDay 14 12 30)
      , epgTxnId            = Just "epgTxnId"
      , gateway             = "gateway"
      , processed           = True
      , rootReferenceNumber = Just "rootReferenceNumber"
      , txnDetailId         = Just 2
      , referenceId         = Just "referenceId"
      , status              = C.PENDING
      , uniqueRequestId     = Just "uniqueRequestId"
      , errorMessage        = Just "errorMessage"
      , sentToGateway       = Just False
      , responseCode        = Just "responseCode"
      , internalReferenceId = Just "internalReferenceId"
      , refundArn           = Just "refundArn"
      , initiatedBy         = Just "initiatedBy"
      , refundType          = Just "refundType"
      , refundSource        = Just "refundSource"
      , lastModified        = Just $ LocalTime (fromGregorian 2020 1 26) (TimeOfDay 12 34 0)
      }
  ]

mChargeback :: Maybe [D.Chargeback]
mChargeback = Just
  [ D.Chargeback
    { id                = D.ChargebackPId "17"
    , version           = 5
    , amount            = C.mkMoney 200
    , dateCreated       = LocalTime (fromGregorian 2018 10 3) (TimeOfDay 15 4 20)
    , dateResolved      = Just $ LocalTime (fromGregorian 2018 12 3) (TimeOfDay 7 30 20)
    , disputeStatus     = "disputeStatus"
    , lastUpdated       = LocalTime (fromGregorian 2018 11 13) (TimeOfDay 5 30 20)
    , merchantAccountId = 56
    , txnDetailId       = Just 23
    , objectReferenceId = "objectReferenceId"
    }
  ]
  where
    txnDetailApi = D.TxnDetail
      { id                       = D.TxnDetailPId 7
      , version                  = 10
      , errorMessage             = Just "errorMessage"
      , orderId                  = "orderId"
      , status                   = CHARGED
      , txnId                    = "txnId"
      , txdType                  = "txdType"
      , dateCreated              = Just $ LocalTime (fromGregorian 2020 1 14) (TimeOfDay 3 30 0)
      , lastModified             = Just $ LocalTime (fromGregorian 2020 1 15) (TimeOfDay 12 30 0)
      , successResponseId        = Just 9
      , txnMode                  = Just "txnMode"
      , addToLocker              = Just True
      , merchantId               = Just "merchantId"
      , bankErrorCode            = Just "bankErrorCode"
      , bankErrorMessage         = Just "bankErrorMessage"
      , gateway                  = Just C.CYBERSOURCE
      , expressCheckout          = Just True
      , redirect                 = Just False
      , gatewayPayload           = Just "gatewayPayload"
      , isEmi                    = Just True
      , emiBank                  = Just "emiBank"
      , emiTenure                = Just 11
      , username                 = Just "username"
      , txnUuid                  = Just "txnUuid"
      , merchantGatewayAccountId = Just 34
      , txnAmount                = Just $ C.mkMoney 55
      , txnObjectType            = Just "txnObjectType"
      , sourceObject             = Just "sourceObject"
      , sourceObjectId           = Just "sourceObjectId"
      , currency                 = Just "currency"
      , netAmount                = Just $ C.mkMoney 22
      , surchargeAmount          = Just $ C.mkMoney 33
      , taxAmount                = Just $ C.mkMoney 44
      }

orderStatusResponse1 :: DO.OrderStatusResponse
orderStatusResponse1 = DO.OrderStatusResponse
  { id = "orderUuid"
  , merchant_id = Just "merchantId"
  , amount = Just $ C.mkMoney 10
  , currency = Just "USD"
  , order_id = Just "orderId"
  , date_created = "2020-01-12 02:13:00"
  , return_url = Just "http://google.ru"
  , product_id = "productId"
  , customer_email = Just "email@email.ru"
  , customer_phone = Just "911"
  , customer_id = Just "customerId"
  , payment_links = D.Paymentlinks {iframe = "iFrame", web = "web", mobile = "mobile"}
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
  , status_id = 21
  , status = DO.TStatus CHARGED
  , payment_method_type = Just "payment_method_type"
  , auth_type = Just "authType"
  , card = Nothing
  , payment_method = Just "payment_method"
  , refunded = Just False
  , amount_refunded = Just $C.mkMoney 0.0
  , chargebacks = Just
    [ D.Chargeback
      { id                = D.ChargebackPId "17"
      , version           = 5
      , amount            = C.mkMoney 200
      , dateCreated       = LocalTime (fromGregorian 2018 10 3) (TimeOfDay 15 4 20)
      , dateResolved      = Just $ LocalTime (fromGregorian 2018 12 3) (TimeOfDay 7 30 20)
      , disputeStatus     = "disputeStatus"
      , lastUpdated       = LocalTime (fromGregorian 2018 11 13) (TimeOfDay 5 30 20)
      , merchantAccountId = 56
      , txnDetailId       = Just 23
      , objectReferenceId = "objectReferenceId"
      }
    ]
  , refunds = Just
      [ D.Refund
        { id                  = D.RefundPId 832
        , amount              = C.mkMoney 700
        , authorizationId     = Just "authorizationId"
        , dateCreated         = LocalTime (fromGregorian 2019 12 16) (TimeOfDay 14 12 30)
        , epgTxnId            = Just "epgTxnId"
        , gateway             = "gateway"
        , processed           = True
        , rootReferenceNumber = Just "rootReferenceNumber"
        , txnDetailId         = Just 2
        , referenceId         = Just "referenceId"
        , status              = C.PENDING
        , uniqueRequestId     = Just "uniqueRequestId"
        , errorMessage        = Just "errorMessage"
        , sentToGateway       = Just False
        , responseCode        = Just "responseCode"
        , internalReferenceId = Just "internalReferenceId"
        , refundArn           = Just "refundArn"
        , initiatedBy         = Just "initiatedBy"
        , refundType          = Just "refundType"
        , refundSource        = Just "refundSource"
        , lastModified        = Just $ LocalTime (fromGregorian 2020 1 26) (TimeOfDay 12 34 0)
        }
      ]
    , mandate = Just D.Mandate
        {  id                       = D.MandatePId 1000000
        ,  merchantId               = "merchantId"
        ,  endDate                  = Just $ LocalTime (fromGregorian 2019 5 14) (TimeOfDay 13 21 18)
        ,  startDate                = Just $ LocalTime (fromGregorian 2018 12 14) (TimeOfDay 10 21 18)
        ,  maxAmount                = Just $ C.mkMoney 200
        ,  merchantCustomerId       = Just "merchantCustomerId"
        ,  paymentMethod            = Just "paymentMethod"
        ,  paymentMethodType        = Just WALLET
        ,  status                   = ACTIVE
        ,  token                    = "token"
        ,  mandateId                = "mandateId"
        ,  paymentMethodId          = Just "paymentMethodId"
        ,  gateway                  = Just C.CYBERSOURCE
        ,  gatewayParams            = Just "gatewayParams"
        ,  authOrderId              = Just 65
        ,  activatedAt              = Just $ LocalTime (fromGregorian 2018 4 24) (TimeOfDay 12 20 10)
        ,  dateCreated              = LocalTime (fromGregorian 2019 2 16) (TimeOfDay 13 30 0)
        ,  lastModified             = LocalTime (fromGregorian 2021 1 14) (TimeOfDay 14 34 1)
        ,  authTxnCardInfo          = Just "authTxnCardInfo"
        ,  currency                 = Just C.USD
        ,  merchantGatewayAccountId = Just 33
        ,  metadata                 = Just "metadata"
        }
    , promotion = Just D.PromotionActive
        { id               = D.PromotionPId 89
        , orderId          = "orderId"
        , rules            = rule
        , dateCreated      = LocalTime (fromGregorian 2019 2 4) (TimeOfDay 4 4 12)
        , discountAmount   = C.mkMoney (-10)
        , status           = "status"
        }
    , risk = Just
      ( D.Risk
        { provider = Just "provider"
        , status = Just "ACTIVE"
        , message = Just "message"
        , flagged = Just True
        , recommendedAction = Just "recommended_action"
        , ebsRiskLevel = "ebs_risk_level"
        , ebsPaymentStatus = Just "ebs_payment_status"
        , ebsBinCountry = "ebs_bin_country"
        , ebsRiskPercentage = "ebs_risk_percentage"
        }
      )
    , bank_error_code = Just "bankErrorCode"
    , bank_error_message = Just "bankErrorMessage"
    , txn_uuid = Just "txnUuid"
    , gateway_payload = Nothing
    , txn_detail = Just
      ( D.TxnDetail
        { id                       = D.TxnDetailPId 7
        , version                  = 10
        , errorMessage             = Just "errorMessage"
        , orderId                  = "orderId"
        , status                   = CHARGED
        , txnId                    = "txnId"
        , txdType                  = "txdType"
        , dateCreated              = Just $ LocalTime (fromGregorian 2020 1 14) (TimeOfDay 3 30 0)
        , lastModified             = Just $ LocalTime (fromGregorian 2020 1 15) (TimeOfDay 12 30 0)
        , successResponseId        = Just 9
        , txnMode                  = Just "txnMode"
        , addToLocker              = Just True
        , merchantId               = Just "merchantId"
        , bankErrorCode            = Just "bankErrorCode"
        , bankErrorMessage         = Just "bankErrorMessage"
        , gateway                  = Just C.CYBERSOURCE
        , expressCheckout          = Just True
        , redirect                 = Just False
        , gatewayPayload           = Just "gatewayPayload"
        , isEmi                    = Just True
        , emiBank                  = Just "emiBank"
        , emiTenure                = Just 11
        , username                 = Just "username"
        , txnUuid                  = Just "txnUuid"
        , merchantGatewayAccountId = Just 34
        , txnAmount                = Just $ C.mkMoney 55
        , txnObjectType            = Just "txnObjectType"
        , sourceObject             = Just "sourceObject"
        , sourceObjectId           = Just "sourceObjectId"
        , currency                 = Just "currency"
        , netAmount                = Just $ C.mkMoney 22
        , surchargeAmount          = Just $ C.mkMoney 33
        , taxAmount                = Just $ C.mkMoney 44
        }
      )
    , payment_gateway_response = mMerchantPgr
    , gateway_id = Just 6
    , emi_bank = Just "emiBank"
    , emi_tenure = Just 11
    , gateway_reference_id = Just "JUSPAY:gateway_reference_id"
    , payer_vpa = Just "payer_vpa"
    , payer_app_name = Just "payer_app_name"
    , second_factor_response = secondFactorResponse
    , txn_flow_info = txnFlowInfo
    }

defaultPaymentlinks :: D.Paymentlinks
defaultPaymentlinks = D.Paymentlinks
  { iframe = T.empty
  , web    = T.empty
  , mobile = T.empty
  }

orderStatusResponse2 :: DO.OrderStatusResponse
orderStatusResponse2 = DO.OrderStatusResponse
  { id = "orderUuid"
  , merchant_id = Just "merchantId"
  , amount = Just $ C.mkMoney 10
  , currency = Just "USD"
  , order_id = Just "orderId"
  , date_created = "2020-01-12 02:13:00"
  , return_url = Just "http://google.ru"
  , product_id = "productId"
  , customer_email = Just "email@email.ru"
  , customer_phone = Just "911"
  , customer_id = Just "customerId"
  , payment_links = D.Paymentlinks {iframe = "iFrame", web = "web", mobile = "mobile"}
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
  , txn_id = Nothing
  , status_id = 0
  , status = DO.OStatus OEx.SUCCESS
  , payment_method_type = Nothing
  , auth_type = Nothing
  , card = Nothing
  , payment_method = Nothing
  , refunded = Just False
  , amount_refunded = Just $ C.mkMoney 0.0
  , chargebacks = Just
    [ D.Chargeback
      { id                = D.ChargebackPId "17"
      , version           = 5
      , amount            = C.mkMoney 200
      , dateCreated       = LocalTime (fromGregorian 2018 10 3) (TimeOfDay 15 4 20)
      , dateResolved      = Just $ LocalTime (fromGregorian 2018 12 3) (TimeOfDay 7 30 20)
      , disputeStatus     = "disputeStatus"
      , lastUpdated       = LocalTime (fromGregorian 2018 11 13) (TimeOfDay 5 30 20)
      , merchantAccountId = 56
      , txnDetailId       = Just 23
      , objectReferenceId = "objectReferenceId"
      }
    ]
  , refunds = Just
      [ D.Refund
        { id                  = D.RefundPId 832
        , amount              = C.mkMoney 700
        , authorizationId     = Just "authorizationId"
        , dateCreated         = LocalTime (fromGregorian 2019 12 16) (TimeOfDay 14 12 30)
        , epgTxnId            = Just "epgTxnId"
        , gateway             = "gateway"
        , processed           = True
        , rootReferenceNumber = Just "rootReferenceNumber"
        , txnDetailId         = Just 2
        , referenceId         = Just "referenceId"
        , status              = C.PENDING
        , uniqueRequestId     = Just "uniqueRequestId"
        , errorMessage        = Just "errorMessage"
        , sentToGateway       = Just False
        , responseCode        = Just "responseCode"
        , internalReferenceId = Just "internalReferenceId"
        , refundArn           = Just "refundArn"
        , initiatedBy         = Just "initiatedBy"
        , refundType          = Just "refundType"
        , refundSource        = Just "refundSource"
        , lastModified        = Just $ LocalTime (fromGregorian 2020 1 26) (TimeOfDay 12 34 0)
        }
      ]
    , mandate = Just D.Mandate
        {  id                       = D.MandatePId 1000000
        ,  merchantId               = "merchantId"
        ,  endDate                  = Just $ LocalTime (fromGregorian 2019 5 14) (TimeOfDay 13 21 18)
        ,  startDate                = Just $ LocalTime (fromGregorian 2018 12 14) (TimeOfDay 10 21 18)
        ,  maxAmount                = Just $ C.mkMoney 200
        ,  merchantCustomerId       = Just "merchantCustomerId"
        ,  paymentMethod            = Just "paymentMethod"
        ,  paymentMethodType        = Just WALLET
        ,  status                   = ACTIVE
        ,  token                    = "token"
        ,  mandateId                = "mandateId"
        ,  paymentMethodId          = Just "paymentMethodId"
        ,  gateway                  = Just C.CYBERSOURCE
        ,  gatewayParams            = Just "gatewayParams"
        ,  authOrderId              = Just 65
        ,  activatedAt              = Just $ LocalTime (fromGregorian 2018 4 24) (TimeOfDay 12 20 10)
        ,  dateCreated              = LocalTime (fromGregorian 2019 2 16) (TimeOfDay 13 30 0)
        ,  lastModified             = LocalTime (fromGregorian 2021 1 14) (TimeOfDay 14 34 1)
        ,  authTxnCardInfo          = Just "authTxnCardInfo"
        ,  currency                 = Just C.USD
        ,  merchantGatewayAccountId = Just 33
        ,  metadata                 = Just "metadata"
        }
    , promotion = Just
      ( D.PromotionActive
        { id               = D.PromotionPId 89
        , orderId          = "orderId"
        , rules            = rule
        , dateCreated      = LocalTime (fromGregorian 2019 2 4) (TimeOfDay 4 4 12)
        , discountAmount   = C.mkMoney (-10)
        , status           = "status"
        }
      )
    , risk = Just
      ( D.Risk
        { provider = Just "provider"
        , status = Just "ACTIVE"
        , message = Just "message"
        , flagged = Just True
        , recommendedAction = Just "recommended_action"
        , ebsRiskLevel = "ebs_risk_level"
        , ebsPaymentStatus = Just "ebs_payment_status"
        , ebsBinCountry = "ebs_bin_country"
        , ebsRiskPercentage = "ebs_risk_percentage"
        }
      )
    , bank_error_code = Nothing
    , bank_error_message = Nothing
    , txn_uuid = Nothing
    , gateway_payload = Nothing
    , txn_detail = Nothing
    , payment_gateway_response = mMerchantPgr
    , gateway_id = Nothing
    , emi_bank =Nothing
    , emi_tenure = Nothing
    , gateway_reference_id = Nothing
    , payer_vpa = Nothing
    , payer_app_name = Nothing
    , second_factor_response = secondFactorResponse
    , txn_flow_info = txnFlowInfo
    }

orderStatusResponse3 :: DO.OrderStatusResponse
orderStatusResponse3 = DO.OrderStatusResponse
  { id = "orderUuid"
  , merchant_id = Just "merchantId"
  , amount = Just $ C.mkMoney 20
  , currency = Just "USD"
  , order_id = Just "orderId"
  , date_created = "2020-01-12 02:13:00"
  , return_url = Just "http://google.ru"
  , product_id = "productId"
  , customer_email = Just "email@email.ru"
  , customer_phone = Just "911"
  , customer_id = Just "customerId"
  , payment_links = D.Paymentlinks {iframe = "iFrame", web = "web", mobile = "mobile"}
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
  , status_id = 21
  , status = DO.TStatus CHARGED
  , payment_method_type = Just "payment_method_type"
  , auth_type = Just "authType"
  , card = Nothing
  , payment_method = Just "payment_method"
  , refunded = Just False
  , amount_refunded = Just $ C.mkMoney 0.0
  , chargebacks = Just
    [ D.Chargeback
      { id                = D.ChargebackPId "17"
      , version           = 5
      , amount            = C.mkMoney 200
      , dateCreated       = LocalTime (fromGregorian 2018 10 3) (TimeOfDay 15 4 20)
      , dateResolved      = Just $ LocalTime (fromGregorian 2018 12 3) (TimeOfDay 7 30 20)
      , disputeStatus     = "disputeStatus"
      , lastUpdated       = LocalTime (fromGregorian 2018 11 13) (TimeOfDay 5 30 20)
      , merchantAccountId = 56
      , txnDetailId       = Just 23
      , objectReferenceId = "objectReferenceId"
      }
    ]
  , refunds = Just
      [ D.Refund
          { id                  = D.RefundPId 832
          , amount              = C.mkMoney 700
          , authorizationId     = Just "authorizationId"
          , dateCreated         = LocalTime (fromGregorian 2019 12 16) (TimeOfDay 14 12 30)
          , epgTxnId            = Just "epgTxnId"
          , gateway             = "gateway"
          , processed           = True
          , rootReferenceNumber = Just "rootReferenceNumber"
          , txnDetailId         = Just 2
          , referenceId         = Just "referenceId"
          , status              = C.PENDING
          , uniqueRequestId     = Just "uniqueRequestId"
          , errorMessage        = Just "errorMessage"
          , sentToGateway       = Just False
          , responseCode        = Just "responseCode"
          , internalReferenceId = Just "internalReferenceId"
          , refundArn           = Just "refundArn"
          , initiatedBy         = Just "initiatedBy"
          , refundType          = Just "refundType"
          , refundSource        = Just "refundSource"
          , lastModified        = Just $ LocalTime (fromGregorian 2020 1 26) (TimeOfDay 12 34 0)
          }
      ]
    , mandate = Just D.Mandate
        {  id                       = D.MandatePId 1000000
        ,  merchantId               = "merchantId"
        ,  endDate                  = Just $ LocalTime (fromGregorian 2019 5 14) (TimeOfDay 13 21 18)
        ,  startDate                = Just $ LocalTime (fromGregorian 2018 12 14) (TimeOfDay 10 21 18)
        ,  maxAmount                = Just $ C.mkMoney 200
        ,  merchantCustomerId       = Just "merchantCustomerId"
        ,  paymentMethod            = Just "paymentMethod"
        ,  paymentMethodType        = Just WALLET
        ,  status                   = ACTIVE
        ,  token                    = "token"
        ,  mandateId                = "mandateId"
        ,  paymentMethodId          = Just "paymentMethodId"
        ,  gateway                  = Just C.CYBERSOURCE
        ,  gatewayParams            = Just "gatewayParams"
        ,  authOrderId              = Just 65
        ,  activatedAt              = Just $ LocalTime (fromGregorian 2018 4 24) (TimeOfDay 12 20 10)
        ,  dateCreated              = LocalTime (fromGregorian 2019 2 16) (TimeOfDay 13 30 0)
        ,  lastModified             = LocalTime (fromGregorian 2021 1 14) (TimeOfDay 14 34 1)
        ,  authTxnCardInfo          = Just "authTxnCardInfo"
        ,  currency                 = Just C.USD
        ,  merchantGatewayAccountId = Just 33
        ,  metadata                 = Just "metadata"
        }
    , promotion = Nothing
    , risk = Just
      ( D.Risk
        { provider = Just "provider"
        , status = Just "ACTIVE"
        , message = Just "message"
        , flagged = Just True
        , recommendedAction = Just "recommended_action"
        , ebsRiskLevel = "ebs_risk_level"
        , ebsPaymentStatus = Just "ebs_payment_status"
        , ebsBinCountry = "ebs_bin_country"
        , ebsRiskPercentage = "ebs_risk_percentage"
        }
      )
    , bank_error_code = Just "bankErrorCode"
    , bank_error_message = Just "bankErrorMessage"
    , txn_uuid = Just "txnUuid"
    , gateway_payload = Nothing
    , txn_detail = Just
      ( D.TxnDetail
        { id                       = D.TxnDetailPId 7
        , version                  = 10
        , errorMessage             = Just "errorMessage"
        , orderId                  = "orderId"
        , status                   = CHARGED
        , txnId                    = "txnId"
        , txdType                  = "txdType"
        , dateCreated              = Just $ LocalTime (fromGregorian 2020 1 14) (TimeOfDay 3 30 0)
        , lastModified             = Just $ LocalTime (fromGregorian 2020 1 15) (TimeOfDay 12 30 0)
        , successResponseId        = Just 9
        , txnMode                  = Just "txnMode"
        , addToLocker              = Just True
        , merchantId               = Just "merchantId"
        , bankErrorCode            = Just "bankErrorCode"
        , bankErrorMessage         = Just "bankErrorMessage"
        , gateway                  = Just C.CYBERSOURCE
        , expressCheckout          = Just True
        , redirect                 = Just False
        , gatewayPayload           = Just "gatewayPayload"
        , isEmi                    = Just True
        , emiBank                  = Just "emiBank"
        , emiTenure                = Just 11
        , username                 = Just "username"
        , txnUuid                  = Just "txnUuid"
        , merchantGatewayAccountId = Just 34
        , txnAmount                = Just $ C.mkMoney 55
        , txnObjectType            = Just "txnObjectType"
        , sourceObject             = Just "sourceObject"
        , sourceObjectId           = Just "sourceObjectId"
        , currency                 = Just "currency"
        , netAmount                = Just $ C.mkMoney 22
        , surchargeAmount          = Just $ C.mkMoney 33
        , taxAmount                = Just $ C.mkMoney 44
        }
      )
    , payment_gateway_response = mMerchantPgr
    , gateway_id = Just 6
    , emi_bank = Just "emiBank"
    , emi_tenure = Just 11
    , gateway_reference_id = Just "JUSPAY:gateway_reference_id"
    , payer_vpa = Just "payer_vpa"
    , payer_app_name = Just "payer_app_name"
    , second_factor_response = secondFactorResponse
    , txn_flow_info = txnFlowInfo
    }
