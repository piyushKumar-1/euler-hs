{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeApplications          #-}

module Euler.Product.OLTP.Order.OrderStatus where



import           EulerHS.Prelude hiding (id, First, getFirst, Last, getLast)
import qualified EulerHS.Prelude as P (id)
import qualified Prelude as P (show)

import           Euler.Lens
import           EulerHS.Language
import           EulerHS.Types
import           WebService.Language

import           Data.Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import           Data.Char (toLower)
import           Data.Either.Extra
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import           Data.Time
import           Servant.Server
import           Control.Comonad hiding ((<<=))
import           Data.Semigroup as S
import           Control.Monad.Except

import           Euler.API.MerchantPaymentGatewayResponse
import           Euler.API.Order as AO
import           Euler.API.Refund
import           Euler.API.RouteParameters (RouteParameters (..), lookupRP)
import qualified Euler.API.RouteParameters as Param
import           Euler.API.Transaction
import           Euler.API.Types

import qualified Euler.Common.Metric as Metric
import qualified Euler.Common.Types as C
import           Euler.Common.Types.DefaultDate
import           Euler.Common.Types.Gateway
import           Euler.Common.Types.Mandate as Mandate
import           Euler.Common.Types.Merchant
import           Euler.Common.Types.Money
import           Euler.Common.Types.Order (OrderId, OrderTokenExpiryData (..),
                                           defaultOrderTokenExpiryData)
import qualified Euler.Common.Types.Order as C
import           Euler.Common.Types.PaymentGatewayResponseXml
import           Euler.Common.Types.Promotion
import           Euler.Common.Types.Refund as Refund
import           Euler.Common.Types.TxnDetail (TxnStatus (..), txnStatusToInt)
import           Euler.Common.Utils
import           Euler.Config.Config as Config

-- EHS: this dep should be moved somewhere. Additional business logic for KV DB
import qualified Euler.KVDB.Redis as KVDBExtra (rGet, setCacheWithExpiry)

import qualified Euler.Product.Domain as D

import           Euler.Product.OLTP.Card.Card
import           Euler.Product.OLTP.Services.AuthenticationService (extractApiKey, getMerchantId)

import qualified Euler.Storage.Types as DB

import           Euler.Storage.Repository

import           Euler.Storage.DBConfig

import           Euler.Services.Gateway.MerchantPaymentGatewayResponse
import           Euler.Services.Version.OrderStatusResponse

import           Database.Beam ((&&.), (/=.), (<-.), (==.))
import qualified Database.Beam as B
import qualified Database.Beam.Backend.SQL as B



myerr    n = err403 { errBody = "Err # " <> n }
myerr400 n = err400 { errBody = "Err # " <> n }


data FlowError = FlowError
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)


(<<=) :: Comonad w => w a -> (w a -> b) -> w b
(<<=) = (=>>)

type ResponseBuilder = OrderStatusResponseTemp -> OrderStatusResponse


type APIKey = Text

data FlowState = FlowState
  { merchantId :: Maybe Text
  , orderId    :: Text
  }
  deriving (Generic, Show, Eq, ToJSON, FromJSON )


data FlowStateOption = FlowStateOption
  deriving (Generic, Show, Eq, ToJSON, FromJSON )

instance OptionEntity FlowStateOption FlowState


type AuthToken = Text



handleByOrderId
  :: RouteParameters
  -> Param.OrderId
  -> D.MerchantAccount
  -> Flow (Either FlowError OrderStatusResponse)
handleByOrderId orderId rps merchantAccount  = do

  let query = OrderStatusQuery
        { orderId         = orderId
        , merchantId      = merchantAccount ^. _merchantId
        , resellerId      = merchantAccount ^. _resellerId
        , isAuthenticated = True
        , sendCardIsin    = fromMaybe False $ merchantAccount ^. _enableSendingCardIsin
        , txnId           = undefined :: Maybe Text
        , sendFullGatewayResponse = getSendFullGatewayResponse rps
        }

  response <- execOrderStatusQuery query

  pure $ mapLeft (const FlowError) response where


getSendFullGatewayResponse :: RouteParameters -> Bool
getSendFullGatewayResponse routeParams =
  -- EHS: magic constants
  case Map.lookup "options.add_full_gateway_response" (unRP routeParams) of
    Nothing  -> False
    Just str -> str == "1" || T.map toLower str == "true"


execOrderStatusQuery :: OrderStatusQuery -> Flow (Either Text OrderStatusResponse)
execOrderStatusQuery query = do

  let queryOrderId = query ^. _orderId
  let queryMerchantId = query ^. _merchantId
  let resellerId = query ^. _resellerId
  let sendFullGatewayResponse = query ^. _sendFullGatewayResponse

  mOrder <- loadOrder queryOrderId queryMerchantId

  (order :: D.Order) <- case mOrder of
    Just ord -> pure ord
    Nothing -> throwException err404
      { errBody = "Order not found "
      <> "OrderId" <> show queryOrderId
      <> " MerchantId" <> show queryMerchantId }

  let orderId = order ^. _orderId
  let merchantId = order ^. _merchantId
  let orderUuid = order ^. _orderUuid
  let orderType = order ^. _orderType
  let orderPId = order ^. _id
  let udf2 = (order ^. _udf) ^. _udf2

  links <- getPaymentLinks resellerId orderUuid

  mPromotion' <- getPromotion orderPId orderId
  mMandate' <- getMandate orderPId merchantId orderType

  mTxnDetail1 <- findTxnByOrderIdMerchantIdTxnuuidId orderId merchantId orderUuid
  mTxnDetail2 <- getLastTxn orderId merchantId
  let mTxn = (mTxnDetail1 <|> mTxnDetail2)

  gatewayRefId <- case mTxn of
    Nothing -> getGatewayReferenceId2 Nothing orderPId udf2 merchantId
    Just txn -> do
      let gateway = txn ^. _gateway
      getGatewayReferenceId2 gateway orderPId udf2 merchantId

  (mRisk, mTxnCard, mRefunds', mChargeback', mMerchantPgr) <- case mTxn of
    Nothing -> pure (Nothing, Nothing, Nothing, Nothing, Nothing)
    Just txn -> do
      let txnDetailPId = D.txnDetailPId $ txn ^. _id
      mRisk <- getRisk txnDetailPId
      mTxnCard <- loadTxnCardInfo txnDetailPId
      mRefunds' <- maybeList <$> (refundDetails txnDetailPId)
      mChargeback' <- maybeList <$> (chargebackDetails txnDetailPId $ mapTxnDetail txn)
      mMerchantPgr <- getMerchantPGR txn sendFullGatewayResponse
      pure (mRisk, mTxnCard, mRefunds', mChargeback', mMerchantPgr)

  mCardBrand <- case mTxnCard of
    Nothing                    -> pure Nothing
    Just (card :: D.TxnCardInfo) ->
      getCardBrandFromIsin (fromMaybe "" $ card ^. _cardIsin)

  returnUrl <- getReturnUrl order

  paymentMethodsAndTypes <- case (mTxn, mTxnCard) of
    (Just txn, Just txnCard) -> getPaymentMethodAndType txn txnCard
    _                        -> pure (Nothing, Nothing, Nothing, Nothing)

  txnFlowInfoAndMerchantSFR <- case (mTxn, mTxnCard) of
    (Just txn, Just txnCard) -> do
      let txnDetail_id = D.txnDetailPId $ txn ^. _id
      getTxnFlowInfoAndMerchantSFR txnDetail_id txnCard
    (Nothing, Nothing) -> pure (Nothing, Nothing)

  pure $ runExcept $ makeOrderStatusResponse
    order
    links
    mPromotion'
    mMandate'
    query
    mTxn
    gatewayRefId
    mRisk
    mTxnCard
    mCardBrand
    mRefunds'
    mChargeback'
    returnUrl
    paymentMethodsAndTypes
    txnFlowInfoAndMerchantSFR
    mMerchantPgr



emptyBuilder :: ResponseBuilder -> OrderStatusResponse
emptyBuilder builder = builder mempty


buildStatusResponse :: ResponseBuilder
buildStatusResponse OrderStatusResponseTemp{..} = OrderStatusResponse
  { id                        = fromMaybe T.empty $ fmap getFirst idT
  , merchant_id               = fmap getFirst merchant_idT
  , amount                    = whenNothing (fmap getLast amountT) (Just 0)
  , currency                  = fmap getLast currencyT
  , order_id                  = fmap getFirst order_idT
  , date_created              = fromMaybe T.empty $ fmap getLast date_createdT
  , return_url                = fmap getLast return_urlT
  , product_id                = fromMaybe T.empty $ fmap getLast product_idT
  , customer_email            = fmap getLast customer_emailT
  , customer_phone            = fmap getLast customer_phoneT
  , customer_id               = fmap getLast customer_idT
  , payment_links             = fromMaybe defaultPaymentlinks $ fmap getLast payment_linksT
  , udf1                      = fromMaybe "udf1" $ fmap getLast udf1T
  , udf2                      = fromMaybe "udf2" $ fmap getLast udf2T
  , udf3                      = fromMaybe "udf3" $ fmap getLast udf3T
  , udf4                      = fromMaybe "udf4" $ fmap getLast udf4T
  , udf5                      = fromMaybe "udf5" $ fmap getLast udf5T
  , udf6                      = fromMaybe "udf6" $ fmap getLast udf6T
  , udf7                      = fromMaybe "udf7" $ fmap getLast udf7T
  , udf8                      = fromMaybe "udf8" $ fmap getLast udf8T
  , udf9                      = fromMaybe "udf9" $ fmap getLast udf9T
  , udf10                     = fromMaybe "udf10" $ fmap getLast udf10T
  , txn_id                    = fmap getLast txn_idT
  , status_id                 = fromMaybe 0 $ fmap getLast status_idT
  , status                    = fromMaybe "DEFAULT" $ fmap getLast statusT
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
  , payment_gateway_response' = fmap getLast payment_gateway_responseT'
  , payment_gateway_response  = fmap getLast payment_gateway_responseT
  , gateway_id                = fmap getLast gateway_idT
  , emi_bank                  = fmap getLast emi_bankT
  , emi_tenure                = fmap getLast emi_tenureT
  , gateway_reference_id      = fmap getLast gateway_reference_idT
  , payer_vpa                 = fmap getLast payer_vpaT
  , payer_app_name            = fmap getLast payer_app_nameT
  , juspay                    = fmap getLast juspayT
  , second_factor_response    = fmap getLast second_factor_responseT
  , txn_flow_info             = fmap getLast txn_flow_infoT
  }


makeOrderStatusResponse
  :: D.Order
  -> Paymentlinks
  -> Maybe Promotion'
  -> Maybe Mandate'
  -> OrderStatusQuery
  -> Maybe D.TxnDetail
  -> Text
  -> Maybe Risk
  -> Maybe D.TxnCardInfo
  -> Maybe Text
  -> Maybe [Refund']
  -> Maybe [Chargeback']
  -> Text
  -> (Maybe Text, Maybe Text, Maybe Text, Maybe Text)
  -> (Maybe TxnFlowInfo, Maybe MerchantSecondFactorResponse)
  -> Maybe MerchantPaymentGatewayResponse
  -> Except Text OrderStatusResponse
makeOrderStatusResponse
  order
  paymentLinks
  mPromotion
  mMandate
  query@OrderStatusQuery{..}
  mTxn
  gatewayRefId
  mRisk
  mTxnCard
  mCardBrand
  mRefunds
  mChargebacks
  returnUrl
  paymentMethodsAndTypes
  (txnFlowInfo, merchantSFR)
  mMerchantPgr
  = do

  let ordId = order ^. _orderUuid

  let mCustomerId = whenNothing  (order ^. _customerId) (Just "")
      email = (\email -> if isAuthenticated then email else Just "")  (order ^. _customerEmail)
      phone = (\phone -> if isAuthenticated then phone else Just "")  (order ^. _customerPhone)
      amount = fromMoney $ order ^. _amount
      amountRefunded = fmap sanitizeAmount $ order ^. _amountRefunded

      getStatus = show . (^. _status)
      getStatusId = txnStatusToInt . (^. _status)
      getGatewayId txn = maybe 0 gatewayIdFromGateway $ txn ^. _gateway
      getBankErrorCode txn = whenNothing  (txn ^. _bankErrorCode) (Just "")
      getBankErrorMessage txn = whenNothing  (txn ^. _bankErrorMessage) (Just "")
      getGatewayPayload txn = if isBlankMaybe (mGatewayPayload' txn) then (mGatewayPayload' txn) else Nothing
        where mGatewayPayload' t = t ^. _gatewayPayload
      currency = show $ order ^. _currency

      isEmi txn = isTrueMaybe  (txn ^. _isEmi)
      emiTenure txn = txn ^. _emiTenure
      emiBank txn = txn ^. _emiBank

      paymentMethod = whenNothing mCardBrand (Just "UNKNOWN")

      maybeTxnCard f = maybe emptyBuilder f mTxnCard
      maybeTxn f = maybe emptyBuilder f mTxn
      maybeTxnAndTxnCard f = case (mTxn, mTxnCard) of
        (Just txn, Just txnCard) -> f txn txnCard
        _                        -> emptyBuilder


  pure $ extract $ buildStatusResponse

    <<= changeMerchantPGR mMerchantPgr
    -- addGatewayResponse

    <<= changeChargeBacks mChargebacks

    <<= changeRefund mRefunds


    <<= changeMerchantSecondFactorResponse merchantSFR
    <<= changeTxnFlowInfo txnFlowInfo
      -- addSecondFactorResponseAndTxnFlowInfo

    <<= maybeTxnAndTxnCard (\txn txnCard -> if isBlankMaybe (txnCard ^. _cardIsin)
        then changeCard (getCardDetails txnCard txn sendCardIsin)
        else emptyBuilder)

    <<= maybeTxnAndTxnCard (\_ txnCard -> if isBlankMaybe (txnCard ^. _cardIsin)
        then changePaymentMethodType "CARD"
        else emptyBuilder)

    <<= maybeTxnAndTxnCard (\_ txnCard -> if isBlankMaybe (txnCard ^. _cardIsin)
        then changeEmiPaymentMethod paymentMethod
        else emptyBuilder)

    <<= maybeTxnAndTxnCard (\_ txnCard -> changeAuthType $ whenNothing (txnCard ^. _authType) (Just ""))
    <<= maybeTxnAndTxnCard (\txn _ -> if isEmi txn then changeEmiTenureEmiBank (emiTenure txn) (emiBank txn) else emptyBuilder)

    <<= maybeTxnAndTxnCard (\_ _ -> changePaymentMethodAndTypeAndVpa paymentMethodsAndTypes)

    <<= changeRisk mRisk

    <<= maybeTxn (changeTxnDetails . mapTxnDetail)
    <<= maybeTxn (changeGatewayPayload . getGatewayPayload)
    <<= maybeTxn (changeBankErrorMessage . getBankErrorMessage)
    <<= maybeTxn (changeBankErrorCode . getBankErrorCode)
    <<= maybeTxn (const $ changeGatewayRefId gatewayRefId)
    <<= maybeTxn (changeGatewayId . getGatewayId)
    <<= maybeTxn (changeTxnUuid . (^. _txnUuid))
    <<= maybeTxn (changeTxnId . (^. _txnId))
    <<= maybeTxn (changeStatusId . getStatusId)
    <<= maybeTxn (changeStatus . getStatus)

    <<= changeMandate mMandate

    <<= changeAmountAfterPromotion mPromotion

    <<= changePromotion mPromotion

    <<= changeUtf10 ((order ^. _udf) ^. _udf10)
    <<= changeUtf9 ((order ^. _udf) ^. _udf9)
    <<= changeUtf8 ((order ^. _udf) ^. _udf8)
    <<= changeUtf7 ((order ^. _udf) ^. _udf7)
    <<= changeUtf6 ((order ^. _udf) ^. _udf6)
    <<= changeUtf5 ((order ^. _udf) ^. _udf5)
    <<= changeUtf4 ((order ^. _udf) ^. _udf4)
    <<= changeUtf3 ((order ^. _udf) ^. _udf3)
    <<= changeUtf2 ((order ^. _udf) ^. _udf2)
    <<= changeUtf1 ((order ^. _udf) ^. _udf1)
    <<= changeReturnUrl returnUrl
    <<= changeCustomerPhone phone
    <<= changeCustomerEmail email
    <<= changeDateCreated (show $ order ^. _dateCreated)
    <<= changeAmountRefunded amountRefunded
    <<= changePaymentLinks paymentLinks
    <<= changeRefunded (order ^. _refundedEntirely)
    <<= changeCurrency currency
    <<= changeAmount amount
    <<= changeStatus (show $ order ^. _orderStatus)
    <<= changeProductId (order ^. _productId)
    <<= changeCustomerId mCustomerId
    <<= changeOrderId (order ^. _orderId)
    <<= changeMerchantId (order ^. _merchantId)
    <<= changeId ordId


changeId :: Text -> ResponseBuilder -> OrderStatusResponse
changeId orderId builder = builder $ mempty {idT = Just $ First orderId}

changeMerchantId :: Text -> ResponseBuilder -> OrderStatusResponse
changeMerchantId mMerchantId builder = builder $ mempty {merchant_idT = Just $ First mMerchantId}

changeAmount :: Double -> ResponseBuilder -> OrderStatusResponse
changeAmount amount builder = builder $ mempty {amountT = Just $ Last amount}

changeOrderId :: Text -> ResponseBuilder -> OrderStatusResponse
changeOrderId orderId builder = builder $ mempty {order_idT = Just $ First orderId}

changeCustomerId :: Maybe Text -> ResponseBuilder -> OrderStatusResponse
changeCustomerId customerId builder = builder $ mempty {customer_idT = fmap Last customerId}

changeProductId :: Maybe Text -> ResponseBuilder -> OrderStatusResponse
changeProductId productId builder = builder $ mempty {product_idT = fmap Last productId}

changeStatus :: Text -> ResponseBuilder -> OrderStatusResponse
changeStatus status builder = builder $ mempty {statusT = Just $ Last status}

changeCurrency :: Text -> ResponseBuilder -> OrderStatusResponse
changeCurrency currency builder = builder $ mempty {currencyT = Just $ Last currency}

changeRefunded :: Bool -> ResponseBuilder -> OrderStatusResponse
changeRefunded refunded builder = builder $ mempty {refundedT = Just $ Last refunded}

changePaymentLinks :: Paymentlinks -> ResponseBuilder -> OrderStatusResponse
changePaymentLinks paymentLinks builder = builder $ mempty {payment_linksT = Just $ Last paymentLinks}

changeAmountRefunded :: Maybe Double -> ResponseBuilder -> OrderStatusResponse
changeAmountRefunded amountRefunded builder = builder $ mempty {amount_refundedT = fmap Last amountRefunded}

changeDateCreated :: Text -> ResponseBuilder -> OrderStatusResponse
changeDateCreated dateCreated builder = builder $ mempty {date_createdT = Just $ Last dateCreated}

changeCustomerEmail :: Maybe Text -> ResponseBuilder -> OrderStatusResponse
changeCustomerEmail customerEmail builder = builder $ mempty {customer_emailT = map Last customerEmail}

changeCustomerPhone :: Maybe Text -> ResponseBuilder -> OrderStatusResponse
changeCustomerPhone customerPhone builder = builder $ mempty {customer_phoneT = map Last customerPhone}

changeReturnUrl :: Text -> ResponseBuilder -> OrderStatusResponse
changeReturnUrl returnUrl builder = builder $ mempty {return_urlT = Just $ Last returnUrl}

changeUtf1 :: Maybe Text -> ResponseBuilder -> OrderStatusResponse
changeUtf1 utf1 builder = builder $ mempty {udf1T = map Last utf1}

changeUtf2 :: Maybe Text -> ResponseBuilder -> OrderStatusResponse
changeUtf2 utf2 builder = builder $ mempty {udf2T = map Last utf2}

changeUtf3 :: Maybe Text -> ResponseBuilder -> OrderStatusResponse
changeUtf3 utf3 builder = builder $ mempty {udf3T = map Last utf3}

changeUtf4 :: Maybe Text -> ResponseBuilder -> OrderStatusResponse
changeUtf4 utf4 builder = builder $ mempty {udf4T = map Last utf4}

changeUtf5 :: Maybe Text -> ResponseBuilder -> OrderStatusResponse
changeUtf5 utf5 builder = builder $ mempty {udf5T = map Last utf5}

changeUtf6 :: Maybe Text -> ResponseBuilder -> OrderStatusResponse
changeUtf6 utf6 builder = builder $ mempty {udf6T = map Last utf6}

changeUtf7 :: Maybe Text -> ResponseBuilder -> OrderStatusResponse
changeUtf7 utf7 builder = builder $ mempty {udf7T = map Last utf7}

changeUtf8 :: Maybe Text -> ResponseBuilder -> OrderStatusResponse
changeUtf8 utf8 builder = builder $ mempty {udf8T = map Last utf8}

changeUtf9 :: Maybe Text -> ResponseBuilder -> OrderStatusResponse
changeUtf9 utf9 builder = builder $ mempty {udf9T = map Last utf9}

changeUtf10 :: Maybe Text -> ResponseBuilder -> OrderStatusResponse
changeUtf10 utf10 builder = builder $ mempty {udf10T = map Last utf10}


changePromotion :: Maybe Promotion' -> ResponseBuilder -> OrderStatusResponse
changePromotion Nothing builder = builder mempty
changePromotion mNewProm builder = builder mempty { promotionT = fmap Last mNewProm }

changeAmountAfterPromotion :: Maybe Promotion' -> ResponseBuilder -> OrderStatusResponse
changeAmountAfterPromotion Nothing builder = builder mempty
changeAmountAfterPromotion (Just newProm) builder =
  let oldStatus = extract builder
      mOldAmount = oldStatus ^. _amount
      mOldPromotion = newProm ^. _discount_amount
      newAmount = sanitizeAmount $ (fromMaybe 0 mOldAmount) + (fromMaybe 0 mOldPromotion)
  in builder mempty { amountT = Just $ Last newAmount }


changeMandate :: Maybe Mandate' -> ResponseBuilder -> OrderStatusResponse
changeMandate mandate builder = builder $ mempty { mandateT = fmap Last mandate}


changeStatusId :: Int -> ResponseBuilder -> OrderStatusResponse
changeStatusId statusId builder = builder $ mempty {status_idT = Just $ Last statusId}

changeTxnId :: Text -> ResponseBuilder -> OrderStatusResponse
changeTxnId txnId builder = builder $ mempty {txn_idT = Just $ Last txnId}

changeTxnUuid :: Maybe Text -> ResponseBuilder -> OrderStatusResponse
changeTxnUuid txnUuid builder = builder $ mempty {txn_uuidT = fmap Last txnUuid}

changeGatewayId :: Int -> ResponseBuilder -> OrderStatusResponse
changeGatewayId gatewayId builder = builder $ mempty {gateway_idT = Just $ Last gatewayId}

changeGatewayRefId :: Text -> ResponseBuilder -> OrderStatusResponse
changeGatewayRefId gatewayRefId builder = builder $ mempty {gateway_reference_idT = Just $ Last gatewayRefId}

changeBankErrorCode :: Maybe Text -> ResponseBuilder -> OrderStatusResponse
changeBankErrorCode bankErrorCode builder = builder $ mempty {bank_error_codeT = fmap Last bankErrorCode}

changeBankErrorMessage :: Maybe Text -> ResponseBuilder -> OrderStatusResponse
changeBankErrorMessage bankErrorMessage builder = builder $ mempty {bank_error_messageT = fmap Last bankErrorMessage}

changeGatewayPayload :: Maybe Text -> ResponseBuilder -> OrderStatusResponse
changeGatewayPayload gatewayPayload builder = builder $ mempty {gateway_payloadT = fmap Last gatewayPayload}

changeTxnDetails :: TxnDetail' -> ResponseBuilder -> OrderStatusResponse
changeTxnDetails txnDetail builder = builder $ mempty {txn_detailT = Just $ Last txnDetail}


changeRisk :: Maybe Risk -> ResponseBuilder -> OrderStatusResponse
changeRisk risk builder = builder $ mempty {riskT = fmap Last risk}


changeEmiTenureEmiBank :: Maybe Int -> Maybe Text -> ResponseBuilder -> OrderStatusResponse
changeEmiTenureEmiBank emiTenure emiBank builder = builder $ mempty
  {emi_tenureT = map Last emiTenure
  , emi_bankT = map Last emiBank
  }

changeAuthType :: Maybe Text -> ResponseBuilder -> OrderStatusResponse
changeAuthType authType builder = builder $ mempty {auth_typeT = fmap Last authType}

changeEmiPaymentMethod :: Maybe Text -> ResponseBuilder -> OrderStatusResponse
changeEmiPaymentMethod paymentMethod builder = builder $ mempty {payment_methodT = fmap Last paymentMethod}

changePaymentMethodType :: Text -> ResponseBuilder -> OrderStatusResponse
changePaymentMethodType paymentMethodType builder =
  builder $ mempty {payment_method_typeT = Just $ Last paymentMethodType}

changeCard :: Card -> ResponseBuilder -> OrderStatusResponse
changeCard card builder = builder $ mempty {cardT = Just $ Last card}


changeRefund :: Maybe [Refund'] -> ResponseBuilder -> OrderStatusResponse
changeRefund mRefunds builder = builder $ mempty {refundsT = fmap Last mRefunds}


changeChargeBacks :: Maybe [Chargeback'] -> ResponseBuilder -> OrderStatusResponse
changeChargeBacks mChargebacks builder = builder $ mempty {chargebacksT = fmap Last mChargebacks}


changePaymentMethodAndTypeAndVpa
  :: (Maybe Text, Maybe Text, Maybe Text, Maybe Text)
  -> ResponseBuilder
  -> OrderStatusResponse
changePaymentMethodAndTypeAndVpa (mPaymentMethod, mPaymentMethodType, mPayerVpa, mPayerAppName) builder =
  builder $ mempty
    { payment_methodT = fmap Last mPaymentMethod
    , payment_method_typeT = fmap Last mPaymentMethodType
    , payer_vpaT = fmap Last mPayerVpa
    , payer_app_nameT = fmap Last mPayerAppName
    }

changeTxnFlowInfo :: Maybe TxnFlowInfo -> ResponseBuilder -> OrderStatusResponse
changeTxnFlowInfo mkTxnFlowInfo builder = builder $ mempty {txn_flow_infoT = fmap Last mkTxnFlowInfo}

changeMerchantSecondFactorResponse
  :: Maybe MerchantSecondFactorResponse
  -> ResponseBuilder
  -> OrderStatusResponse
changeMerchantSecondFactorResponse mMerchantSFR builder =
  builder $ mempty {second_factor_responseT = map Last mMerchantSFR}

changeMerchantPGR :: Maybe MerchantPaymentGatewayResponse -> ResponseBuilder -> OrderStatusResponse
changeMerchantPGR mMerchantPgr builder =
  builder $ mempty {payment_gateway_responseT = map Last mMerchantPgr}

-- EHS: TODO add sorting by dateCreated!
getLastTxn :: C.OrderId -> C.MerchantId -> Flow (Maybe D.TxnDetail)
getLastTxn orderId merchantId = do

  txnDetails <- findTxnByOrderIdMerchantId orderId merchantId

  case txnDetails of
    [] -> do
      logError "get_last_txn"
        $ "No last txn found for orderId: " <> show orderId
        <> " :merchant:" <> show merchantId
      pure Nothing
    _ -> do
      let chargetxn = find (\txn -> (txn ^. _status == CHARGED)) txnDetails
      maybe (pure . Just $ head txnDetails) (pure . Just) chargetxn


getMandate :: C.OrderPId -> C.MerchantId -> C.OrderType -> Flow (Maybe Mandate')
getMandate orderPId merchantId = \case
    C.MANDATE_REGISTER -> do
      mandate <- loadMandate orderPId merchantId
      pure $ mapMandate <$> mandate
    _ -> pure Nothing


getPaymentLinks :: Maybe Text -> Text ->  Flow Paymentlinks
getPaymentLinks resellerId orderUuid = do
  mResellerAccount <- loadReseller resellerId
  let mResellerEndpoint = maybe Nothing (^. _resellerApiEndpoint) mResellerAccount
  pure $ createPaymentLinks orderUuid mResellerEndpoint


createPaymentLinks
  :: Text
  -> Maybe Text
  -> Paymentlinks
createPaymentLinks orderUuid maybeResellerEndpoint =
  Paymentlinks
    { web =   Just (host <> "/merchant/pay/") <> Just orderUuid
    , mobile =   Just (host <> "/merchant/pay/") <> Just orderUuid <> Just "?mobile=true"
    , iframe =   Just (host <> "/merchant/ipay/") <> Just orderUuid
    }
  where
    config = defaultConfig
    protocol = config ^. _protocol
    host = maybe (protocol <> "://" <> (config ^. _host)) P.id maybeResellerEndpoint

getReturnUrl ::  D.Order -> Flow Text
getReturnUrl order = do
  let merchIdOrder = order ^. _merchantId
  merchantAccount <- loadMerchantByMerchantId merchIdOrder
  case merchantAccount of
    Nothing -> pure ""
    Just merchantAcc -> do
      merchantIframePreferences <-
        let merchIdMA = merchantAcc ^. _merchantId
        case merchId of
          Nothing -> pure Nothing
          Just merchantId -> loadMerchantPrefs merchantId
      let merchantIframeReturnUrl = fromMaybe "" ((^. _returnUrl) =<< merchantIframePreferences)
          orderRefReturnUrl       = fromMaybe "" (order ^. _returnUrl)
      if (orderRefReturnUrl == "")
        then pure $ fromMaybe merchantIframeReturnUrl (merchantAcc ^. _returnUrl )
        else pure orderRefReturnUrl


getPromotion :: C.OrderPId -> C.OrderId -> Flow (Maybe Promotion')
getPromotion orderPId orderId = do
  proms <- loadPromotions orderPId
  decryptActivePromotion orderId proms


mapTxnDetail :: D.TxnDetail -> TxnDetail'
mapTxnDetail txn = TxnDetail'
  { txn_id = txn ^. _txnId
  , order_id = txn ^. _orderId
  , txn_uuid = txn ^. _txnUuid
  , gateway_id = Just $ maybe 0 gatewayIdFromGateway $ txn ^. _gateway
  , status = show $ txn ^. _status
  , gateway = show <$> txn ^. _gateway
  , express_checkout = txn ^. _expressCheckout
  , redirect = txn ^. _redirect
  , net_amount = Just $ if isJust (txn ^. _netAmount)
      then show $ maybe 0 fromMoney (txn ^. _netAmount) -- Forign becomes Text in our TxnDetail'
      else mempty
  , surcharge_amount = Just $ if isJust (txn ^. _surchargeAmount)
      then show $ maybe 0 fromMoney (txn ^. _surchargeAmount)
      else mempty
  , tax_amount = Just $ if isJust (txn ^. _taxAmount)
      then show $ maybe 0 fromMoney (txn ^. _taxAmount)
      else mempty
  , txn_amount = Just $ if isJust (txn ^. _txnAmount)
      then show $ maybe 0 fromMoney (txn ^. _txnAmount)
      else mempty
  , currency = txn ^. _currency
  , error_message = Just $ fromMaybe mempty $ txn ^. _bankErrorMessage
  , error_code = Just $ if isJust (txn ^. _bankErrorCode)
      then fromMaybe mempty (txn ^. _bankErrorCode)
      else mempty
  , created = txn ^. _dateCreated
  , txn_object_type = if (fromMaybe mempty $ txn ^. _txnObjectType) /= "ORDER_PAYMENT"
      then txn ^. _txnObjectType
      else Nothing
  , source_object = if (fromMaybe mempty $ txn ^. _txnObjectType) /= "ORDER_PAYMENT"
      then txn ^. _sourceObject
      else Nothing
  , source_object_id = if (fromMaybe mempty $ txn ^. _txnObjectType) /= "ORDER_PAYMENT"
      then txn ^. _sourceObjectId
      else Nothing
  }


loadOrderMetadataV2 :: Int -> Flow (Maybe DB.OrderMetadataV2)
loadOrderMetadataV2 ordRefId = withDB eulerDB $ do
  let predicate DB.OrderMetadataV2 {orderReferenceId} =
        orderReferenceId ==. B.val_ ordRefId
  findRow
    $ B.select
    $ B.limit_ 1
    $ B.filter_ predicate
    $ B.all_ (DB.order_metadata_v2 DB.eulerDBSchema)


getGatewayReferenceId2
  :: Maybe Gateway
  -> C.OrderPId
  -> Maybe Text -- udf2
  -> C.MerchantId
  -> Flow Text
getGatewayReferenceId2 gateway orderPId udf2 merchantId = do
  let checkGateway = checkGatewayRefIdForVodafone2 merchantId udf2 gateway
  let gatewayT = maybe "" show gateway

  ordMeta <- loadOrderMetadataV2 orderPId

  case ordMeta of
    Just (ordM :: DB.OrderMetadataV2) ->
      case blankToNothing (ordM ^. _metadata) of
        Nothing -> checkGateway
        Just md -> do
          let md' = decode $ BSL.fromStrict $ T.encodeUtf8 md :: Maybe (Map Text Text)
          case md' of
            Nothing -> checkGateway
            Just metadata -> do
              gRefId <- pure $ Map.lookup (gatewayT <> ":gateway_reference_id") metadata
              jusId  <- pure $ Map.lookup "JUSPAY:gateway_reference_id" metadata
              case (gRefId <|> jusId) of
                Just v  -> pure v
                Nothing -> checkGateway


checkGatewayRefIdForVodafone2
  :: C.MerchantId
  -> Maybe Text
  -> Maybe Gateway
  -> Flow Text
checkGatewayRefIdForVodafone2 merchantId' udf2 gateway = do

  meybeFeature <- withDB eulerDB $ do
    let predicate DB.Feature {name, merchantId} =
          name ==. B.val_ "USE_UDF2_FOR_GATEWAY_REFERENCE_ID"
          &&. merchantId ==. B.just_ (B.val_ merchantId')
    findRow
      $ B.select
      $ B.limit_ 1
      $ B.filter_ predicate
      $ B.all_ (DB.feature DB.eulerDBSchema)

  case meybeFeature of
    Just feature ->
        if (gateway == Just HSBC_UPI)
            && (feature ^. _enabled)
            && (isJust udf2)
        then pure $ fromMaybe "" udf2
        else pure mempty
    Nothing -> pure mempty


makeRisk :: Risk' -> D.TxnRiskCheck -> Risk
makeRisk risk' trc = if risk' ^. _provider == Just "ebs"
  then case C.decodeRMSIDResult completeResponse of
    Left _ -> mapRisk trc $ risk' & _ebs_payment_status .~ (trc ^. _riskStatus)
    Right rmsidResult -> mapRisk trc risk'
      { ebs_risk_level = Just $ rmsidResult ^. _riskLevel
      , ebs_payment_status = trc ^. _riskStatus
      , ebs_bin_country = Just $ (rmsidResult ^. _output) ^. _bincountry
      , ebs_risk_percentage = readMaybe $ T.unpack $ rmsidResult ^. _riskPercentage
      }
  else mapRisk trc risk'
  where
    completeResponse = T.encodeUtf8 $ trc ^. _completeResponse

mapRisk :: D.TxnRiskCheck -> Risk' -> Risk
mapRisk trc risk' = case provider of
  Just "ebs" -> risk
    { ebs_risk_level = risk' ^. _ebs_risk_level
    , ebs_payment_status = risk' ^. _ebs_payment_status
    , ebs_bin_country = risk' ^. _ebs_bin_country
    , ebs_risk_percentage = Just $ maybe "0" show $ risk' ^.  _ebs_risk_percentage
    }
  _ -> risk
  where
    provider = risk' ^. _provider
    risk = Risk
      { provider = provider
      , status = risk' ^. _status
      , message = risk' ^. _message
      , flagged = show <$> whenNothing (trc ^. _flagged) (Just False)
      , recommended_action = risk' ^. _recommended_action
      , ebs_risk_level = Nothing
      , ebs_payment_status = Nothing
      , ebs_risk_percentage = Nothing
      , ebs_bin_country = Nothing
      }


getRisk :: Int -> Flow (Maybe Risk)
getRisk txnId = do
  txnRiskCheck <- loadTxnRiskCheck txnId
  case txnRiskCheck of
    Nothing -> pure Nothing
    Just trc -> do
      let riskMAId = trc ^. _riskManagementAccountId
      riskMngAcc <- loadRiskManagementAccount riskMAId
      let risk' = makeRisk' ((^. _provider) <$> riskMngAcc) trc
      pure $ Just $ makeRisk risk' trc


refundDetails :: Int -> Flow [Refund']
refundDetails txnId = do
  l <- findRefunds txnId
  pure $ map mapRefund l


chargebackDetails :: Int -> TxnDetail' -> Flow [Chargeback']
chargebackDetails txnId txn = do
  chargebacks <- findChargebacks  txnId
  pure $ map (AO.mapChargeback txn) chargebacks


sanitizeAmount x = x
sanitizeNullAmount = fmap sanitizeAmount


getPaymentMethodAndType
  :: D.TxnDetail
  -> D.TxnCardInfo
  -> Flow (Maybe Text, Maybe Text, Maybe Text, Maybe Text)
  -- ^ Result is (payment_method, payment_method_type, payer_vpa, payer_app_name)
  -- when Nothing do not change a field
getPaymentMethodAndType txn card = do
  case (card ^. _cardType) of
    Just "NB" -> pure
      ( whenNothing (card ^. _cardIssuerBankName) (Just T.empty)
      , Just "NB"
      , Nothing
      , Nothing
      )
    Just "WALLET" -> do
      payerVpa <- case card ^. _paymentMethod of
        Nothing          -> pure ""
        Just "GOOGLEPAY" -> getPayerVpa $ txn ^. _successResponseId
        Just _           -> pure ""
      pure
        ( whenNothing (card ^. _cardIssuerBankName) (Just T.empty)
        , Just "WALLET"
        , Just payerVpa
        , Nothing
        )

    Just "UPI" -> do
      let payment_method = Just "UPI"
          payment_method_type = Just "UPI"
          paymentSource = if (fromMaybe "null" $ card ^. _paymentSource) == "null"
            then Just ""
            else whenNothing (card ^. _paymentSource) (Just "null")
          sourceObj = fromMaybe "" $ txn ^. _sourceObject
      if sourceObj == "UPI_COLLECT" || sourceObj == "upi_collect"
        then pure
          ( payment_method
          , payment_method_type
          , paymentSource
          , Nothing
          )
        else do
          let respId = txn ^. _successResponseId
          let gateway = txn ^. _gateway
          payervpa <- getPayerVpaByGateway respId gateway
          case payervpa of
            "" -> pure
              ( payment_method
              , payment_method_type
              , paymentSource
              , paymentSource
              )
            _ -> pure
              ( payment_method
              , payment_method_type
              , Just payervpa
              , paymentSource
              )

    Just "PAYLATER" -> pure
        ( Just "JUSPAY"
        , Just "PAYLATER"
        , Nothing
        , Nothing
        )
    Just "CARD" -> pure
      ( whenNothing (card ^. _cardIssuerBankName) (Just T.empty)
      , Just "CARD"
      , Nothing
      , Nothing
      )
    Just "REWARD" -> pure
      ( whenNothing (card ^. _cardIssuerBankName) (Just T.empty)
      , Just "REWARD"
      , Nothing
      , Nothing
      )
    Just "ATM_CARD" -> pure
      ( whenNothing (card ^. _cardIssuerBankName) (Just T.empty)
      , Just "CARD"
      , Nothing
      , Nothing
      )
    Just _ -> checkPaymentMethodType card
    Nothing -> checkPaymentMethodType card

  where
    checkPaymentMethodType card' = case (card' ^. _paymentMethodType) of
      Just Mandate.CASH -> pure
        ( whenNothing (card' ^. _paymentMethod) (Just T.empty)
        , Just "CASH"
        , Nothing
        , Nothing
        )
      Just Mandate.CONSUMER_FINANCE -> pure
        ( whenNothing (card' ^. _paymentMethod) (Just T.empty)
        , Just "CONSUMER_FINANCE"
        , Nothing
        , Nothing
        )
      _ -> pure (Nothing, Nothing, Nothing, Nothing)

getPayerVpa :: Maybe Int -> Flow Text
getPayerVpa mSuccessResponseId = do
  mPaymentGatewayResp <- loadPGR mSuccessResponseId
  let mXml = (^. _responseXml) =<< mPaymentGatewayResp
  pure $ case mXml of
    Nothing  -> T.empty
    Just xml -> findEntry "payerVpa" "" $ decodePGRXml $ T.encodeUtf8 xml

getPayerVpaByGateway :: Maybe Int -> Maybe Gateway -> Flow Text
getPayerVpaByGateway respId gateway = do
  mPgr <- loadPGR respId
  case mPgr of
    Nothing  -> pure T.empty
    Just pgr -> pure $ findPayerVpaByGateway gateway (pgr ^. _responseXml)

findPayerVpaByGateway :: Maybe Gateway -> Maybe Text -> Text
findPayerVpaByGateway _ Nothing = T.empty
findPayerVpaByGateway gateway (Just xml) =
  case gateway of
    Nothing -> T.empty
    Just gateway' -> case gateway' of
      AXIS_UPI    -> findEntry "payerVpa" (findEntry "customerVpa" "" pgrXml) pgrXml
      HDFC_UPI    -> findEntry "payerVpa" "" pgrXml
      INDUS_UPI   -> findEntry "payerVpa" "" pgrXml
      KOTAK_UPI   -> findEntry "payerVpa" "" pgrXml
      SBI_UPI     -> findEntry "payerVpa" "" pgrXml
      ICICI_UPI   -> findEntry "payerVpa" "" pgrXml
      HSBC_UPI    -> findEntry "payerVpa" "" pgrXml
      VIJAYA_UPI  -> findEntry "payerVpa" "" pgrXml
      YESBANK_UPI -> findEntry "payerVpa" "" pgrXml
      PAYTM_UPI   -> findEntry "payerVpa" "" pgrXml
      PAYU        -> findEntry "field3" "" pgrXml
      RAZORPAY    -> findEntry "vpa" "" pgrXml
      PAYTM_V2    -> findEntry "VPA" "" pgrXml
      GOCASHFREE  -> findEntry "payersVPA" "" pgrXml
      _           -> T.empty
  where
    pgrXml = decodePGRXml $ T.encodeUtf8 xml

getTxnFlowInfoAndMerchantSFR
  :: Int
  -> D.TxnCardInfo
  -> Flow (Maybe TxnFlowInfo, Maybe MerchantSecondFactorResponse)
getTxnFlowInfoAndMerchantSFR txnDetId card = do

  if (fromMaybe T.empty $ card ^. _authType) == "VIES" then do

    mSecondFactor <- findSecondFactor txnDetId

    case mSecondFactor of
      Nothing ->  pure (Nothing, Nothing)
      Just sf -> do
        authReqParams <- do
          let authReqParams = fromMaybe "{}" $  sf ^. _gatewayAuthReqParams
          let vies = (decode $ BSL.fromStrict $ T.encodeUtf8 authReqParams)
          case vies of
            Just v -> pure v
            Nothing -> throwException err500
              {errBody = "AuthReqParams decoding failed"}

        let txnFlowInfo = mkTxnFlowInfo authReqParams

        mSecondFactorResponse <- findSecondFactorResponse $ D.sfPId $ sf ^. _id

        let mMerchantSFR = mkMerchantSecondFactorResponse <$> mSecondFactorResponse
        pure (Just txnFlowInfo, mMerchantSFR)
    else pure (Nothing, Nothing) --NON VIES Txn


getMerchantPGR :: D.TxnDetail -> Bool -> Flow (Maybe MerchantPaymentGatewayResponse)
getMerchantPGR txn shouldSendFullGatewayResponse = do

  -- EHS: TODO OS: We need to have this clarified with Sushobhith
  -- they introduces PGRV1 in early 2020

  mPaymentGatewayResp <- loadPGR $ txn ^. _successResponseId

  case mPaymentGatewayResp of
    Nothing -> pure Nothing
    Just pgr -> do
      let gateway = txn ^. _gateway
      let pgrXml  = case pgr ^. _responseXml of
            Nothing  -> Map.empty
            Just xml -> getMapFromPGRXml $ decodePGRXml $ T.encodeUtf8 xml
      let date = show <$> pgr ^. _dateCreated
      let mPgr = defaultMerchantPaymentGatewayResponse' {created = date} :: MerchantPaymentGatewayResponse'
      let gateway' = maybe "" show gateway
      let merchantPgr' = transformMpgrByGateway mPgr pgrXml $ mkMerchantPGRServiceTemp gateway' txn pgr pgrXml
      let gatewayResponse = getGatewayResponseInJson pgr shouldSendFullGatewayResponse
      let merchantPgr = makeMerchantPaymentGatewayResponse gatewayResponse merchantPgr'
      pure $ Just merchantPgr

getGatewayResponseInJson
  :: DB.PaymentGatewayResponse
  -> Bool
  -> Maybe Text
getGatewayResponseInJson paymentGatewayResponse shouldSendFullGatewayResponse =
  if shouldSendFullGatewayResponse
    then
      let xmlResp = fromMaybe T.empty $ paymentGatewayResponse ^. _responseXml
          pgrXml = decodePGRXml $ T.encodeUtf8 xmlResp

      -- EHS: it need review and need authentic json data to check
      -- jsonPgr <- createJsonFromPGRXmlResponse $ getMapFromPGRXml pgrXml
      -- jsonPgr <- encode $ getMapFromPGRXml pgrXml
      in Just $ toStrict $ TL.decodeUtf8 $ encode $ getMapFromPGRXml pgrXml
    else Nothing

