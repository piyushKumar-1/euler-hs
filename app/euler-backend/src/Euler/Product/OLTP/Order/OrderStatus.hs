{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeApplications          #-}

module Euler.Product.OLTP.Order.OrderStatus where



import           EulerHS.Prelude hiding (First, Last, getFirst, getLast, id)
import qualified EulerHS.Prelude as P (id)

import qualified Euler.Encryption as E
import           Euler.Lens
import           EulerHS.Language
import           EulerHS.Types

import           Control.Comonad hiding ((<<=))
import           Control.Monad.Except
import           Data.Aeson
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as LC
import           Data.Char (toLower)
import           Data.Either.Extra
import qualified Data.Map.Strict as Map
import           Data.Semigroup as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy.Encoding as TL
import           Servant.Server
import qualified Data.Aeson as A

import           Euler.API.MerchantPaymentGatewayResponse
import           Euler.API.Order as AO
import           Euler.API.Refund
import           Euler.API.RouteParameters (RouteParameters (..))
import qualified Euler.API.RouteParameters as Param


import qualified Euler.Constant.Feature as FeatureC

import           Euler.Common.Types.External.Mandate as Mandate
import qualified Euler.Common.Types                           as C
import           Euler.Common.Types.PaymentGatewayResponseXml
import           Euler.Common.Types.TxnDetail                 (TxnStatus (..), txnStatusToInt)

import           Euler.Common.Utils
import           Euler.Config.Config as Config

import qualified Euler.Product.Domain as D
import qualified Euler.Product.Domain.OrderStatusResponse as DO
import           Euler.Product.OLTP.Card.Card

import           Euler.Product.OLTP.Services.OrderStatusBuilder
import           Euler.Product.OLTP.Services.OrderStatusCacheService

import           Euler.Storage.DBConfig
import           Euler.Storage.Repository
import qualified Euler.Storage.Types as DB

import           Euler.Services.Gateway.MerchantPaymentGatewayResponse

import           Database.Beam ((==.))
import qualified Database.Beam as B



data FlowError = FlowError
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

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
handleByOrderId rps (Param.OrderId orderId) merchantAccount  = do

  let request = DO.OrderStatusRequest
        { orderId         = orderId
        , merchantId      = merchantAccount ^. _merchantId
        , resellerId      = merchantAccount ^. _resellerId
        , isAuthenticated = True
        , sendCardIsin    = fromMaybe False $ merchantAccount ^. _enableSendingCardIsin
        , sendFullGatewayResponse = getSendFullGatewayResponse rps
        }

  response <- execOrderStatusQuery request

  pure $ mapLeft (const FlowError) response


getSendFullGatewayResponse :: RouteParameters -> Bool
getSendFullGatewayResponse routeParams =
  -- EHS: magic constants
  case Map.lookup "options.add_full_gateway_response" (unRP routeParams) of
    Nothing  -> False
    Just str -> str == "1" || T.map toLower str == "true"

execOrderStatusQuery :: DO.OrderStatusRequest -> Flow (Either Text OrderStatusResponse)
execOrderStatusQuery request = do

  let queryOrderId = request ^. _orderId
  let queryMerchantId = request ^. _merchantId
  let resellerId = request ^. _resellerId
  let isAuth = request ^. _isAuthenticated
  let sendFullGatewayResponse = request ^. _sendFullGatewayResponse

  mbCached <- getCachedResponse queryOrderId queryMerchantId isAuth
  case mbCached of
    Just cached -> pure $ Right cached
    Nothing -> do
      mOrder <- loadOrder queryOrderId queryMerchantId

      order <- case mOrder of
        Just o -> pure o
        Nothing -> throwException err404
          { errBody = "Order not found "
          <> "orderId: " <> show queryOrderId
          <> ", merchantId: " <> show queryMerchantId }

      let orderId = order ^. _orderId
      let merchantId = order ^. _merchantId
      let orderUuid = order ^. _orderUuid
      let orderType = order ^. _orderType
      let orderPId = order ^. _id
      let udf2 = (order ^. _udf) ^. _udf2
      let orderReturnUrl = order ^. _returnUrl

      links <- getPaymentLinks resellerId orderUuid

      mPromotionActive <- getPromotion orderPId orderId
      mMandate <- getMandate orderPId merchantId orderType

      mTxnDetail1 <- loadTxnDetail orderId merchantId orderUuid
      mTxnDetail2 <- getLastTxn orderId merchantId
      let mTxn = (mTxnDetail1 <|> mTxnDetail2)

      gatewayRefId <- case mTxn of
        Nothing -> getGatewayReferenceId2 Nothing orderPId udf2 merchantId
        Just txn -> do
          let gateway = txn ^. _gateway
          getGatewayReferenceId2 gateway orderPId udf2 merchantId

      (mRisk, mTxnCard, mRefunds, mChargeback, mMerchantPgr) <- case mTxn of
        Nothing -> pure (Nothing, Nothing, Nothing, Nothing, Nothing)
        Just txn -> do
          let txnDetailPId = D.txnDetailPId $ txn ^. _id
          mRisk <- getRisk txnDetailPId
          mTxnCard <- loadTxnCardInfo txnDetailPId
          mRefunds <- maybeList <$> loadRefunds txnDetailPId
          mChargeback <- maybeList <$> findChargebacks txnDetailPId
          mMerchantPgr <- getMerchantPGR txn sendFullGatewayResponse
          pure (mRisk, mTxnCard, mRefunds, mChargeback, mMerchantPgr)

      mCardBrand <- case mTxnCard of
        Nothing                    -> pure Nothing
        Just (card :: D.TxnCardInfo) ->
          getCardBrandFromIsin (fromMaybe "" $ card ^. _cardIsin)

      mReturnUrl <- getReturnUrl merchantId orderReturnUrl

      paymentMethodsAndTypes <- case (mTxn, mTxnCard) of
        (Just txn, Just txnCard) -> getPaymentMethodAndType txn txnCard
        _                        -> pure (Nothing, Nothing, Nothing, Nothing)

      txnFlowInfoAndMerchantSFR <- case (mTxn, mTxnCard) of
        (Just txn, Just txnCard) -> do
          let txnDetail_id = D.txnDetailPId $ txn ^. _id
          getTxnFlowInfoAndMerchantSFR txnDetail_id txnCard
        _ -> pure (Nothing, Nothing)

      let res = mapRight mkOrderStatusResponse $ runExcept $ makeOrderStatusResponse
            order
            links
            mPromotionActive
            mMandate
            request
            mTxn
            gatewayRefId
            mRisk
            mTxnCard
            mCardBrand
            mRefunds
            mChargeback
            mReturnUrl
            paymentMethodsAndTypes
            txnFlowInfoAndMerchantSFR
            mMerchantPgr

      case res of
        Left _ -> pure res
        Right res' -> do
          _ <- addToCache orderId merchantId isAuth res'
          pure $ Right res'


makeOrderStatusResponse
  :: D.Order
  -> D.Paymentlinks
  -> Maybe D.PromotionActive
  -> Maybe D.Mandate
  -> DO.OrderStatusRequest
  -> Maybe D.TxnDetail
  -> Text
  -> Maybe D.Risk
  -> Maybe D.TxnCardInfo
  -> Maybe Text
  -> Maybe [D.Refund]
  -> Maybe [D.Chargeback]
  -> Maybe Text -- returnUrl
  -> (Maybe Text, Maybe Text, Maybe Text, Maybe Text)
  -> (Maybe D.TxnFlowInfo, Maybe D.SecondFactorResponse)
  -> Maybe D.MerchantPaymentGatewayResponse
  -> Except Text DO.OrderStatusResponse
makeOrderStatusResponse
  order
  paymentLinks
  mPromotionActive
  mMandate
  request
  mTxn
  gatewayRefId
  mRisk
  mTxnCard
  mCardBrand
  mRefunds
  mChargebacks
  mReturnUrl
  paymentMethodsAndTypes
  (txnFlowInfo, secondFactorResp)
  mMerchantPgr
  = do

  let orderId = order ^. _orderUuid
  let isAuthenticated = request ^. _isAuthenticated
  let sendCardIsin = request ^. _sendCardIsin

  let mCustomerId = whenNothing  (order ^. _customerId) (Just "")
      email = (\mail -> if isAuthenticated then mail else Just "")  (order ^. _customerEmail)
      phone = (\phn -> if isAuthenticated then phn else Just "")  (order ^. _customerPhone)
      amount = order ^. _amount
      amountRefunded = order ^. _amountRefunded

      getStatusId = txnStatusToInt . (^. _status)
      getGatewayId txn = maybe 0 C.gatewayIdFromGateway $ txn ^. _gateway
      getBankErrorCode txn = whenNothing  (txn ^. _bankErrorCode) (Just "")
      getBankErrorMessage txn = whenNothing  (txn ^. _bankErrorMessage) (Just "")
      getGatewayPayload txn = if isBlankMaybe (mGatewayPayload' txn) then (mGatewayPayload' txn) else Nothing
        where mGatewayPayload' t = t ^. _gatewayPayload
      currency = show $ order ^. _currency

      isEmi txn = isTrueMaybe  (txn ^. _isEmi)
      emiTenure txn = txn ^. _emiTenure
      emiBank txn = txn ^. _emiBank

      paymentMethod = whenNothing mCardBrand (Just "UNKNOWN")

      -- maybeTxnCard f = maybe emptyBuilder f mTxnCard
      maybeTxn f = maybe emptyBuilder f mTxn
      maybeTxnAndTxnCard f = case (mTxn, mTxnCard) of
        (Just txn, Just txnCard) -> f txn txnCard
        _                        -> emptyBuilder


  pure $ extract $ buildOrderStatusResponse

    <<= changeMerchantPGR mMerchantPgr
    -- addGatewayResponse

    <<= changeChargeBacks mChargebacks

    <<= changeRefund mRefunds


    <<= changeSecondFactorResponse secondFactorResp
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

    <<= maybeTxn changeTxnDetails
    <<= maybeTxn (changeGatewayPayload . getGatewayPayload)
    <<= maybeTxn (changeBankErrorMessage . getBankErrorMessage)
    <<= maybeTxn (changeBankErrorCode . getBankErrorCode)
    <<= maybeTxn (const $ changeGatewayRefId gatewayRefId)
    <<= maybeTxn (changeGatewayId . getGatewayId)
    <<= maybeTxn (changeTxnUuid . (^. _txnUuid))
    <<= maybeTxn (changeTxnId . (^. _txnId))
    <<= maybeTxn (changeStatusId . getStatusId)
    <<= maybeTxn (changeStatus . DO.TStatus .  (^. _status))

    <<= changeMandate mMandate

    <<= changeAmountAfterPromotion mPromotionActive

    <<= changePromotion mPromotionActive

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
    <<= changeReturnUrl mReturnUrl
    <<= changeCustomerPhone phone
    <<= changeCustomerEmail email
    <<= changeDateCreated (show $ order ^. _dateCreated)
    <<= changeAmountRefunded amountRefunded
    <<= changePaymentLinks paymentLinks
    <<= changeRefunded (order ^. _refundedEntirely)
    <<= changeCurrency currency
    <<= changeAmount amount
    <<= changeStatus (DO.OStatus $ C.toOrderStatusEx $ order ^. _orderStatus)
    <<= changeProductId (order ^. _productId)
    <<= changeCustomerId mCustomerId
    <<= changeOrderId (order ^. _orderId)
    <<= changeMerchantId (order ^. _merchantId)
    <<= changeId orderId


changeId :: Text -> OrderStatusResponseBuilder -> DO.OrderStatusResponse
changeId orderId builder = builder $ mempty {idT = Just $ First orderId}

changeMerchantId :: Text -> OrderStatusResponseBuilder -> DO.OrderStatusResponse
changeMerchantId mMerchantId builder = builder $ mempty {merchant_idT = Just $ First mMerchantId}

changeAmount :: C.Money -> OrderStatusResponseBuilder -> DO.OrderStatusResponse
changeAmount amount builder = builder $ mempty {amountT = Just $ Last amount}

changeOrderId :: Text -> OrderStatusResponseBuilder -> DO.OrderStatusResponse
changeOrderId orderId builder = builder $ mempty {order_idT = Just $ First orderId}

changeCustomerId :: Maybe Text -> OrderStatusResponseBuilder -> DO.OrderStatusResponse
changeCustomerId customerId builder = builder $ mempty {customer_idT = fmap Last customerId}

changeProductId :: Maybe Text -> OrderStatusResponseBuilder -> DO.OrderStatusResponse
changeProductId productId builder = builder $ mempty {product_idT = fmap Last productId}

changeStatus :: DO.OrderTxnStatus -> OrderStatusResponseBuilder -> DO.OrderStatusResponse
changeStatus status builder = builder $ mempty {statusT = Just $ Last status}

changeCurrency :: Text -> OrderStatusResponseBuilder -> DO.OrderStatusResponse
changeCurrency currency builder = builder $ mempty {currencyT = Just $ Last currency}

changeRefunded :: Bool -> OrderStatusResponseBuilder -> DO.OrderStatusResponse
changeRefunded refunded builder = builder $ mempty {refundedT = Just $ Last refunded}

changePaymentLinks :: D.Paymentlinks -> OrderStatusResponseBuilder -> DO.OrderStatusResponse
changePaymentLinks paymentLinks builder = builder $ mempty {payment_linksT = Just $ Last paymentLinks}

changeAmountRefunded :: Maybe C.Money -> OrderStatusResponseBuilder -> DO.OrderStatusResponse
changeAmountRefunded amountRefunded builder = builder $ mempty {amount_refundedT = fmap Last amountRefunded}

changeDateCreated :: Text -> OrderStatusResponseBuilder -> DO.OrderStatusResponse
changeDateCreated dateCreated builder = builder $ mempty {date_createdT = Just $ Last dateCreated}

changeCustomerEmail :: Maybe Text -> OrderStatusResponseBuilder -> DO.OrderStatusResponse
changeCustomerEmail customerEmail builder = builder $ mempty {customer_emailT = map Last customerEmail}

changeCustomerPhone :: Maybe Text -> OrderStatusResponseBuilder -> DO.OrderStatusResponse
changeCustomerPhone customerPhone builder = builder $ mempty {customer_phoneT = map Last customerPhone}

changeReturnUrl :: Maybe Text -> OrderStatusResponseBuilder -> DO.OrderStatusResponse
changeReturnUrl returnUrl builder = builder $ mempty {return_urlT = fmap Last returnUrl}

changeUtf1 :: Maybe Text -> OrderStatusResponseBuilder -> DO.OrderStatusResponse
changeUtf1 utf1 builder = builder $ mempty {udf1T = map Last utf1}

changeUtf2 :: Maybe Text -> OrderStatusResponseBuilder -> DO.OrderStatusResponse
changeUtf2 utf2 builder = builder $ mempty {udf2T = map Last utf2}

changeUtf3 :: Maybe Text -> OrderStatusResponseBuilder -> DO.OrderStatusResponse
changeUtf3 utf3 builder = builder $ mempty {udf3T = map Last utf3}

changeUtf4 :: Maybe Text -> OrderStatusResponseBuilder -> DO.OrderStatusResponse
changeUtf4 utf4 builder = builder $ mempty {udf4T = map Last utf4}

changeUtf5 :: Maybe Text -> OrderStatusResponseBuilder -> DO.OrderStatusResponse
changeUtf5 utf5 builder = builder $ mempty {udf5T = map Last utf5}

changeUtf6 :: Maybe Text -> OrderStatusResponseBuilder -> DO.OrderStatusResponse
changeUtf6 utf6 builder = builder $ mempty {udf6T = map Last utf6}

changeUtf7 :: Maybe Text -> OrderStatusResponseBuilder -> DO.OrderStatusResponse
changeUtf7 utf7 builder = builder $ mempty {udf7T = map Last utf7}

changeUtf8 :: Maybe Text -> OrderStatusResponseBuilder -> DO.OrderStatusResponse
changeUtf8 utf8 builder = builder $ mempty {udf8T = map Last utf8}

changeUtf9 :: Maybe Text -> OrderStatusResponseBuilder -> DO.OrderStatusResponse
changeUtf9 utf9 builder = builder $ mempty {udf9T = map Last utf9}

changeUtf10 :: Maybe Text -> OrderStatusResponseBuilder -> DO.OrderStatusResponse
changeUtf10 utf10 builder = builder $ mempty {udf10T = map Last utf10}


changePromotion :: Maybe D.PromotionActive -> OrderStatusResponseBuilder -> DO.OrderStatusResponse
changePromotion Nothing builder  = builder mempty
changePromotion mNewProm builder = builder mempty { promotionT = fmap Last mNewProm }

changeAmountAfterPromotion :: Maybe D.PromotionActive -> OrderStatusResponseBuilder -> DO.OrderStatusResponse
changeAmountAfterPromotion Nothing builder = builder mempty
changeAmountAfterPromotion (Just newProm) builder =
  let oldStatus = extract builder
      mOldAmount = oldStatus ^. _amount
      mOldPromotion = newProm ^. _discountAmount
      newAmount = sanitizeAmount $ (fromMaybe mempty mOldAmount) <> mOldPromotion
  in builder mempty { amountT = Just $ Last newAmount }


changeMandate :: Maybe D.Mandate -> OrderStatusResponseBuilder -> DO.OrderStatusResponse
changeMandate mandate builder = builder $ mempty { mandateT = fmap Last mandate}


changeStatusId :: Int -> OrderStatusResponseBuilder -> DO.OrderStatusResponse
changeStatusId statusId builder = builder $ mempty {status_idT = Just $ Last statusId}

changeTxnId :: Text -> OrderStatusResponseBuilder -> DO.OrderStatusResponse
changeTxnId txnId builder = builder $ mempty {txn_idT = Just $ Last txnId}

changeTxnUuid :: Maybe Text -> OrderStatusResponseBuilder -> DO.OrderStatusResponse
changeTxnUuid txnUuid builder = builder $ mempty {txn_uuidT = fmap Last txnUuid}

changeGatewayId :: Int -> OrderStatusResponseBuilder -> DO.OrderStatusResponse
changeGatewayId gatewayId builder = builder $ mempty {gateway_idT = Just $ Last gatewayId}

changeGatewayRefId :: Text -> OrderStatusResponseBuilder -> DO.OrderStatusResponse
changeGatewayRefId gatewayRefId builder = builder $ mempty {gateway_reference_idT = Just $ Last gatewayRefId}

changeBankErrorCode :: Maybe Text -> OrderStatusResponseBuilder -> DO.OrderStatusResponse
changeBankErrorCode bankErrorCode builder = builder $ mempty {bank_error_codeT = fmap Last bankErrorCode}

changeBankErrorMessage :: Maybe Text -> OrderStatusResponseBuilder -> DO.OrderStatusResponse
changeBankErrorMessage bankErrorMessage builder = builder $ mempty {bank_error_messageT = fmap Last bankErrorMessage}

changeGatewayPayload :: Maybe Text -> OrderStatusResponseBuilder -> DO.OrderStatusResponse
changeGatewayPayload gatewayPayload builder = builder $ mempty {gateway_payloadT = fmap Last gatewayPayload}

changeTxnDetails :: D.TxnDetail -> OrderStatusResponseBuilder -> DO.OrderStatusResponse
changeTxnDetails txnDetail builder = builder $ mempty {txn_detailT = Just $ Last txnDetail}


changeRisk :: Maybe D.Risk -> OrderStatusResponseBuilder -> DO.OrderStatusResponse
changeRisk risk builder = builder $ mempty {riskT = fmap Last risk}


changeEmiTenureEmiBank :: Maybe Int -> Maybe Text -> OrderStatusResponseBuilder -> DO.OrderStatusResponse
changeEmiTenureEmiBank emiTenure emiBank builder = builder $ mempty
  {emi_tenureT = map Last emiTenure
  , emi_bankT = map Last emiBank
  }

changeAuthType :: Maybe Text -> OrderStatusResponseBuilder -> DO.OrderStatusResponse
changeAuthType authType builder = builder $ mempty {auth_typeT = fmap Last authType}

changeEmiPaymentMethod :: Maybe Text -> OrderStatusResponseBuilder -> DO.OrderStatusResponse
changeEmiPaymentMethod paymentMethod builder = builder $ mempty {payment_methodT = fmap Last paymentMethod}

changePaymentMethodType :: Text -> OrderStatusResponseBuilder -> DO.OrderStatusResponse
changePaymentMethodType paymentMethodType builder =
  builder $ mempty {payment_method_typeT = Just $ Last paymentMethodType}

changeCard :: D.Card -> OrderStatusResponseBuilder -> DO.OrderStatusResponse
changeCard card builder = builder $ mempty {cardT = Just $ Last card}


changeRefund :: Maybe [D.Refund] -> OrderStatusResponseBuilder -> DO.OrderStatusResponse
changeRefund mRefunds builder = builder $ mempty {refundsT = fmap Last mRefunds}


changeChargeBacks :: Maybe [D.Chargeback] -> OrderStatusResponseBuilder -> DO.OrderStatusResponse
changeChargeBacks mChargebacks builder = builder $ mempty {chargebacksT = fmap Last mChargebacks}


changePaymentMethodAndTypeAndVpa
  :: (Maybe Text, Maybe Text, Maybe Text, Maybe Text)
  -> OrderStatusResponseBuilder
  -> DO.OrderStatusResponse
changePaymentMethodAndTypeAndVpa (mPaymentMethod, mPaymentMethodType, mPayerVpa, mPayerAppName) builder =
  builder $ mempty
    { payment_methodT = fmap Last mPaymentMethod
    , payment_method_typeT = fmap Last mPaymentMethodType
    , payer_vpaT = fmap Last mPayerVpa
    , payer_app_nameT = fmap Last mPayerAppName
    }

changeTxnFlowInfo :: Maybe D.TxnFlowInfo -> OrderStatusResponseBuilder -> DO.OrderStatusResponse
changeTxnFlowInfo txnFlowInfo builder = builder $ mempty {txn_flow_infoT = fmap Last txnFlowInfo}

changeSecondFactorResponse
  :: Maybe D.SecondFactorResponse
  -> OrderStatusResponseBuilder
  -> DO.OrderStatusResponse
changeSecondFactorResponse mSFR builder =
  builder $ mempty {second_factor_responseT = map Last mSFR}

changeMerchantPGR :: Maybe D.MerchantPaymentGatewayResponse -> OrderStatusResponseBuilder -> DO.OrderStatusResponse
changeMerchantPGR mMerchantPgr builder =
  builder $ mempty {payment_gateway_responseT = map Last mMerchantPgr}



getLastTxn :: C.OrderId -> C.MerchantId -> Flow (Maybe D.TxnDetail)
getLastTxn orderId merchantId = do

  txnDetails <- loadTxnDetails orderId merchantId

  case txnDetails of
    [] -> do
      logError @Text "get_last_txn"
        $ "No last txn found for orderId: " <> show orderId
        <> " :merchant:" <> show merchantId
      pure Nothing
    _ -> do
      let chargetxn = find (\txn -> (txn ^. _status == CHARGED)) txnDetails
      maybe (pure . Just $ head txnDetails) (pure . Just) chargetxn


getMandate :: C.OrderPId -> C.MerchantId -> C.OrderType -> Flow (Maybe D.Mandate)
getMandate orderPId merchantId = \case
    C.MANDATE_REGISTER -> do
      loadMandate orderPId merchantId
      -- pure $ mapMandate <$> mandate
    _ -> pure Nothing


getPaymentLinks :: Maybe Text -> Text ->  Flow D.Paymentlinks
getPaymentLinks resellerId orderUuid = do
  mResellerAccount <- loadReseller resellerId
  let mResellerEndpoint = maybe Nothing (^. _resellerApiEndpoint) mResellerAccount
  createPaymentLinks orderUuid mResellerEndpoint


createPaymentLinks
  :: Text           -- orderUuid (possibly blank string)
  -> Maybe Text     -- maybeResellerEndpoint
  -> Flow D.Paymentlinks
createPaymentLinks orderUuid maybeResellerEndpoint = do
  config <- runIO getECRConfig
  let protocol = config ^. _protocol
  let host = maybe (protocol <> "://" <> (config ^. _host)) P.id maybeResellerEndpoint
  pure D.Paymentlinks
    { web =   (host <> "/merchant/pay/") <> orderUuid
    , mobile = (host <> "/merchant/pay/") <> orderUuid <> "?mobile=true"
    , iframe = (host <> "/merchant/ipay/") <> orderUuid
    }

getReturnUrl :: C.MerchantId -> Maybe Text -> Flow (Maybe Text)
getReturnUrl merchantIdOrder returnUrlOrder = do
  merchantAccount <- loadMerchantByMerchantId merchantIdOrder
  case merchantAccount of
    Nothing -> pure Nothing
    Just merchantAcc -> do
      merchantIframePreferences <- loadMerchantPrefsMaybe (merchantAcc ^. _merchantId)
      let merchantIframeReturnUrl = (^. _returnUrl) =<< merchantIframePreferences
      pure $ returnUrlOrder <|> (merchantAcc ^. _returnUrl ) <|> merchantIframeReturnUrl


getPromotion :: C.OrderPId -> C.OrderId -> Flow (Maybe D.PromotionActive)
getPromotion orderPId orderId = do
  proms <- loadPromotions orderPId
  getActivePromotion orderId proms

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
  :: Maybe C.Gateway
  -> C.OrderPId
  -> Maybe Text -- udf2
  -> C.MerchantId
  -> Flow Text
getGatewayReferenceId2 gateway orderPId udf2 merchantId = do
  let checkGateway = checkGatewayRefIdForVodafone merchantId udf2 gateway
  let gatewayT = maybe "" show gateway

  ordMeta <- loadOrderMetadataV2 orderPId

  case ordMeta of
    Nothing -> checkGateway
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


checkGatewayRefIdForVodafone
  :: C.MerchantId
  -> Maybe Text
  -> Maybe C.Gateway
  -> Flow Text
checkGatewayRefIdForVodafone merchantId udf2 gateway = do

  meybeFeature <- loadFeature FeatureC.UseUdf2ForGatewayReferenceId merchantId

  case meybeFeature of
    Just feature ->
        if (gateway == Just C.HSBC_UPI)
            && (feature ^. _enabled)
            && (isJust udf2)
        then pure $ fromMaybe "" udf2
        else pure mempty
    Nothing -> pure mempty


getRisk :: Int -> Flow (Maybe D.Risk)
getRisk txnId = do
  txnRiskCheck <- loadTxnRiskCheck txnId
  case txnRiskCheck of
    Nothing -> pure Nothing
    Just trc -> do
      let riskMAId = trc ^. _riskManagementAccountId
      riskMngAcc <- loadRiskManagementAccount riskMAId
      let risk = makeRisk ((^. _provider   ) <$> riskMngAcc) trc
      pure $ Just risk

makeRisk :: Maybe Text -> D.TxnRiskCheck -> D.Risk
makeRisk provider trc = if provider == Just "ebs"
  then case C.decodeRMSIDResult completeResponse of
    Left _ -> risk & _ebsPaymentStatus .~ (trc ^. _riskStatus) :: D.Risk
    Right rmsidResult -> risk
      { D.ebsRiskLevel = rmsidResult ^. _riskLevel
      , D.ebsPaymentStatus = trc ^. _riskStatus
      , D.ebsBinCountry = (rmsidResult ^. _output) ^. _bincountry
      , D.ebsRiskPercentage = rmsidResult ^. _riskPercentage
      }
  else risk
  where
    completeResponse = T.encodeUtf8 $ trc ^. _completeResponse
    risk = D.Risk
      { provider = provider
      , status = trc ^. _status
      , message = trc ^. _message
      , flagged = trc ^. _flagged
      , recommendedAction = trc ^. _recommendedAction
      , ebsRiskLevel = ""
      , ebsPaymentStatus = Nothing
      , ebsBinCountry = ""
      , ebsRiskPercentage = ""
      }


-- makeRisk :: Risk' -> D.TxnRiskCheck -> Risk
-- makeRisk risk' trc = if risk' ^. _provider == Just "ebs"
--   then case C.decodeRMSIDResult completeResponse of
--     Left _ -> mapRisk trc $ risk' & _ebs_payment_status .~ (trc ^. _riskStatus)
--     Right rmsidResult -> mapRisk trc risk'
--       { ebs_risk_level = Just $ rmsidResult ^. _riskLevel
--       , ebs_payment_status = trc ^. _riskStatus
--       , ebs_bin_country = Just $ (rmsidResult ^. _output) ^. _bincountry
--       , ebs_risk_percentage = readMaybe $ T.unpack $ rmsidResult ^. _riskPercentage
--       }
--   else mapRisk trc risk'
--   where
--     completeResponse = T.encodeUtf8 $ trc ^. _completeResponse

-- mapRisk :: D.TxnRiskCheck -> Risk' -> Risk
-- mapRisk trc risk' = case provider of
--   Just "ebs" -> risk
--     { ebs_risk_level = risk' ^. _ebs_risk_level
--     , ebs_payment_status = risk' ^. _ebs_payment_status
--     , ebs_bin_country = risk' ^. _ebs_bin_country
--     , ebs_risk_percentage = Just $ maybe "0" show $ risk' ^.  _ebs_risk_percentage
--     }
--   _ -> risk
--   where
--     provider = risk' ^. _provider
--     risk = Risk
--       { provider = provider
--       , status = risk' ^. _status
--       , message = risk' ^. _message
--       , flagged = show <$> whenNothing (trc ^. _flagged) (Just False)
--       , recommended_action = risk' ^. _recommended_action
--       , ebs_risk_level = Nothing
--       , ebs_payment_status = Nothing
--       , ebs_risk_percentage = Nothing
--       , ebs_bin_country = Nothing
      -- }

-- refundDetails :: Int -> Flow [Refund']
-- refundDetails txnId = do
--   l <- loadRefunds txnId
--   pure $ map mapRefund l


-- chargebackDetails :: Int -> D.TxnDetailStatus -> Flow [D.ChargebackStatus]
-- chargebackDetails txnId txn = do
--   chargebacks <- findChargebacks  txnId
--   pure $ map mapChargeback chargebacks

-- mapChargeback :: D.Chargeback -> D.ChargebackStatus
-- mapChargeback chargeback = D.ChargebackStatus
--   {  id = chargeback ^. _id
--   ,  amount = chargeback ^. _amount
--   ,  objectReferenceId = chargeback ^. _objectReferenceId
--   ,  dateResolved = chargeback ^. _dateResolved
--   ,  dateCreated = chargeback ^. _dateCreated
--   ,  lastUpdated = chargeback ^. _lastUpdated
--   ,  disputeStatus = chargeback ^. _disputeStatus
--   }

sanitizeAmount x = x


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

getPayerVpaByGateway :: Maybe Int -> Maybe C.Gateway -> Flow Text
getPayerVpaByGateway respId gateway = do
  mPgr <- loadPGR respId
  case mPgr of
    Nothing  -> pure T.empty
    Just pgr -> pure $ findPayerVpaByGateway gateway (pgr ^. _responseXml)

findPayerVpaByGateway :: Maybe C.Gateway -> Maybe Text -> Text
findPayerVpaByGateway _ Nothing = T.empty
findPayerVpaByGateway gateway (Just xml) =
  case gateway of
    Nothing -> T.empty
    Just gateway' -> case gateway' of
      C.AXIS_UPI    -> findEntry "payerVpa" (findEntry "customerVpa" "" pgrXml) pgrXml
      C.HDFC_UPI    -> findEntry "payerVpa" "" pgrXml
      C.INDUS_UPI   -> findEntry "payerVpa" "" pgrXml
      C.KOTAK_UPI   -> findEntry "payerVpa" "" pgrXml
      C.SBI_UPI     -> findEntry "payerVpa" "" pgrXml
      C.ICICI_UPI   -> findEntry "payerVpa" "" pgrXml
      C.HSBC_UPI    -> findEntry "payerVpa" "" pgrXml
      C.VIJAYA_UPI  -> findEntry "payerVpa" "" pgrXml
      C.YESBANK_UPI -> findEntry "payerVpa" "" pgrXml
      C.PAYTM_UPI   -> findEntry "payerVpa" "" pgrXml
      C.PAYU        -> findEntry "field3" "" pgrXml
      C.RAZORPAY    -> findEntry "vpa" "" pgrXml
      C.PAYTM_V2    -> findEntry "VPA" "" pgrXml
      C.GOCASHFREE  -> findEntry "payersVPA" "" pgrXml
      _             -> T.empty
  where
    pgrXml = decodePGRXml $ T.encodeUtf8 xml

getTxnFlowInfoAndMerchantSFR
  :: Int
  -> D.TxnCardInfo
  -> Flow (Maybe D.TxnFlowInfo, Maybe D.SecondFactorResponse)
getTxnFlowInfoAndMerchantSFR txnDetId card = do

  if (fromMaybe T.empty $ card ^. _authType) == "VIES" then do

    mSecondFactor <- findSecondFactor txnDetId

    case mSecondFactor of
      Nothing ->  pure (Nothing, Nothing)
      Just sf -> do
        authReqParams <- do
          let authReqParams = fromMaybe "{}" $  sf ^. _gatewayAuthReqParams
          let vies = decode $ BSL.fromStrict $ T.encodeUtf8 authReqParams
          case vies of
            Just v -> pure v
            Nothing -> throwException err500
              {errBody = "AuthReqParams decoding failed"}

        let txnFlowInfo = mkTxnFlowInfo authReqParams

        mSecondFactorResponse <- findSecondFactorResponse $ D.sfPId $ sf ^. _id

        pure (Just txnFlowInfo, mSecondFactorResponse)
    else pure (Nothing, Nothing) --NON VIES Txn

mkTxnFlowInfo :: C.ViesGatewayAuthReqParams -> D.TxnFlowInfo
mkTxnFlowInfo params = D.TxnFlowInfo
  {  flowType = params ^. _flow
  ,  status = params ^. _flowStatus
  ,  errorCode = params ^. _errorCode
  ,  errorMessage = params ^. _errorMessage
  }

-- mkMerchantSecondFactorResponse :: D.SecondFactorResponse -> D.MerchantSecondFactorResponse
-- mkMerchantSecondFactorResponse sfr = D.MerchantSecondFactorResponse
--   { cavv = sfr ^. _cavv
--   , eci = sfr ^. _eci
--   , xid = sfr ^. _xid
--   , paresStatus = sfr ^. _status
  -- }

getMerchantPGR :: D.TxnDetail -> Bool -> Flow (Maybe D.MerchantPaymentGatewayResponse)
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
      let mPgr = D.defaultMerchantPaymentGatewayResponse & _created .~ date
      -- let gateway' = maybe "" show gateway
      let merchantPgr = transformMpgrByGateway mPgr pgrXml $ mkMerchantPGRServiceTemp gateway txn pgr pgrXml
      let gatewayResp = getGatewayResponseInJson pgr shouldSendFullGatewayResponse
      -- let merchantPgr = makeMerchantPaymentGatewayResponse gatewayResponse merchantPgr'
      pure $ Just (merchantPgr & _gatewayResponse .~ gatewayResp)

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

getCardDetails :: D.TxnCardInfo -> D.TxnDetail -> Bool -> D.Card
getCardDetails card txn shouldSendCardIsin = D.Card
  { expiryYear = card ^. _cardExpYear
  , cardReference = card ^. _cardReferenceId
  , savedToLocker = isSavedToLocker card txn
  , expiryMonth = card ^. _cardExpMonth
  , nameOnCard = card ^. _nameOnCard
  , cardIssuer = card ^. _cardIssuerBankName
  , lastFourDigits = card ^. _cardLastFourDigits
  , usingSavedCard = txn ^. _expressCheckout
  , cardFingerprint = card ^. _cardFingerprint
  , cardIsin = if shouldSendCardIsin then (card ^. _cardIsin) else Just ""
  , cardType = card ^. _cardType
  , cardBrand = card ^. _cardSwitchProvider
  , shouldSendCardIsin = shouldSendCardIsin
  }
  where
    isSavedToLocker card' txn' =
      isTrueMaybe (txn' ^. _addToLocker) && (isBlankMaybe $ card' ^. _cardReferenceId)

getActivePromotion :: C.OrderId -> [D.Promotion] -> Flow (Maybe D.PromotionActive)
getActivePromotion _ [] = pure Nothing
getActivePromotion orderId promotions = do
  let mPromotion = find (\promotion -> (promotion ^. _status) == "ACTIVE" ) promotions
  case mPromotion of
    Nothing -> pure Nothing
    Just promotion -> do
      let rulesRaw = promotion ^. _rules
      rules <- decryptPromotionRules rulesRaw
      pure $ Just $ mkPromotionActive rules orderId promotion

mkPromotionActive :: C.Rules -> C.OrderId -> D.Promotion -> D.PromotionActive
mkPromotionActive rule orderId p = D.PromotionActive
  { id               = p ^. _id
  , orderId          = orderId
  , rules            = rule
  , dateCreated      = p ^. _dateCreated
  , discountAmount   = p ^. _discountAmount
  , status           = p ^. _status
  }

decryptPromotionRules :: Text -> Flow C.Rules
decryptPromotionRules rulesTxt = do

  -- EHS: TODO: ecTempCardCred partly implemented. See Euler.Config.Config for details.
  keyForDecryption <- Config.ecTempCardCred

  let rulesDecoded = B64.decode $ T.encodeUtf8 rulesTxt -- $ promotion ^. _rules
  rulesjson <- case rulesDecoded of
    Right result -> pure $ E.decryptEcb keyForDecryption result
    Left err     -> throwException err500 {errBody = LC.pack err}

  let rules = getRulesFromString rulesjson
  let rValue = getMaskedAccNo (rules ^. _value)
  pure $ rules & _value .~ rValue

getRulesFromString :: Either E.EncryptionError ByteString -> C.Rules
getRulesFromString bs =
  case A.decode . BSL.fromStrict <$> bs of
    Right (Just (rules :: [C.Rules])) -> case find (\r -> r ^. _dimension == "card_number") rules of
      Just resp -> resp
      Nothing   -> C.Rules { dimension = "", value = ""}
    _ -> C.Rules { dimension = "", value = ""}

getMaskedAccNo :: Text -> Text
getMaskedAccNo txt = T.append (T.map (const 'X') $ T.dropEnd 4 txt) (T.takeEnd 4 txt)

