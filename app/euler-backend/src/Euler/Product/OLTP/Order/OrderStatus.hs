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
import qualified Euler.Config.Creditails as Cred
import           Euler.Lens
import           EulerHS.Language
import           EulerHS.Types

import           Control.Comonad (extract)
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
import qualified Euler.API.RouteParameters as RP


import qualified Euler.Constant.Feature as FeatureC

import           Euler.Common.Types.External.Mandate as M
import qualified Euler.Common.Types                           as C
import           Euler.Common.Types.PaymentGatewayResponseXml
import           Euler.Common.Types.TxnDetail                 (TxnStatus (..), txnStatusToInt)

import           Euler.Common.Utils
import           Euler.Config.Config as Config

import qualified Euler.Product.Domain as D

import           Euler.Product.OLTP.Card.Card
import qualified Euler.Product.OLTP.Order.OrderStatusVersioningService as VS
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


-- | API handler
handleByOrderId
  :: RouteParameters
  -- EHS: why RP?
  -> RP.OrderId
  -> D.MerchantAccount
  -> Flow (Either FlowError OrderStatusResponse)
handleByOrderId rps (RP.OrderId orderId) merchantAccount  = do

    let request = D.OrderStatusRequest
          { orderId                 = orderId
          , merchantId              = merchantAccount ^. _merchantId
          , resellerId              = merchantAccount ^. _resellerId
          , isAuthenticated         = True
          , sendCardIsin            = fromMaybe False $ merchantAccount ^. _enableSendingCardIsin
          , sendFullGatewayResponse = sendFullGatewayResponse'
          , sendAuthToken           = False
          , version                 = version'
          }
    response <- execOrderStatusQuery request
    pure $ mapLeft (const FlowError) response
  where
    sendFullGatewayResponse' =
      -- EHS: magic constants
      case (Map.lookup "options.add_full_gateway_response" (unRP rps)) of
        Nothing  -> False
        Just str -> str == "1" || T.map toLower str == "true"
    version' = RP.lookupRP @RP.Version rps



-- | Top-level domain-type handler
execOrderStatusQuery :: D.OrderStatusRequest-> Flow (Either Text OrderStatusResponse)
execOrderStatusQuery req@D.OrderStatusRequest{..} = do
    mbCached <- fastPath
    result <- case mbCached of
      Just cached -> pure cached
      Nothing -> slowPath
    -- EHS: todo: version transformations and token adding should be done later on
    -- EHS: gateway transformations? are we done with it?
    --let gatewayId = fromMaybe 0 $ resp' ^. _gateway_id
    --pure $ transformOrderStatus gatewayId resp'
    let vHandle = VS.mkHandle version sendAuthToken
    transformed <- VS.doVersionTransformation vHandle result
    pure $ Right transformed
  where
    fastPath = getCachedResponse orderId merchantId isAuthenticated
    slowPath = do
      r <- execOrderStatusQuery' req
      case r of
        Left e -> do
          -- EHS: fixme
          logError @Text "" ""
          throwException err500
        Right r' -> do
          _ <- addToCache orderId merchantId isAuthenticated r'
          pure r'


-- | Slow-path execution of order status query
execOrderStatusQuery' :: D.OrderStatusRequest -> Flow (Either Text OrderStatusResponse)
execOrderStatusQuery' request = do

  let queryOrderId = request ^. _orderId
  let queryMerchantId = request ^. _merchantId
  let resellerId = request ^. _resellerId
  let sendFullGatewayResponse = request ^. _sendFullGatewayResponse

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
    Nothing -> getGatewayReferenceId Nothing orderPId udf2 merchantId
    Just txn -> getGatewayReferenceId (txn ^. _gateway) orderPId udf2 merchantId

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
    Nothing                      -> pure Nothing
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

  pure $ mapRight mkOrderStatusResponse
       $ runExcept
       $ makeOrderStatusResponse
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


makeOrderStatusResponse
  :: D.Order
  -> D.Paymentlinks
  -> Maybe D.PromotionActive
  -> Maybe D.Mandate
  -> D.OrderStatusRequest
  -> Maybe D.TxnDetail
  -> Text
  -> Maybe D.Risk
  -> Maybe D.TxnCardInfo
  -> Maybe Text -- cardBrand
  -> Maybe [D.Refund]
  -> Maybe [D.Chargeback]
  -> Maybe Text -- returnUrl
  -> (Maybe Text, Maybe M.PaymentMethodType, Maybe Text, Maybe Text)
  -> (Maybe D.TxnFlowInfo, Maybe D.SecondFactorResponse)
  -> Maybe D.MerchantPaymentGatewayResponse
  -> Except Text D.OrderStatusResponse
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


      -- maybeTxnCard f = maybe emptyBuilder f mTxnCard
      maybeTxn f = maybe emptyBuilder f mTxn
      maybeTxnAndTxnCard f = case (mTxn, mTxnCard) of
        (Just txn, Just txnCard) -> f txn txnCard
        _                        -> emptyBuilder

-- <== is equal to =>> and used for quick visual understanding
  pure $ extract $ buildOrderStatusResponse
    -- finish

    <== changeMerchantPGR mMerchantPgr
    -- addGatewayResponse

    <== changeChargeBacks mChargebacks

    <== changeRefund mRefunds


    <== changeSecondFactorResponse secondFactorResp
    <== changeTxnFlowInfo txnFlowInfo
      -- addSecondFactorResponseAndTxnFlowInfo

    <== changeCard (getCardDetails mTxnCard mTxn sendCardIsin)

    <== changePaymentMethodType (getPaymentMethodType mTxnCard mTxn)

    <== changeEmiPaymentMethod (getEmiPaymentMethod mCardBrand  mTxnCard mTxn)

    <== maybeTxnAndTxnCard (\_ txnCard -> changeAuthType $ whenNothing (txnCard ^. _authType) (Just ""))
    <== maybeTxnAndTxnCard (\txn _ -> if isEmi txn then changeEmiTenureEmiBank (emiTenure txn) (emiBank txn) else emptyBuilder)

    <== maybeTxnAndTxnCard (\_ _ -> changePaymentMethodAndTypeAndVpa paymentMethodsAndTypes)

    <== changeRisk mRisk

    <== maybeTxn changeTxnDetails
    <== maybeTxn (changeGatewayPayload . getGatewayPayload)
    <== maybeTxn (changeBankErrorMessage . getBankErrorMessage)
    <== maybeTxn (changeBankErrorCode . getBankErrorCode)
    <== changeGatewayRefId gatewayRefId
    <== maybeTxn (changeGatewayId . getGatewayId)
    <== maybeTxn (changeTxnUuid . (^. _txnUuid))
    <== maybeTxn (changeTxnId . (^. _txnId))
    <== maybeTxn (changeStatusId . getStatusId)
    <== maybeTxn (changeStatus . D.TStatus .  (^. _status))

    <== changeMandate mMandate

    -- changeAmountAfterPromotion should follow after changePromotion
    <== changeAmountAfterPromotion mPromotionActive
    <== changePromotion mPromotionActive

    <== changeUtf (order ^. _udf)
    <== changeReturnUrl mReturnUrl
    <== changeCustomerPhone phone
    <== changeCustomerEmail email
    <== changeDateCreated (show $ order ^. _dateCreated)
    <== changeAmountRefunded amountRefunded
    <== changePaymentLinks paymentLinks
    <== changeRefunded (order ^. _refundedEntirely)
    <== changeCurrency currency
    <== changeAmount amount
    <== changeStatus (D.OStatus $ C.toOrderStatusEx $ order ^. _orderStatus)
    <== changeProductId (order ^. _productId)
    <== changeCustomerId mCustomerId
    <== changeOrderId (order ^. _orderId)
    <== changeMerchantId (order ^. _merchantId)
    <== changeId orderId
    -- start


changeId :: Text -> OrderStatusResponseBuilder -> D.OrderStatusResponse
changeId orderId builder = builder $ mempty {idT = Just $ First orderId}

changeMerchantId :: Text -> OrderStatusResponseBuilder -> D.OrderStatusResponse
changeMerchantId mMerchantId builder = builder $ mempty {merchant_idT = Just $ First mMerchantId}

changeAmount :: C.Money -> OrderStatusResponseBuilder -> D.OrderStatusResponse
changeAmount amount builder = builder $ mempty {amountT = Just $ Last amount}

changeOrderId :: Text -> OrderStatusResponseBuilder -> D.OrderStatusResponse
changeOrderId orderId builder = builder $ mempty {order_idT = Just $ First orderId}

changeCustomerId :: Maybe Text -> OrderStatusResponseBuilder -> D.OrderStatusResponse
changeCustomerId customerId builder = builder $ mempty {customer_idT = fmap Last customerId}

changeProductId :: Maybe Text -> OrderStatusResponseBuilder -> D.OrderStatusResponse
changeProductId productId builder = builder $ mempty {product_idT = fmap Last productId}

changeStatus :: D.OrderTxnStatus -> OrderStatusResponseBuilder -> D.OrderStatusResponse
changeStatus status builder = builder $ mempty {statusT = Just $ Last status}

changeCurrency :: Text -> OrderStatusResponseBuilder -> D.OrderStatusResponse
changeCurrency currency builder = builder $ mempty {currencyT = Just $ Last currency}

changeRefunded :: Bool -> OrderStatusResponseBuilder -> D.OrderStatusResponse
changeRefunded refunded builder = builder $ mempty {refundedT = Just $ Last refunded}

changePaymentLinks :: D.Paymentlinks -> OrderStatusResponseBuilder -> D.OrderStatusResponse
changePaymentLinks paymentLinks builder = builder $ mempty {payment_linksT = Just $ Last paymentLinks}

changeAmountRefunded :: Maybe C.Money -> OrderStatusResponseBuilder -> D.OrderStatusResponse
changeAmountRefunded amountRefunded builder = builder $ mempty {amount_refundedT = fmap Last amountRefunded}

changeDateCreated :: Text -> OrderStatusResponseBuilder -> D.OrderStatusResponse
changeDateCreated dateCreated builder = builder $ mempty {date_createdT = Just $ Last dateCreated}

changeCustomerEmail :: Maybe Text -> OrderStatusResponseBuilder -> D.OrderStatusResponse
changeCustomerEmail customerEmail builder = builder $ mempty {customer_emailT = map Last customerEmail}

changeCustomerPhone :: Maybe Text -> OrderStatusResponseBuilder -> D.OrderStatusResponse
changeCustomerPhone customerPhone builder = builder $ mempty {customer_phoneT = map Last customerPhone}

changeReturnUrl :: Maybe Text -> OrderStatusResponseBuilder -> D.OrderStatusResponse
changeReturnUrl returnUrl builder = builder $ mempty {return_urlT = fmap Last returnUrl}

changeUtf :: C.UDF -> OrderStatusResponseBuilder -> D.OrderStatusResponse
changeUtf utf builder = builder $ mempty {udfT = Just $ Last utf}

changePromotion :: Maybe D.PromotionActive -> OrderStatusResponseBuilder -> D.OrderStatusResponse
changePromotion Nothing builder  = builder mempty
changePromotion mNewProm builder = builder mempty { promotionT = fmap Last mNewProm }

changeAmountAfterPromotion :: Maybe D.PromotionActive -> OrderStatusResponseBuilder -> D.OrderStatusResponse
changeAmountAfterPromotion Nothing builder = builder mempty
changeAmountAfterPromotion (Just newProm) builder =
  let oldStatus = extract builder
      mOldAmount = oldStatus ^. _amount
      mOldPromotion = newProm ^. _discountAmount
      newAmount = sanitizeAmount $ (fromMaybe mempty mOldAmount) <> mOldPromotion
  in builder mempty { amountT = Just $ Last newAmount }


changeMandate :: Maybe D.Mandate -> OrderStatusResponseBuilder -> D.OrderStatusResponse
changeMandate mandate builder = builder $ mempty { mandateT = fmap Last mandate}


changeStatusId :: Int -> OrderStatusResponseBuilder -> D.OrderStatusResponse
changeStatusId statusId builder = builder $ mempty {status_idT = Just $ Last statusId}

changeTxnId :: Text -> OrderStatusResponseBuilder -> D.OrderStatusResponse
changeTxnId txnId builder = builder $ mempty {txn_idT = Just $ Last txnId}

changeTxnUuid :: Maybe Text -> OrderStatusResponseBuilder -> D.OrderStatusResponse
changeTxnUuid txnUuid builder = builder $ mempty {txn_uuidT = fmap Last txnUuid}

changeGatewayId :: Int -> OrderStatusResponseBuilder -> D.OrderStatusResponse
changeGatewayId gatewayId builder = builder $ mempty {gateway_idT = Just $ Last gatewayId}

changeGatewayRefId :: Text -> OrderStatusResponseBuilder -> D.OrderStatusResponse
changeGatewayRefId gatewayRefId builder = builder $ mempty {gateway_reference_idT = Just $ Last gatewayRefId}

changeBankErrorCode :: Maybe Text -> OrderStatusResponseBuilder -> D.OrderStatusResponse
changeBankErrorCode bankErrorCode builder = builder $ mempty {bank_error_codeT = fmap Last bankErrorCode}

changeBankErrorMessage :: Maybe Text -> OrderStatusResponseBuilder -> D.OrderStatusResponse
changeBankErrorMessage bankErrorMessage builder = builder $ mempty {bank_error_messageT = fmap Last bankErrorMessage}

changeGatewayPayload :: Maybe Text -> OrderStatusResponseBuilder -> D.OrderStatusResponse
changeGatewayPayload gatewayPayload builder = builder $ mempty {gateway_payloadT = fmap Last gatewayPayload}

changeTxnDetails :: D.TxnDetail -> OrderStatusResponseBuilder -> D.OrderStatusResponse
changeTxnDetails txnDetail builder = builder $ mempty {txn_detailT = Just $ Last txnDetail}


changeRisk :: Maybe D.Risk -> OrderStatusResponseBuilder -> D.OrderStatusResponse
changeRisk risk builder = builder $ mempty {riskT = fmap Last risk}


changeEmiTenureEmiBank :: Maybe Int -> Maybe Text -> OrderStatusResponseBuilder -> D.OrderStatusResponse
changeEmiTenureEmiBank emiTenure emiBank builder = builder $ mempty
  {emi_tenureT = map Last emiTenure
  , emi_bankT = map Last emiBank
  }

changeAuthType :: Maybe Text -> OrderStatusResponseBuilder -> D.OrderStatusResponse
changeAuthType authType builder = builder $ mempty {auth_typeT = fmap Last authType}

changeEmiPaymentMethod :: Maybe Text -> OrderStatusResponseBuilder -> D.OrderStatusResponse
changeEmiPaymentMethod paymentMethod builder = builder $ mempty {payment_methodT = fmap Last paymentMethod}

changePaymentMethodType
  :: Maybe M.PaymentMethodType
  -> OrderStatusResponseBuilder
  -> D.OrderStatusResponse
changePaymentMethodType paymentMethodType builder =
  builder $ mempty {payment_method_typeT = fmap Last paymentMethodType}

changeCard :: Maybe D.Card -> OrderStatusResponseBuilder -> D.OrderStatusResponse
changeCard card builder = builder $ mempty {cardT = fmap Last card}


changeRefund :: Maybe [D.Refund] -> OrderStatusResponseBuilder -> D.OrderStatusResponse
changeRefund mRefunds builder = builder $ mempty {refundsT = fmap Last mRefunds}


changeChargeBacks :: Maybe [D.Chargeback] -> OrderStatusResponseBuilder -> D.OrderStatusResponse
changeChargeBacks mChargebacks builder = builder $ mempty {chargebacksT = fmap Last mChargebacks}


changePaymentMethodAndTypeAndVpa
  :: (Maybe Text, Maybe M.PaymentMethodType, Maybe Text, Maybe Text)
  -> OrderStatusResponseBuilder
  -> D.OrderStatusResponse
changePaymentMethodAndTypeAndVpa (mPaymentMethod, mPaymentMethodType, mPayerVpa, mPayerAppName) builder =
  builder $ mempty
    { payment_methodT = fmap Last mPaymentMethod
    , payment_method_typeT = fmap Last mPaymentMethodType
    , payer_vpaT = fmap Last mPayerVpa
    , payer_app_nameT = fmap Last mPayerAppName
    }

changeTxnFlowInfo :: Maybe D.TxnFlowInfo -> OrderStatusResponseBuilder -> D.OrderStatusResponse
changeTxnFlowInfo txnFlowInfo builder = builder $ mempty {txn_flow_infoT = fmap Last txnFlowInfo}

changeSecondFactorResponse
  :: Maybe D.SecondFactorResponse
  -> OrderStatusResponseBuilder
  -> D.OrderStatusResponse
changeSecondFactorResponse mSFR builder =
  builder $ mempty {second_factor_responseT = map Last mSFR}

changeMerchantPGR :: Maybe D.MerchantPaymentGatewayResponse -> OrderStatusResponseBuilder -> D.OrderStatusResponse
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
  let config = getECRConfig
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


getGatewayReferenceId
  :: Maybe C.Gateway
  -> C.OrderPId
  -> Maybe Text -- udf2
  -> C.MerchantId
  -> Flow Text
getGatewayReferenceId gateway orderPId udf2 merchantId = do
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
  -> Flow (Maybe Text, Maybe M.PaymentMethodType, Maybe Text, Maybe Text)
  -- ^ Result is (payment_method, payment_method_type, payer_vpa, payer_app_name)
  -- when Nothing do not change a field
getPaymentMethodAndType txn card = do
  case (card ^. _cardType) of
    Just "NB" -> pure
      ( whenNothing (card ^. _cardIssuerBankName) (Just T.empty)
      , Just M.NB
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
        , Just M.WALLET
        , Just payerVpa
        , Nothing
        )

    Just "UPI" -> do
      let payment_method = Just "UPI"
          payment_method_type = Just M.UPI
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
        , Just M.PAYLATER
        , Nothing
        , Nothing
        )
    Just "CARD" -> pure
      ( whenNothing (card ^. _cardIssuerBankName) (Just T.empty)
      , Just M.CARD
      , Nothing
      , Nothing
      )
    Just "REWARD" -> pure
      ( whenNothing (card ^. _cardIssuerBankName) (Just T.empty)
      , Just M.REWARD
      , Nothing
      , Nothing
      )
    Just "ATM_CARD" -> pure
      ( whenNothing (card ^. _cardIssuerBankName) (Just T.empty)
      , Just M.CARD
      , Nothing
      , Nothing
      )
    Just _ -> checkPaymentMethodType card
    Nothing -> checkPaymentMethodType card

  where
    checkPaymentMethodType card' = case (card' ^. _paymentMethodType) of
      Just M.CASH -> pure
        ( whenNothing (card' ^. _paymentMethod) (Just T.empty)
        , Just M.CASH
        , Nothing
        , Nothing
        )
      Just M.CONSUMER_FINANCE -> pure
        ( whenNothing (card' ^. _paymentMethod) (Just T.empty)
        , Just M.CONSUMER_FINANCE
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

getCardDetails :: Maybe D.TxnCardInfo -> Maybe D.TxnDetail -> Bool -> Maybe D.Card
getCardDetails (Just card) (Just txn) shouldSendCardIsin =
  if isBlankMaybe (card ^. _cardIsin)
    then Just D.Card
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
    else Nothing
  where
    isSavedToLocker card' txn' =
      isTrueMaybe (txn' ^. _addToLocker) && (isBlankMaybe $ card' ^. _cardReferenceId)
getCardDetails _ _ _ = Nothing

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

  ecCred <- Cred.ecTempCardCred

  let rulesDecoded = B64.decode $ T.encodeUtf8 rulesTxt -- $ promotion ^. _rules
  rulesjson <- case rulesDecoded of
    Right result -> pure $ E.decryptEcb (E.Key ecCred :: E.Key E.AES256 ByteString) result
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

getPaymentMethodType :: Maybe D.TxnCardInfo -> Maybe D.TxnDetail -> Maybe M.PaymentMethodType
getPaymentMethodType (Just txnCard) (Just _) = if isBlankMaybe (txnCard ^. _cardIsin)
  then Just M.CARD
  else Nothing
getPaymentMethodType _ _ = Nothing

getEmiPaymentMethod :: Maybe Text -> Maybe D.TxnCardInfo -> Maybe D.TxnDetail -> Maybe Text
getEmiPaymentMethod (Just cardBrand) (Just txnCard) (Just _) =
  if isBlankMaybe (txnCard ^. _cardIsin) then Just cardBrand else Nothing
getEmiPaymentMethod Nothing (Just txnCard) (Just _) =
  if isBlankMaybe (txnCard ^. _cardIsin) then Just "UNKNOWN" else Nothing
getEmiPaymentMethod _ _ _ = Nothing
