{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Euler.Product.OLTP.Order.OrderStatus
  ( -- * main handler by orderId and route parameters
    orderStatus
    -- * OrderStatusResponse-based handler
  , orderStatusRequest
    -- * EHS: these are used only in tests
  , findPayerVpaByGateway
  , makeRisk
  , makeOrderStatusResponse
  ) where


import           EulerHS.Prelude hiding (First, Last, getFirst, getLast, id)
import qualified EulerHS.Prelude as P (id)
import           EulerHS.Types

import           EulerHS.Language

import           Control.Comonad (extract)
-- import           Control.Monad.Except
import           Data.Aeson
import qualified Data.Aeson              as A
import qualified Data.ByteString.Base64  as B64
import qualified Data.ByteString.Lazy    as BSL
import qualified Data.Map.Strict         as Map
import           Data.Semigroup          as S
import qualified Data.Text               as T
import qualified Data.Text.Encoding      as T
import qualified Data.Text.Lazy.Encoding as TL
import           Data.Time (LocalTime)

import           Euler.API.Order as AO
import           Euler.API.RouteParameters (RouteParameters (..))
import qualified Euler.API.RouteParameters            as RP
import qualified Euler.Config.Creditails              as Cred
import qualified Euler.Constant.Feature               as FeatureC
import qualified Euler.Common.Errors.PredefinedErrors as Errs
import qualified Euler.Common.Types                   as C
import           Euler.Common.Types.External.Mandate  as M
import           Euler.Common.Types.PaymentGatewayResponseXml
import           Euler.Common.Types.TxnDetail (TxnStatus (..))
import           Euler.Common.Utils
import           Euler.Config.Config                  as Config
import qualified Euler.Encryption                     as E
import           Euler.Lens
import qualified Euler.Product.Domain                 as D
import           Euler.Product.OLTP.Card.Card
import qualified Euler.Product.OLTP.Order.OrderStatusVersioningService as VS
import           Euler.Product.OLTP.Services.OrderStatusBuilder
import           Euler.Product.OLTP.Services.OrderStatusCacheService
import           Euler.Storage.Repository
import qualified Euler.Storage.Types as DB
import           Euler.Services.Gateway.MerchantPaymentGatewayResponse
import           WebService.Language



-- | API handler wrapper, constructs OrderStatusRequest and runs it
orderStatus
  :: RouteParameters
  -- EHS: is it worth moving this from RP to API types?
  -> RP.OrderId
  -> D.MerchantAccount
  -> Flow AO.OrderStatusResponse
orderStatus rps (RP.OrderId orderId) merchantAccount  = do
    orderStatusRequest request
  where
    request = D.OrderStatusRequest
          { orderId                 = orderId
          , merchantId              = merchantAccount ^. _merchantId
          , merchantReturnUrl       = merchantAccount ^. _returnUrl
          , resellerId              = merchantAccount ^. _resellerId
          , isAuthenticated         = True
          , sendCardIsin            = fromMaybe False $ merchantAccount ^. _enableSendingCardIsin
          , sendFullGatewayResponse = sendFullPrgResponse'
          , sendAuthToken           = False
          , version                 = version'
          }
    sendFullPrgResponse' = RP.sendFullPgr rps
    version'             = RP.lookupRP @RP.Version rps


-- | Top-level flow to run OrderStatusRequest
orderStatusRequest :: D.OrderStatusRequest -> Flow AO.OrderStatusResponse
orderStatusRequest req@D.OrderStatusRequest{..} = do
    mbCached <- fastPath
    result <- case mbCached of
      Just cached -> pure cached
      Nothing     -> slowPath
    let vHandle = VS.mkHandle version sendAuthToken
    transformed <- VS.doVersionTransformation vHandle result
    pure transformed
  where
    fastPath = getCachedResponse orderId merchantId isAuthenticated
    slowPath = do
      res <- execOrderStatus req
      _ <- addToCache orderId merchantId isAuthenticated res
      pure res


-- | Slow-path flow to execute order status request
execOrderStatus :: D.OrderStatusRequest -> Flow OrderStatusResponse
execOrderStatus request = do

  let reqOrderId = request ^. _orderId
  let reqMerchantId = request ^. _merchantId
  let merchantReturnUrl = request ^. _merchantReturnUrl
  let resellerId = request ^. _resellerId
  let sendFullGatewayResponse = request ^. _sendFullGatewayResponse

  -- EHS: I would suggest to have `loadSthStrict` functions in repositories to not repeat ourselves each time
  -- we are loading a mandatory value
  mOrder <- loadOrder reqOrderId reqMerchantId

  order <- case mOrder of
    Just o -> pure o
    Nothing -> do
      logErrorT "OrderStatus.execOrderStatusQuery'"
        $ "Order not found "
        <> "orderId: " <> show reqOrderId
        <> ", merchantId: " <> show reqMerchantId
      throwException $ Errs.orderNotFound reqOrderId

  let orderId = order ^. _orderId
  let merchantId = order ^. _merchantId
  let orderUuid = order ^. _orderUuid
  let orderType = order ^. _orderType
  let orderPId = order ^. _id
  let udf2 = (order ^. _udf) ^. _udf2
  let orderReturnUrl = order ^. _returnUrl

  awaitLinks <- forkFlow' "getPaymentLinks" $ getPaymentLinks resellerId orderUuid
  awaitPromotionActive <- forkFlow' "getPromotion" $ getPromotion orderPId orderId
  awaitMandate <- forkFlow' "getMandate" $ getMandate orderPId merchantId orderType
  awaitReturnUrl <- forkFlow' "getReturnUrl" $ getReturnUrl reqMerchantId merchantReturnUrl orderReturnUrl

  mTxnDetail1 <- loadTxnDetail orderId merchantId orderUuid
  mTxnDetail2 <- getLastTxn orderId merchantId
  let mTxn = (mTxnDetail1 <|> mTxnDetail2)

  mTxnCard <- case mTxn of
    Nothing -> pure Nothing
    Just txn -> loadTxnCardInfo (D.txnDetailPId $ txn ^. _id)

  awaitGatewayRefId <- forkFlow' "getGatewayReferenceId" $ case mTxn of
    Nothing  -> getGatewayReferenceId Nothing orderPId udf2 merchantId
    Just txn -> getGatewayReferenceId (txn ^. _gateway) orderPId udf2 merchantId

  awaitRisk <- forkFlow' "getRisk" $ case mTxn of
    Nothing -> pure Nothing
    Just txn -> getRisk $ D.txnDetailPId $ txn ^. _id

  awaitRefunds <- forkFlow' "loadRefunds" $ case mTxn of
    Nothing -> pure Nothing
    Just txn -> maybeList <$> (loadRefunds $ D.txnDetailPId $ txn ^. _id)

  awaitChargeback <- forkFlow' "findChargebacks" $ case mTxn of
    Nothing -> pure Nothing
    Just txn -> maybeList <$> findChargebacks (D.txnDetailPId $ txn ^. _id)

  awaitMerchantPgr <- forkFlow' "getMerchantPGR" $ case mTxn of
    Nothing -> pure Nothing
    Just txn -> getMerchantPGR txn sendFullGatewayResponse

  awaitCardBrand <- forkFlow' "getEmiPaymentMethod" $ case mTxnCard of
    Nothing                      -> pure Nothing
    Just (card :: D.TxnCardInfo) -> do
      let cardIsin = card ^. _cardIsin
      cardBrand <- getCardBrandFromIsin cardIsin
      pure $ getEmiPaymentMethod cardIsin cardBrand

  awaitPaymentMethodsAndTypes <- forkFlow' "getPaymentMethodAndType" $
    case (mTxn, mTxnCard) of
      (Just txn, Just txnCard) -> getPaymentMethodAndType txn txnCard
      _                        -> pure (Nothing, Nothing, Nothing, Nothing)

  awaitTxnFlowInfoAndMerchantSFR <- forkFlow' "getTxnFlowInfoAndMerchantSFR" $
    case (mTxn, mTxnCard) of
      (Just txn, Just txnCard) -> do
        let txnDetail_id = D.txnDetailPId $ txn ^. _id
        getTxnFlowInfoAndMerchantSFR txnDetail_id txnCard
      _ -> pure (Nothing, Nothing)

  links <- fromAwait request $ await Nothing awaitLinks
  mPromotionActive <- fromAwait request $ await Nothing awaitPromotionActive
  mMandate <- fromAwait request $ await Nothing awaitMandate
  mReturnUrl <- fromAwait request $ await Nothing awaitReturnUrl
  gatewayRefId <- fromAwait request $ await Nothing awaitGatewayRefId
  mRisk <- fromAwait request $ await Nothing awaitRisk
  mRefunds <- fromAwait request $ await Nothing awaitRefunds
  mChargeback <- fromAwait request $ await Nothing awaitChargeback
  mMerchantPgr <- fromAwait request $ await Nothing awaitMerchantPgr
  mCardBrand <- fromAwait request $ await Nothing awaitCardBrand
  paymentMethodsAndTypes <- fromAwait request $ await Nothing awaitPaymentMethodsAndTypes
  txnFlowInfoAndMerchantSFR <- fromAwait request $ await Nothing awaitTxnFlowInfoAndMerchantSFR

  pure $ mkOrderStatusResponse $ makeOrderStatusResponse
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
  -> D.OrderStatusResponse
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
  =

-- <== is equal to =>> and used for quick visual understanding
  extract $ buildOrderStatusResponse

    -- finish
    <== changeMerchantPGR mMerchantPgr

    <== changeChargeBacks mChargebacks

    <== changeRefund mRefunds

    <== changeSecondFactorResponse secondFactorResp
    <== changeTxnFlowInfo txnFlowInfo

    <== changeCard (getCardDetails mTxnCard mTxn $ request ^. _sendCardIsin)

    <== changePaymentMethodType (getPaymentMethodType mTxnCard)

    <== changeEmiPaymentMethod mCardBrand

    <== changeAuthType (getAuthType mTxnCard)
    <== uncurry changeEmiTenureEmiBank (getEmiTenureEmiBank mTxn)

    <== changePaymentMethodAndTypeAndVpa paymentMethodsAndTypes

    <== changeRisk mRisk

    <== changeTxnDetails mTxn
    <== changeGatewayPayload (getGatewayPayload mTxn)
    <== changeBankErrorMessage (getBankErrorMessage mTxn)
    <== changeBankErrorCode (getBankErrorCode mTxn)
    <== changeGatewayRefId gatewayRefId
    <== changeGatewayId (getGatewayId mTxn)
    <== changeTxnUuid (getTxnUuid mTxn)
    <== changeTxnId (getTxnId mTxn)
    <== changeStatusId (getStatusId mTxn)
    <== changeStatus (getTxnStatus mTxn)

    <== changeMandate mMandate

    -- changeAmountAfterPromotion should follow after changePromotion
    <== changeAmountAfterPromotion mPromotionActive
    <== changePromotion mPromotionActive

    <== changeUtf (order ^. _udf)
    <== changeReturnUrl mReturnUrl
    <== changeCustomerPhone (if request ^. _isAuthenticated then order ^. _customerPhone else Just "")
    <== changeCustomerEmail (if request ^. _isAuthenticated then order ^. _customerEmail else Just "")
    <== changeDateCreated (order ^. _dateCreated)
    <== changeAmountRefunded (order ^. _amountRefunded)
    <== changePaymentLinks paymentLinks
    <== changeRefunded (order ^. _refundedEntirely)
    <== changeCurrency (order ^. _currency)
    <== changeAmount (order ^. _amount)
    <== changeStatus (Just $ D.OStatus $ C.toOrderStatusEx $ order ^. _orderStatus)
    <== changeProductId (order ^. _productId)
    <== changeCustomerId (order ^. _customerId)
    <== changeOrderId (order ^. _orderId)
    <== changeMerchantId (order ^. _merchantId)
    <== changeId (order ^. _orderUuid)
    -- start


changeId :: Text -> OrderStatusResponseBuilder -> D.OrderStatusResponse
changeId orderUuid builder = builder $ mempty {idT = Just $ First orderUuid}

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

changeStatus :: Maybe D.OrderTxnStatus -> OrderStatusResponseBuilder -> D.OrderStatusResponse
changeStatus status builder = builder $ mempty {statusT = fmap Last status}

changeCurrency :: C.Currency -> OrderStatusResponseBuilder -> D.OrderStatusResponse
changeCurrency currency builder = builder $ mempty {currencyT = Just $ Last currency}

changeRefunded :: Bool -> OrderStatusResponseBuilder -> D.OrderStatusResponse
changeRefunded refunded builder = builder $ mempty {refundedT = Just $ Last refunded}

changePaymentLinks :: D.Paymentlinks -> OrderStatusResponseBuilder -> D.OrderStatusResponse
changePaymentLinks paymentLinks builder = builder $ mempty {payment_linksT = Just $ Last paymentLinks}

changeAmountRefunded :: C.Money -> OrderStatusResponseBuilder -> D.OrderStatusResponse
changeAmountRefunded amountRefunded builder = builder $ mempty {amount_refundedT = Last <$> Just amountRefunded}

changeDateCreated :: LocalTime -> OrderStatusResponseBuilder -> D.OrderStatusResponse
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
      newAmount = (fromMaybe mempty mOldAmount) <> mOldPromotion
  in builder mempty { amountT = Just $ Last newAmount }


changeMandate :: Maybe D.Mandate -> OrderStatusResponseBuilder -> D.OrderStatusResponse
changeMandate mandate builder = builder $ mempty { mandateT = fmap Last mandate}


changeStatusId :: Maybe Int -> OrderStatusResponseBuilder -> D.OrderStatusResponse
changeStatusId statusId builder = builder $ mempty {status_idT = fmap Last statusId}

changeTxnId :: Maybe Text -> OrderStatusResponseBuilder -> D.OrderStatusResponse
changeTxnId txnId builder = builder $ mempty {txn_idT = fmap Last txnId}

changeTxnUuid :: Maybe Text -> OrderStatusResponseBuilder -> D.OrderStatusResponse
changeTxnUuid txnUuid builder = builder $ mempty {txn_uuidT = fmap Last txnUuid}

changeGatewayId :: Maybe Int -> OrderStatusResponseBuilder -> D.OrderStatusResponse
changeGatewayId gatewayId builder = builder $ mempty {gateway_idT = fmap Last gatewayId}

changeGatewayRefId :: Text -> OrderStatusResponseBuilder -> D.OrderStatusResponse
changeGatewayRefId gatewayRefId builder = builder $ mempty {gateway_reference_idT = Just $ Last gatewayRefId}

changeBankErrorCode :: Maybe Text -> OrderStatusResponseBuilder -> D.OrderStatusResponse
changeBankErrorCode bankErrorCode builder = builder $ mempty {bank_error_codeT = fmap Last bankErrorCode}

changeBankErrorMessage :: Maybe Text -> OrderStatusResponseBuilder -> D.OrderStatusResponse
changeBankErrorMessage bankErrorMessage builder = builder $ mempty {bank_error_messageT = fmap Last bankErrorMessage}

changeGatewayPayload :: Maybe Text -> OrderStatusResponseBuilder -> D.OrderStatusResponse
changeGatewayPayload gatewayPayload builder = builder $ mempty {gateway_payloadT = fmap Last gatewayPayload}

changeTxnDetails :: Maybe D.TxnDetail -> OrderStatusResponseBuilder -> D.OrderStatusResponse
changeTxnDetails txnDetail builder = builder $ mempty {txn_detailT = fmap Last txnDetail}

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
      logWarningT "get_last_txn"
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
    _ -> pure Nothing


getPaymentLinks :: Maybe Text -> Text ->  Flow D.Paymentlinks
getPaymentLinks resellerId orderUuid = do
  mResellerAccount <- loadReseller resellerId
  let mResellerEndpoint = maybe Nothing (^. _resellerApiEndpoint) mResellerAccount
  pure $ createPaymentLinks orderUuid mResellerEndpoint


createPaymentLinks
  :: Text           -- orderUuid (possibly blank string)
  -> Maybe Text     -- maybeResellerEndpoint
  -> D.Paymentlinks
createPaymentLinks orderUuid maybeResellerEndpoint =
  let config = getECRConfig
      protocol = config ^. _protocol
      host = maybe (protocol <> "://" <> (config ^. _host)) P.id maybeResellerEndpoint
  in D.Paymentlinks
    { web =   (host <> "/merchant/pay/") <> orderUuid
    , mobile = (host <> "/merchant/pay/") <> orderUuid <> "?mobile=true"
    , iframe = (host <> "/merchant/ipay/") <> orderUuid
    }

getReturnUrl :: C.MerchantId -> Maybe Text -> Maybe Text -> Flow (Maybe Text)
getReturnUrl merchantId merchantReturnUrl orderReturnUrl = do
  merchantIframePreferences <- loadMerchantPrefsMaybe merchantId
  let merchantIframeReturnUrl = (^. _returnUrl) =<< merchantIframePreferences
  pure $ orderReturnUrl <|> merchantReturnUrl <|> merchantIframeReturnUrl


getPromotion :: C.OrderPId -> C.OrderId -> Flow (Maybe D.PromotionActive)
getPromotion orderPId orderId = do
  proms <- loadPromotions orderPId
  getActivePromotion orderId proms


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
    Just (ordM :: D.OrderMetadataV2) ->
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
      C.AXIS_UPI    -> findEntry "payerVpa" (findAXIS_UPI "payload" "customerVpa" "" pgrXml) pgrXml
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
      C.GOCASHFREE  -> findGOCASHFREE "payersVPA" "" pgrXml
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
            Nothing -> do
              logErrorT "getTxnFlowInfoAndMerchantSFR" "AuthReqParams decoding failed"
              throwException Errs.internalError
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
      let MerchantPGRServiceByGateway{..} = mkMerchantPGRService gateway
      let merchantPgr = transformMpgrByGateway txn pgr pgrXml
      let gatewayResp = getGatewayResponseInJson pgr shouldSendFullGatewayResponse
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

  let rulesDecoded = B64.decode $ T.encodeUtf8 rulesTxt
  rulesjson <- case rulesDecoded of
    Right result -> pure $ E.decryptEcb (E.Key ecCred :: E.Key E.AES256 ByteString) result
    Left err     -> do
      logErrorT "decryptPromotionRules" $ T.pack err
      throwException Errs.internalError

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

getPaymentMethodType :: Maybe D.TxnCardInfo -> Maybe M.PaymentMethodType
getPaymentMethodType Nothing = Nothing
getPaymentMethodType (Just txnCard) = if isBlankMaybe (txnCard ^. _cardIsin)
  then Just M.CARD
  else Nothing

getEmiPaymentMethod :: Maybe Text -> Maybe Text -> Maybe Text
getEmiPaymentMethod cardIsin (Just cardBrand) = if isBlankMaybe cardIsin then Just cardBrand else Nothing
getEmiPaymentMethod cardIsin Nothing = if isBlankMaybe cardIsin then Just "UNKNOWN" else Nothing

getBankErrorMessage :: Maybe D.TxnDetail -> Maybe Text
getBankErrorMessage Nothing    = Nothing
getBankErrorMessage (Just txn) = txn ^. _bankErrorMessage

getBankErrorCode :: Maybe D.TxnDetail -> Maybe Text
getBankErrorCode Nothing    = Nothing
getBankErrorCode (Just txn) = txn ^. _bankErrorCode

getGatewayId :: Maybe D.TxnDetail -> Maybe Int
getGatewayId Nothing    = Nothing
getGatewayId (Just txn) = maybe Nothing C.gatewayIdFromGateway $ txn ^. _gateway

getTxnStatus :: Maybe D.TxnDetail -> Maybe D.OrderTxnStatus
getTxnStatus Nothing    = Nothing
getTxnStatus (Just txn) = Just $ D.TStatus $ txn ^. _status

getTxnUuid :: Maybe D.TxnDetail -> Maybe Text
getTxnUuid Nothing    = Nothing
getTxnUuid (Just txn) = txn ^. _txnUuid

getTxnId :: Maybe D.TxnDetail -> Maybe Text
getTxnId Nothing    = Nothing
getTxnId (Just txn) = Just $ txn ^. _txnId

getStatusId :: Maybe D.TxnDetail -> Maybe Int
getStatusId Nothing    = Nothing
getStatusId (Just txn) = Just $ txnStatusToInt $ txn ^. _status

getGatewayPayload :: Maybe D.TxnDetail -> Maybe Text
getGatewayPayload Nothing = Nothing
getGatewayPayload (Just txn) = if isBlankMaybe gatewayPayload then gatewayPayload else Nothing
  where gatewayPayload = txn ^. _gatewayPayload

getAuthType :: Maybe D.TxnCardInfo -> Maybe Text
getAuthType Nothing    = Nothing
getAuthType (Just txnCard) = txnCard ^. _authType

getEmiTenureEmiBank :: Maybe D.TxnDetail -> (Maybe Int, Maybe Text)
getEmiTenureEmiBank (Just txn) = if isTrueMaybe  (txn ^. _isEmi)
    then (txn ^. _emiTenure, txn ^. _emiBank)
    else (Nothing, Nothing)
getEmiTenureEmiBank Nothing = (Nothing, Nothing)

-- EHS: find a good module for it
txnStatusToInt :: TxnStatus -> Int
txnStatusToInt AUTHENTICATION_FAILED = 26
txnStatusToInt AUTHORIZATION_FAILED = 27
txnStatusToInt AUTHORIZING = 28
txnStatusToInt CHARGED = 21
txnStatusToInt JUSPAY_DECLINED = 22
txnStatusToInt PENDING_VBV = 23
txnStatusToInt STARTED = 20
txnStatusToInt VBV_SUCCESSFUL = 24
txnStatusToInt AUTHORIZED = 25
txnStatusToInt COD_INITIATED = 29
txnStatusToInt VOIDED = 31
txnStatusToInt VOID_INITIATED = 32
txnStatusToInt NOP = -1
txnStatusToInt CAPTURE_INITIATED = 33
txnStatusToInt CAPTURE_FAILED = 34
txnStatusToInt VOID_FAILED = 35
-- txnStatusToInt NEW = 10
-- txnStatusToInt _ = -1

fromAwait :: D.OrderStatusRequest -> Flow (Either AwaitingError a) -> Flow a
fromAwait request resultAwait = do
  result <- resultAwait
  case result of
    Right res -> pure res
    Left err -> do
        logErrorT "execOrderStatus"
          $  "errors while executing request: " <> show request
          <> ", errors: " <> show err
        throwException Errs.internalError
