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
import           Data.Generics.Product.Fields
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
import           Euler.API.RouteParameters (RouteParameters(..), OrderId, lookupRP)
import           Euler.API.Transaction
import           Euler.API.Types

import qualified Euler.Common.Metric as Metric
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
import           Euler.Common.Types.TxnDetail
import           Euler.Common.Utils
import           Euler.Config.Config as Config

import qualified Euler.Product.Domain as D

import           Euler.Product.OLTP.Card.Card
import           Euler.Product.OLTP.Services.AuthenticationService (extractApiKey, getMerchantId)

import qualified Euler.Storage.Types as DB

import           Euler.Storage.Repository

import           Euler.Storage.DBConfig

import           Euler.Version.Services.OrderStatusResponse

import           Database.Beam ((&&.), (/=.), (<-.), (==.))
import qualified Database.Beam as B
import qualified Database.Beam.Backend.SQL as B



myerr    n = err403 { errBody = "Err # " <> n }
myerr400 n = err400 { errBody = "Err # " <> n }


eulerUpiGateways :: [Gateway]
eulerUpiGateways = [HDFC_UPI, INDUS_UPI, KOTAK_UPI, SBI_UPI, ICICI_UPI, HSBC_UPI, VIJAYA_UPI, YESBANK_UPI, PAYTM_UPI]


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
        , merchantId      = getField @"merchantId" merchantAccount
        , resellerId      = getField @"resellerId" merchantAccount
        , isAuthenticated = True
        , sendCardIsin    = fromMaybe False $ getField @"enableSendingCardIsin" merchantAccount
        , txnId           = undefined :: Maybe Text
        , sendFullGatewayResponse = getSendFullGatewayResponse rps
        }

  response <- execOrderStatusQuery query

  pure $ mapLeft (const FlowError) response


getSendFullGatewayResponse :: RouteParameters -> Bool
getSendFullGatewayResponse routeParams =
  case Map.lookup "options.add_full_gateway_response" (unRP routeParams) of
    Nothing  -> False
    Just str -> str == "1" || T.map toLower str == "true"


execOrderStatusQuery :: OrderStatusQuery -> Flow (Either Text OrderStatusResponse)
execOrderStatusQuery query@OrderStatusQuery{..} = do

  orderRef <- getOrderReference query  -- TODO loadOrder orderId merchantId

  let orderUuid = fromMaybe T.empty $ getField @"orderUuid" orderRef

  links <- getPaymentLinks resellerId orderUuid

  mPromotion' <- getPromotion orderRef
  mMandate' <- getMandate orderRef

  mTxnDetail1 <- getTxnFromTxnUuid orderRef txnId
  mTxnDetail2 <- getLastTxn orderRef
  let mTxn = (mTxnDetail1 <|> mTxnDetail2)

  gatewayRefId <- case mTxn of
    Nothing -> getGatewayReferenceId2 Nothing orderRef
    Just txn -> do
      let gateway = getField @"gateway" txn
      getGatewayReferenceId2 gateway orderRef

  (mRisk, mTxnCard, mRefunds', mChargeback', mMerchantPgr) <- case mTxn of
    Nothing -> pure (Nothing, Nothing, Nothing, Nothing, Nothing)
    Just txn -> do
      let txnDetail_id = D.txnDetail_id $ getField @"id" txn
      mRisk <- getRisk $ show txnDetail_id
      mTxnCard <- loadTxnCardInfo $ show txnDetail_id
      mRefunds' <- maybeList <$> (refundDetails txnDetail_id)
      mChargeback' <- maybeList <$> (chargebackDetails txnDetail_id $ mapTxnDetail txn)
      mMerchantPgr <- getMerchantPGR txn sendFullGatewayResponse
      pure (mRisk, mTxnCard, mRefunds', mChargeback', mMerchantPgr)

  mCardBrand <- case mTxnCard of
    Nothing                    -> pure Nothing
    Just (card :: DB.TxnCardInfo) ->
      getCardBrandFromIsin (fromMaybe "" $ getField @"cardIsin" card)

  returnUrl <- getReturnUrl orderRef

  paymentMethodsAndTypes <- case (mTxn, mTxnCard) of
    (Just txn, Just txnCard) -> getPaymentMethodAndType txn txnCard
    _                        -> pure (Nothing, Nothing, Nothing, Nothing)

  txnFlowInfoAndMerchantSFR <- case (mTxn, mTxnCard) of
    (Just txn, Just txnCard) -> do
      let txnDetail_id = D.txnDetail_id $ getField @"id" txn
      getTxnFlowInfoAndMerchantSFR txnDetail_id txnCard
    (Nothing, Nothing) -> pure (Nothing, Nothing)

  pure $ runExcept $ makeOrderStatusResponse
    orderRef
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
  :: DB.OrderReference
  -> Paymentlinks
  -> Maybe Promotion'
  -> Maybe Mandate'
  -> OrderStatusQuery
  -> Maybe D.TxnDetail
  -> Text
  -> Maybe Risk
  -> Maybe DB.TxnCardInfo
  -> Maybe Text
  -> Maybe [Refund']
  -> Maybe [Chargeback']
  -> Text
  -> (Maybe Text, Maybe Text, Maybe Text, Maybe Text)
  -> (Maybe TxnFlowInfo, Maybe MerchantSecondFactorResponse)
  -> Maybe MerchantPaymentGatewayResponse
  -> Except Text OrderStatusResponse
makeOrderStatusResponse
  ordRef
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

  ordId <- liftEither $ maybe (Left "4") Right $ getField @ "orderUuid" ordRef

  let mCustomerId = whenNothing (getField @"customerId" ordRef) (Just "")
      email = (\email -> if isAuthenticated then email else Just "") (getField @"customerEmail" ordRef)
      phone = (\phone -> if isAuthenticated then phone else Just "") (getField @"customerPhone" ordRef)
      amount = fmap sanitizeAmount $ getField @ "amount" ordRef
      amountRefunded = fmap sanitizeAmount $ getField @"amountRefunded" ordRef

      getStatus = show . getField @"status"
      getStatusId = txnStatusToInt . getField @"status"
      getGatewayId txn = maybe 0 gatewayIdFromGateway $ getField @"gateway" txn
      getBankErrorCode txn = whenNothing (getField @"bankErrorCode" txn) (Just "")
      getBankErrorMessage txn = whenNothing (getField @"bankErrorMessage" txn) (Just "")
      getGatewayPayload txn = if isBlankMaybe (mGatewayPayload' txn) then (mGatewayPayload' txn) else Nothing
        where mGatewayPayload' t = getField @"gatewayPayload" t

      isEmi txn = isTrueMaybe (getField @"isEmi" txn)
      emiTenure txn = getField @"emiTenure" txn
      emiBank txn = getField @"emiBank" txn

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

    <<= maybeTxnAndTxnCard (\txn txnCard -> if isBlankMaybe (getField @"cardIsin" txnCard)
        then changeCard (getCardDetails txnCard txn sendCardIsin)
        else emptyBuilder)

    <<= maybeTxnAndTxnCard (\_ txnCard -> if isBlankMaybe (getField @"cardIsin" txnCard)
        then changePaymentMethodType "CARD"
        else emptyBuilder)

    <<= maybeTxnAndTxnCard (\_ txnCard -> if isBlankMaybe (getField @"cardIsin" txnCard)
        then changeEmiPaymentMethod paymentMethod
        else emptyBuilder)

    <<= maybeTxnAndTxnCard (\_ txnCard -> changeAuthType $ whenNothing (getField @"authType" txnCard) (Just ""))
    <<= maybeTxnAndTxnCard (\txn _ -> if isEmi txn then changeEmiTenureEmiBank (emiTenure txn) (emiBank txn) else emptyBuilder)

    <<= maybeTxnAndTxnCard (\_ _ -> changePaymentMethodAndTypeAndVpa paymentMethodsAndTypes)

    <<= changeRisk mRisk

    <<= maybeTxn (changeTxnDetails . mapTxnDetail)
    <<= maybeTxn (changeGatewayPayload . getGatewayPayload)
    <<= maybeTxn (changeBankErrorMessage . getBankErrorMessage)
    <<= maybeTxn (changeBankErrorCode . getBankErrorCode)
    <<= maybeTxn (const $ changeGatewayRefId gatewayRefId)
    <<= maybeTxn (changeGatewayId . getGatewayId)
    <<= maybeTxn (changeTxnUuid . getField @"txnUuid")
    <<= maybeTxn (changeTxnId . getField @"txnId")
    <<= maybeTxn (changeStatusId . getStatusId)
    <<= maybeTxn (changeStatus . getStatus)

    <<= changeMandate mMandate

    <<= changeAmountAfterPromotion mPromotion

    <<= changePromotion mPromotion

    <<= changeUtf10 (getField @"udf10" ordRef)
    <<= changeUtf9 (getField @"udf9" ordRef)
    <<= changeUtf8 (getField @"udf8" ordRef)
    <<= changeUtf7 (getField @"udf7" ordRef)
    <<= changeUtf6 (getField @"udf6" ordRef)
    <<= changeUtf5 (getField @"udf5" ordRef)
    <<= changeUtf4 (getField @"udf4" ordRef)
    <<= changeUtf3 (getField @"udf3" ordRef)
    <<= changeUtf2 (getField @"udf2" ordRef)
    <<= changeUtf1 (getField @"udf1" ordRef)
    <<= changeReturnUrl returnUrl
    <<= changeCustomerPhone phone
    <<= changeCustomerEmail email
    <<= changeDateCreated (show $ getField @"dateCreated" ordRef)
    <<= changeAmountRefunded amountRefunded
    <<= changePaymentLinks paymentLinks
    <<= changeRefunded (getField @"refundedEntirely" ordRef)
    <<= changeCurrency (getField @"currency" ordRef)
    <<= changeAmount amount
    <<= changeStatus (show $ getField @ "status" ordRef)
    <<= changeProductId (getField @"productId" ordRef)
    <<= changeCustomerId mCustomerId
    <<= changeOrderId (getField @"orderId" ordRef)
    <<= changeMerchantId (getField @"merchantId" ordRef)
    <<= changeId ordId


changeId :: Text -> ResponseBuilder -> OrderStatusResponse
changeId orderId builder = builder $ mempty {idT = Just $ First orderId}

changeMerchantId :: Maybe Text -> ResponseBuilder -> OrderStatusResponse
changeMerchantId mMerchantId builder = builder $ mempty {merchant_idT = fmap First mMerchantId}

changeAmount :: Maybe Double -> ResponseBuilder -> OrderStatusResponse
changeAmount amount builder = builder $ mempty {amountT = fmap Last amount}

changeOrderId :: Maybe Text -> ResponseBuilder -> OrderStatusResponse
changeOrderId orderId builder = builder $ mempty {order_idT = fmap First orderId}

changeCustomerId :: Maybe Text -> ResponseBuilder -> OrderStatusResponse
changeCustomerId customerId builder = builder $ mempty {customer_idT = fmap Last customerId}

changeProductId :: Maybe Text -> ResponseBuilder -> OrderStatusResponse
changeProductId productId builder = builder $ mempty {product_idT = fmap Last productId}

changeStatus :: Text -> ResponseBuilder -> OrderStatusResponse
changeStatus status builder = builder $ mempty {statusT = Just $ Last status}

changeCurrency :: Maybe Text -> ResponseBuilder -> OrderStatusResponse
changeCurrency currency builder = builder $ mempty {currencyT = map Last currency}

changeRefunded :: Maybe Bool -> ResponseBuilder -> OrderStatusResponse
changeRefunded refunded builder = builder $ mempty {refundedT = map Last refunded}

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
      mOldAmount = getField @"amount" oldStatus
      mOldPromotion = getField @"discount_amount" newProm
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


getOrderReference
  :: OrderStatusQuery
  -> Flow DB.OrderReference
getOrderReference query = do
  let orderId' = getField @"orderId" query
  let merchantId' = getField @"merchantId" query

  conn <- getConn eulerDB
  res <- runDB conn $ do
    let predicate DB.OrderReference {orderId, merchantId} = (orderId ==. B.just_ (B.val_ orderId'))
          &&. (merchantId ==. B.just_ (B.val_ merchantId'))
    findRow
      $ B.select
      $ B.limit_ 1
      $ B.filter_ predicate
      $ B.all_ (DB.order_reference DB.eulerDBSchema)
  case res of
    Right (Just ordRef) -> pure ordRef
    Right Nothing -> throwException err404 {errBody = "Order " <> show orderId' <> " not found."}
    Left err -> do
      logError "Find OrderReference" $ toText $ P.show err
      throwException err500


getTxnFromTxnUuid :: DB.OrderReference -> Maybe Text -> Flow (Maybe D.TxnDetail)
getTxnFromTxnUuid order maybeTxnUuid = do
  -- EHS: TODO should we throw exceptions?
      -- orderId' <- whenNothing (getField @"orderId" order) (throwException err500)
      -- merchantId' <- whenNothing (getField @"merchantId" order) (throwException err500)

      findTxnByOrderIdMerchantIdTxnuuidId
        (getField @"orderId" order)
        (getField @"merchantId" order)
        maybeTxnUuid

-- EHS: TODO add sorting by dateCreated!
getLastTxn :: DB.OrderReference -> Flow (Maybe D.TxnDetail)
getLastTxn orderRef = do
  -- EHS: TODO should we throw exceptions?
  -- orderId' <- whenNothing (getField @"orderId" orderRef) (throwException err500)
  -- merchantId' <- whenNothing (getField @"merchantId" orderRef) (throwException err500)
  let orderId = getField @"orderId" orderRef
  let merchantId = getField @"merchantId" orderRef

  txnDetails <- findTxnByOrderIdMerchantId orderId merchantId

  case txnDetails of
    [] -> do
      logError "get_last_txn"
        $ "No last txn found for orderId: " <> show orderId
        <> " :merchant:" <> show merchantId
      pure Nothing
    _ -> do
      let chargetxn = find (\txn -> (getField @"status" txn == CHARGED)) txnDetails
      maybe (pure . Just $ head txnDetails) (pure . Just) chargetxn


getMandate :: DB.OrderReference -> Flow (Maybe Mandate')
getMandate ordRef = do
  let id = getField @"id" ordRef
      merchId = getField @"merchantId" ordRef
      orderType = getField @"orderType" ordRef

  case (id, merchId, orderType) of
    (Just id', Just merchantId, Just C.MANDATE_REGISTER) -> do

      mandate <- loadMandate id' merchantId
      pure $ mapMandate <$> mandate

    _ -> pure Nothing


getPaymentLinks :: Maybe Text -> Text ->  Flow Paymentlinks
getPaymentLinks resellerId orderUuid = do
  mResellerAccount <- maybe (pure Nothing) loadResellerAccount resellerId
  let mResellerEndpoint = maybe Nothing (getField @"resellerApiEndpoint") mResellerAccount
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
    protocol = getField @"protocol" config
    host = maybe (protocol <> "://" <> (getField @"host" config)) P.id maybeResellerEndpoint

getReturnUrl ::  DB.OrderReference -> Flow Text
getReturnUrl orderRef = do
  conn <- getConn eulerDB
  merchantAccount <- do
    res <- runDB conn $ do
      let predicate DB.MerchantAccount {merchantId} = merchantId ==. B.just_ (B.val_ $ fromMaybe "" $ orderRef ^. _merchantId)
      findRow
        $ B.select
        $ B.limit_ 1
        $ B.filter_ predicate
        $ B.all_ (DB.merchant_account DB.eulerDBSchema)
    case res of
      Right mMAcc -> pure mMAcc
      Left err -> do
        logError "Find MerchantAccount" $ toText $ P.show err
        throwException err500
  case merchantAccount of
    Just merchantAcc -> do
          merchantIframePreferences <- do
            res <- runDB conn $ do
              let predicate DB.MerchantIframePreferences {merchantId} = merchantId ==. (B.val_ $ fromMaybe "" $ merchantAcc ^. _merchantId)
              findRow
                $ B.select
                $ B.limit_ 1
                $ B.filter_ predicate
                $ B.all_ (DB.merchant_iframe_preferences DB.eulerDBSchema)
            case res of
              Right mMIP -> pure mMIP
              Left err -> do
                logError "SQLDB Interraction." $ toText $ P.show err
                throwException err500
          let merchantIframeReturnUrl = fromMaybe "" (getField @"returnUrl"  =<< merchantIframePreferences)
              orderRefReturnUrl       = fromMaybe "" (getField @"returnUrl" orderRef )
          if (orderRefReturnUrl == "")
            then pure $ fromMaybe merchantIframeReturnUrl (getField @"returnUrl" merchantAcc )
            else pure orderRefReturnUrl
    Nothing -> pure $ ""


loadPromotions :: DB.OrderReference -> Flow (Text, [DB.Promotions])
loadPromotions orderRef = do
  let id = fromMaybe 0 $ getField @"id" orderRef
      orderId = fromMaybe "" $ getField @"orderId" orderRef
  promotions <- do
    conn <- getConn eulerDB
    res  <- runDB conn $ do
      let predicate DB.Promotions{orderReferenceId} =
            orderReferenceId ==. B.just_ (B.val_ id)
      findRows
        $ B.select
        $ B.filter_ predicate
        $ B.all_ (DB.promotions DB.eulerDBSchema)
    case res of
      Right proms -> pure proms
      Left err -> do
        logError "Find Promotions" $ toText $ P.show err
        throwException err500
  pure (orderId, promotions)

decryptActivePromotion :: (Text, [DB.Promotions]) -> Flow (Maybe Promotion')
decryptActivePromotion (_,[]) = pure Nothing
decryptActivePromotion (ordId, promotions) = do
  let mPromotion = find (\promotion -> (getField @"status" promotion == "ACTIVE" )) promotions
  traverse (decryptPromotionRules ordId) mPromotion

decryptPromotionRules :: Text -> DB.Promotions -> Flow Promotion'
decryptPromotionRules ordId promotions = pure defaultPromotion'

getPromotion :: DB.OrderReference -> Flow (Maybe Promotion')
getPromotion orderRef = do
  proms <- loadPromotions orderRef
  decryptActivePromotion  proms


mapTxnDetail :: D.TxnDetail -> TxnDetail'
mapTxnDetail txn = TxnDetail'
  { txn_id = getField @"txnId" txn
  , order_id = getField @"orderId" txn
  , txn_uuid = getField @"txnUuid" txn
  , gateway_id = Just $ maybe 0 gatewayIdFromGateway $ getField @"gateway" txn
  , status = show $ getField @"status" txn
  , gateway = show <$> getField @"gateway" txn
  , express_checkout = getField @"expressCheckout" txn
  , redirect = getField @"redirect" txn
  , net_amount = Just $ if isJust (getField @"netAmount" txn)
      then show $ maybe 0 fromMoney (getField @"netAmount" txn) -- Forign becomes Text in our TxnDetail'
      else mempty
  , surcharge_amount = Just $ if isJust (getField @"surchargeAmount" txn)
      then show $ maybe 0 fromMoney (getField @"surchargeAmount" txn)
      else mempty
  , tax_amount = Just $ if isJust (getField @"taxAmount" txn)
      then show $ maybe 0 fromMoney (getField @"taxAmount" txn)
      else mempty
  , txn_amount = Just $ if isJust (getField @"txnAmount" txn)
      then show $ maybe 0 fromMoney (getField @"txnAmount" txn)
      else mempty
  , currency = getField @"currency" txn
  , error_message = Just $ fromMaybe mempty $ getField @"bankErrorMessage" txn
  , error_code = Just $ if isJust (getField @"bankErrorCode" txn)
      then fromMaybe mempty (getField @"bankErrorCode" txn)
      else mempty
  , created = getField @"dateCreated" txn
  , txn_object_type = if (fromMaybe mempty $ getField @"txnObjectType" txn) /= "ORDER_PAYMENT"
      then getField @"txnObjectType" txn
      else Nothing
  , source_object = if (fromMaybe mempty $ getField @"txnObjectType" txn) /= "ORDER_PAYMENT"
      then getField @"sourceObject" txn
      else Nothing
  , source_object_id = if (fromMaybe mempty $ getField @"txnObjectType" txn) /= "ORDER_PAYMENT"
      then getField @"sourceObjectId" txn
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


getGatewayReferenceId2 :: Maybe Gateway -> DB.OrderReference -> Flow Text
getGatewayReferenceId2 gateway ordRef = do
  let checkGateway = checkGatewayRefIdForVodafone2 ordRef gateway
  let gatewayT = maybe "" show gateway

  case getField @"id" ordRef of
    Nothing -> checkGateway
    Just refId -> do
      ordMeta <- loadOrderMetadataV2 refId

      case ordMeta of
        Just (ordM :: DB.OrderMetadataV2) ->
          case blankToNothing (getField @"metadata" ordM) of
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


checkGatewayRefIdForVodafone2 :: DB.OrderReference -> Maybe Gateway -> Flow Text
checkGatewayRefIdForVodafone2 ordRef gateway = do
  merchantId' <- whenNothing (getField @"merchantId" ordRef) (throwException err500)

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
            && (getField @"enabled" feature)
            && (isJust $ getField @"udf2" ordRef)
        then pure $ fromMaybe "" $ getField @"udf2" ordRef
        else pure mempty
    Nothing -> pure mempty




loadTxnRiskCheck :: Text -> Flow (Maybe DB.TxnRiskCheck)
loadTxnRiskCheck txnId =
  withDB eulerDB $ do
    let predicate DB.TxnRiskCheck {txnDetailId} = txnDetailId ==. B.val_ txnId
    findRow
      $ B.select
      $ B.limit_ 1
      $ B.filter_ predicate
      $ B.all_ (DB.txn_risk_check DB.eulerDBSchema)


loadRiskManagementAccount :: Int -> Flow (Maybe DB.RiskManagementAccount)
loadRiskManagementAccount riskMAId =
  withDB eulerDB $ do
    let predicate DB.RiskManagementAccount {id} = id ==. B.val_ riskMAId
    findRow
      $ B.select
      $ B.limit_ 1
      $ B.filter_ predicate
      $ B.all_ (DB.risk_management_account DB.eulerDBSchema)


makeRisk' :: Maybe Text -> DB.TxnRiskCheck -> Risk'
makeRisk' provider trc = Risk'
  { provider = provider
  , status = getField @"status" trc
  , message = getField @"message" trc
  , flagged = whenNothing (getField @"flagged" trc) (Just False)
  , recommended_action = whenNothing (getField @"recommendedAction" trc) (Just T.empty)
  , ebs_risk_level = Nothing
  , ebs_payment_status = Nothing
  , ebs_risk_percentage = Nothing
  , ebs_bin_country = Nothing
  }

makeRisk :: Risk' -> Flow Risk
makeRisk risk' = if (fromMaybe T.empty (getField @"provider" risk')) == "ebs"
  then do
      -- EHS: TODO not sure is this reliable enough? how port it?
      -- completeResponseJson <- xml2Json (trc ^. _completeResponse)
      -- outputObjectResponseJson <- xml2Json (trc ^. _completeResponse)
      -- responseObj <- pure $ fromMaybe emptyObj (lookupJson "RMSIDResult" completeResponseJson)
      -- outputObj   <- pure $ fromMaybe emptyObj (maybe Nothing (lookupJson "Output") (lookupJson "RMSIDResult" outputObjectResponseJson))
    let r' = undefined :: Risk'
      -- EHS: TODO
      -- let r' = wrap (unwrap risk) {
      --   ebs_risk_level = NullOrUndefined $ maybe Nothing (Just <<< getString) (lookupJson "RiskLevel" responseObj) ,
      --   ebs_payment_status = (trc ^. _riskStatus),
      --   ebs_risk_percentage = NullOrUndefined $ maybe Nothing fromString (lookupJson "RiskPercentage" responseObj) ,
      --   ebs_bin_country = NullOrUndefined $ maybe Nothing (Just <<< getString) (lookupJson "Bincountry" (outputObj))
      -- }
    addRiskObjDefaultValueAsNull r'
  else
    addRiskObjDefaultValueAsNull risk'
    where getString a = ""

addRiskObjDefaultValueAsNull :: Risk' -> Flow Risk
addRiskObjDefaultValueAsNull risk' = do
  let risk = Risk
        { provider = getField @"provider" risk'
        , status = getField @"status" risk'
        , message = getField @"message" risk'
        , flagged = undefined :: Maybe Text -- EHS: TODO: getField @"flagged" risk' -- : if (isJust $ unNullOrUndefined (risk' ^. _flagged)) then just (toForeign (unNull (risk' ^. _flagged) false)) else just (nullValue unit)
        , recommended_action = getField @"recommended_action" risk'
        , ebs_risk_level = Nothing
        , ebs_payment_status = Nothing
        , ebs_risk_percentage = Nothing
        , ebs_bin_country = Nothing
        }

  case (fromMaybe mempty (getField @"provider" risk')) of
    "ebs" -> pure (risk
        { ebs_risk_level = getField @"ebs_risk_level" risk'
        , ebs_payment_status = getField @"ebs_payment_status" risk'
        , ebs_risk_percentage = undefined :: Maybe Text -- EHS: TODO: getField @"ebs_risk_percentage" risk' -- if (isJust $ unNullOrUndefined (risk' ^. _ebs_risk_percentage)) then just (toForeign (unNull (risk' ^. _ebs_risk_percentage) 0)) else just (nullValue unit)
        , ebs_bin_country = getField @"ebs_bin_country" risk'
        } :: Risk)
    _ -> return risk


getRisk :: Text -> Flow (Maybe Risk)
getRisk txnId = do
  txnRiskCheck <- loadTxnRiskCheck txnId
  case txnRiskCheck of
    Just trc -> do
      let riskMAId = getField @"riskManagementAccountId" trc
      riskMngAcc <- loadRiskManagementAccount riskMAId
      let risk' = makeRisk' (getField @"provider" <$> riskMngAcc) trc
      Just <$> makeRisk risk'
    Nothing -> pure Nothing


loadTxnCardInfo :: Text -> Flow (Maybe DB.TxnCardInfo)
loadTxnCardInfo txnId =
  withDB eulerDB $ do
    let predicate DB.TxnCardInfo {txnDetailId} = txnDetailId ==. B.just_ (B.val_ txnId)
    findRow
      $ B.select
      $ B.limit_ 1
      $ B.filter_ predicate
      $ B.all_ (DB.txn_card_info DB.eulerDBSchema)


getCardDetails :: DB.TxnCardInfo -> D.TxnDetail -> Bool -> Card
getCardDetails card txn shouldSendCardIsin = Card
  { expiry_year = whenNothing (getField @"cardExpYear" card) (Just "")
  , card_reference = whenNothing (getField @"cardReferenceId" card) (Just "")
  , saved_to_locker = isSavedToLocker card txn
  , expiry_month = whenNothing  (getField @"cardExpMonth" card) (Just "")
  , name_on_card = whenNothing  (getField @"nameOnCard" card) (Just "")
  , card_issuer = whenNothing  (getField @"cardIssuerBankName" card) (Just "")
  , last_four_digits = whenNothing  (getField @"cardLastFourDigits" card) (Just "")
  , using_saved_card = getField @"expressCheckout" txn
  , card_fingerprint = whenNothing  (getField @"cardFingerprint" card) (Just "")
  , card_isin = if shouldSendCardIsin then (getField @"cardIsin" card) else Just ""
  , card_type = whenNothing  (getField @"cardType" card) (Just "")
  , card_brand = whenNothing  (getField @"cardSwitchProvider" card) (Just "")
  }
  where
    isSavedToLocker card' txn' = Just $
      isTrueMaybe (getField @"addToLocker" txn') && (isBlankMaybe $ getField @"cardReferenceId" card')


refundDetails :: TxnDetailId -> Flow [Refund']
refundDetails txnId = do
  l <- findRefunds $ show txnId
  pure $ map AO.mapRefund l


chargebackDetails :: Int -> TxnDetail' -> Flow [Chargeback']
chargebackDetails txnId txn = do
  chargebacks <- findChargebacks $ show txnId
  pure $ map (AO.mapChargeback txn) chargebacks


sanitizeAmount x = x
sanitizeNullAmount = fmap sanitizeAmount


getPaymentMethodAndType
  :: D.TxnDetail
  -> DB.TxnCardInfo
  -> Flow (Maybe Text, Maybe Text, Maybe Text, Maybe Text)
  -- ^ Result is (payment_method, payment_method_type, payer_vpa, payer_app_name)
  -- when Nothing do not change a field
getPaymentMethodAndType txn card = do
  case (getField @"cardType" card) of
    Just "NB" -> pure
      ( whenNothing (getField @"cardIssuerBankName" card) (Just T.empty)
      , Just "NB"
      , Nothing
      , Nothing
      )
    Just "WALLET" -> do
      payerVpa <- case getField @"paymentMethod" card of
        Nothing          -> pure ""
        Just "GOOGLEPAY" -> getPayerVpa $ getField @"successResponseId" txn
        Just _           -> pure ""
      pure
        ( whenNothing (getField @"cardIssuerBankName" card) (Just T.empty)
        , Just "WALLET"
        , Just payerVpa
        , Nothing
        )

    Just "UPI" -> do
      let payment_method = Just "UPI"
          payment_method_type = Just "UPI"
          paymentSource = if (fromMaybe "null" $ getField @"paymentSource" card) == "null"
            then Just ""
            else whenNothing (getField @"paymentSource" card) (Just "null")
          sourceObj = fromMaybe "" $ getField @"sourceObject" txn
      if sourceObj == "UPI_COLLECT" || sourceObj == "upi_collect"
        then pure
          ( payment_method
          , payment_method_type
          , paymentSource
          , Nothing
          )
        else do
          let respId = getField @"successResponseId" txn
          let gateway = getField @"gateway" txn
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
      ( whenNothing (getField @"cardIssuerBankName" card) (Just T.empty)
      , Just "CARD"
      , Nothing
      , Nothing
      )
    Just "REWARD" -> pure
      ( whenNothing (getField @"cardIssuerBankName" card) (Just T.empty)
      , Just "REWARD"
      , Nothing
      , Nothing
      )
    Just "ATM_CARD" -> pure
      ( whenNothing (getField @"cardIssuerBankName" card) (Just T.empty)
      , Just "CARD"
      , Nothing
      , Nothing
      )
    Just _ -> checkPaymentMethodType card
    Nothing -> checkPaymentMethodType card

  where
    checkPaymentMethodType card' = case (getField @"paymentMethodType" card') of
      Just Mandate.CASH -> pure
        ( whenNothing (getField @"paymentMethod" card') (Just T.empty)
        , Just "CASH"
        , Nothing
        , Nothing
        )
      Just Mandate.CONSUMER_FINANCE -> pure
        ( whenNothing (getField @"paymentMethod" card') (Just T.empty)
        , Just "CONSUMER_FINANCE"
        , Nothing
        , Nothing
        )
      _ -> pure (Nothing, Nothing, Nothing, Nothing)

getPayerVpa :: Maybe Int -> Flow Text
getPayerVpa mSuccessResponseId = do
  mPaymentGatewayResp <- loadPGR mSuccessResponseId
  let mXml = getField @"responseXml" =<< mPaymentGatewayResp
  pure $ case mXml of
    Nothing  -> T.empty
    Just xml -> findEntry "payerVpa" "" $ decodeXml $ T.encodeUtf8 xml

getPayerVpaByGateway :: Maybe Int -> Maybe Gateway -> Flow Text
getPayerVpaByGateway respId gateway = do
  mPgr <- loadPGR respId
  case mPgr of
    Nothing  -> pure T.empty
    Just pgr -> pure $ findPayerVpaByGateway gateway (getField @"responseXml" pgr)

findPayerVpaByGateway :: Maybe Gateway -> Maybe Text -> Text
findPayerVpaByGateway _ Nothing = T.empty
findPayerVpaByGateway gateway (Just xml) =
  case gateway of
    Just AXIS_UPI    -> findEntry "payerVpa" (findEntry "customerVpa" "" pgrXml) pgrXml
    Just HDFC_UPI    -> findEntry "payerVpa" "" pgrXml
    Just INDUS_UPI   -> findEntry "payerVpa" "" pgrXml
    Just KOTAK_UPI   -> findEntry "payerVpa" "" pgrXml
    Just SBI_UPI     -> findEntry "payerVpa" "" pgrXml
    Just ICICI_UPI   -> findEntry "payerVpa" "" pgrXml
    Just HSBC_UPI    -> findEntry "payerVpa" "" pgrXml
    Just VIJAYA_UPI  -> findEntry "payerVpa" "" pgrXml
    Just YESBANK_UPI -> findEntry "payerVpa" "" pgrXml
    Just PAYTM_UPI   -> findEntry "payerVpa" "" pgrXml
    Just PAYU        -> findEntry "field3" "" pgrXml
    Just RAZORPAY    -> findEntry "vpa" "" pgrXml
    Just PAYTM_V2    -> findEntry "VPA" "" pgrXml
    Just GOCASHFREE  -> findEntry "payersVPA" "" pgrXml
    _                -> T.empty
  where
    pgrXml = decodeXml $ T.encodeUtf8 xml

getTxnFlowInfoAndMerchantSFR
  :: Int
  -> DB.TxnCardInfo
  -> Flow (Maybe TxnFlowInfo, Maybe MerchantSecondFactorResponse)
getTxnFlowInfoAndMerchantSFR txnDetId card = do

  if (fromMaybe T.empty $ getField @"authType" card) == "VIES" then do

    mSecondFactor <- findSecondFactor $ show txnDetId

    case mSecondFactor of
      Nothing ->  pure (Nothing, Nothing)
      Just sf -> do
        authReqParams <- do
          let authReqParams = fromMaybe "{}" $ getField  @"gatewayAuthReqParams" sf
          let vies = (decode $ BSL.fromStrict $ T.encodeUtf8 authReqParams)
          case vies of
            Just v -> pure v
            Nothing -> throwException err500
              {errBody = "AuthReqParams decoding failed"}

        let txnFlowInfo = mkTxnFlowInfo authReqParams

        mSecondFactorResponse <- findSecondFactorResponse $ D.sfId $ getField @"id" sf

        let mMerchantSFR = mkMerchantSecondFactorResponse <$> mSecondFactorResponse
        pure (Just txnFlowInfo, mMerchantSFR)
    else pure (Nothing, Nothing) --NON VIES Txn


getMerchantPGR :: D.TxnDetail -> Bool -> Flow (Maybe MerchantPaymentGatewayResponse)
getMerchantPGR txn shouldSendFullGatewayResponse = do

  -- EHS: TODO OS: We need to have this clarified with Sushobhith
  -- they introduces PGRV1 in early 2020

  mPaymentGatewayResp <- loadPGR $ getField @"successResponseId" txn

  case mPaymentGatewayResp of
    Nothing -> pure Nothing
    Just pgr -> do
      let gateway = getField @"gateway" txn
      let pgrXml  = case getField @"responseXml" pgr of
            Nothing  -> Map.empty
            Just xml -> getMapFromPGRXml $ decodeXml $ T.encodeUtf8 xml
      let date = show <$> getField @"dateCreated" pgr
      let mPgr = defaultMerchantPaymentGatewayResponse' {created = date} :: MerchantPaymentGatewayResponse'
      let merchantPgr' = casematch txn pgr mPgr gateway pgrXml
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
      let xmlResp = fromMaybe T.empty $ getField @"responseXml" paymentGatewayResponse
          pgrXml = decodeXml $ T.encodeUtf8 xmlResp

      -- EHS: it need review and need authentic json data to check
      -- jsonPgr <- createJsonFromPGRXmlResponse $ getMapFromPGRXml pgrXml
      -- jsonPgr <- encode $ getMapFromPGRXml pgrXml
      in Just $ toStrict $ TL.decodeUtf8 $ encode $ getMapFromPGRXml pgrXml
    else Nothing

casematch
  :: D.TxnDetail
  -> DB.PaymentGatewayResponse
  -> MerchantPaymentGatewayResponse'
  -> Maybe Gateway
  -> Map.Map TL.Text EValue
  -> MerchantPaymentGatewayResponse'
casematch txn pgr merchPGR gateway xmls = match gateway'
  where
    -- EHS: Gateway type from TxnDetail does not match gateways from casematch totally
    gateway' = maybe "" show gateway
    match "CCAVENUE_V2"    = executePGR merchPGR xmls $ ccavenue_v2  txn pgr xmls
    match "BLAZEPAY"       = executePGR merchPGR xmls $ blazepay     txn pgr xmls
    match "STRIPE"         = executePGR merchPGR xmls $ stripe       txn pgr xmls
    match "CITI"           = executePGR merchPGR xmls $ citi         txn xmls
    match "IPG"            = executePGR merchPGR xmls $ ipg          txn xmls
    match "FSS_ATM_PIN_V2" = executePGR merchPGR xmls $ fssatmpin    txn pgr
    match "HDFC_EBS_VAS"   = executePGR merchPGR xmls $ hdfc_ebs_vas txn pgr
    match "FREECHARGE_V2"  = executePGR merchPGR xmls $ freechargev2 txn pgr
    match "FSS_ATM_PIN"    = executePGR merchPGR xmls $ fssatmpin    txn pgr
    match "AIRTELMONEY"    = executePGR merchPGR xmls $ airtelmoney  txn pgr
    match "CYBERSOURCE"    = executePGR merchPGR xmls $ cybersource  txn pgr
    match "OLAPOSTPAID"    = executePGR merchPGR xmls $ olapostpaid  txn pgr
    match "GOCASHFREE"     = executePGR merchPGR xmls $ gocashfree   txn pgr
    match "EPAYLATER"      = executePGR merchPGR xmls $ epaylater    txn pgr
    match "ZESTMONEY"      = executePGR merchPGR xmls $ zestmoney    txn pgr
    match "BILLDESK"       = executePGR merchPGR xmls $ billdesk     txn pgr
    match "JIOMONEY"       = executePGR merchPGR xmls $ jiomoney     txn pgr
    match "SBIBUDDY"       = executePGR merchPGR xmls $ sbibuddy     txn pgr
    match "RAZORPAY"       = executePGR merchPGR xmls $ razorpay     txn pgr
    match "AXIS_UPI"       = executePGR merchPGR xmls $ axisupi      txn pgr
    match "PINELABS"       = executePGR merchPGR xmls $ pinelabs     txn pgr
    match "MOBIKWIK"       = executePGR merchPGR xmls $ mobikwik     txn pgr
    match "LINEPAY"        = executePGR merchPGR xmls $ linepay      txn pgr
    match "PHONEPE"        = executePGR merchPGR xmls $ phonepe      txn pgr
    match "ICICINB"        = executePGR merchPGR xmls $ icicinb      txn pgr
    match "ZAAKPAY"        = executePGR merchPGR xmls $ zaakpay      txn pgr
    match "AIRPAY"         = executePGR merchPGR xmls $ airpay       txn pgr
    match "AXISNB"         = executePGR merchPGR xmls $ axisnb       txn pgr
    match "SODEXO"         = executePGR merchPGR xmls $ sodexo       txn pgr
    match "CITRUS"         = executePGR merchPGR xmls $ citrus       txn pgr
    match "PAYPAL"         = executePGR merchPGR xmls $ paypal       txn pgr
    match "HDFCNB"         = executePGR merchPGR xmls $ hdfcnb       txn pgr
    match "KOTAK"          = executePGR merchPGR xmls $ kotak        txn pgr
    match "MPESA"          = executePGR merchPGR xmls $ mpesa        txn pgr
    match "SIMPL"          = executePGR merchPGR xmls $ simpl        txn pgr
    match "CASH"           = executePGR merchPGR xmls $ cash         txn pgr
    match "TPSL"           = executePGR merchPGR xmls $ tpsl         txn pgr
    match "LAZYPAY"        = executePGR merchPGR xmls $ lazypay      txn pgr
    match "FSSPAY"         = executePGR merchPGR xmls $ fsspay       txn pgr
    match "AMEX"           = executePGR merchPGR xmls $ amex         txn pgr
    match "ATOM"           = executePGR merchPGR xmls $ atom         txn pgr
    match "PAYTM_V2"       = executePGR merchPGR xmls $ paytm_v2     pgr
    match "EBS_V3"         = executePGR merchPGR xmls $ ebs_v3       pgr
    match "AXIS"           = executePGR merchPGR xmls $ axis         pgr
    match "HDFC"           = executePGR merchPGR xmls $ hdfc         pgr
    match "EBS"            = executePGR merchPGR xmls $ ebs          pgr
    match "MIGS"           = executePGR merchPGR xmls $ migs         pgr
    match "ICICI"          = executePGR merchPGR xmls $ icici        pgr
    match "PAYLATER"       = executePGR merchPGR xmls $ paylater     txn
    match "DUMMY"          = executePGR merchPGR xmls $ dummy
    match "FREECHARGE"     = executePGR merchPGR xmls (freecharge txn pgr)
      & \upgr -> if campaignCode /= "NA"
          then upgr
            { offer           = Just campaignCode
            , offer_type      = justNa
            , offer_availed   = Just "NA"
            , discount_amount = Just T.empty
            } :: MerchantPaymentGatewayResponse'
          else upgr
            -- EPS: Freecharge doesn't return any information about offers in their response
      where
        campaignCode = lookupXML xmls "campaignCode" "NA"

    match "PAYU"           = executePGR merchPGR xmls (payu pgr)
      & \upgr -> if offer /= "NA"
      -- EPS: * Payu allows merchants to send multiple offers and avails a valid offer among them
      -- EPS: * offer_availed contains the successfully availed offer.
          then
            (\rec -> if offerFailure /= "null"
              then rec{ offer_failure_reason = Just offerFailure } :: MerchantPaymentGatewayResponse'
              else rec)
            (upgr
            { offer           = Just offerVal
            , offer_type      = offerType
            , offer_availed   = Just $ show offerAvailed
            , discount_amount = Just disAmount
            } :: MerchantPaymentGatewayResponse')
          else upgr
      where
          offer        = lookupXML     xmls "offer"                 "NA"
          offerVal     = lookupXMLKeys xmls "offer_availed" "offer" "null"
          offerType    = Just $ lookupXML xmls "offer_type" "null"
          offerFailure = lookupXML     xmls "offer_failure_reason"  "null"
          discount     = lookupXML     xmls "discount"              "NA"
          offerAvailed = offerVal /= "null"
          disAmount    = maybe T.empty show (readMaybe $ T.unpack discount :: Maybe Int)

    match "PAYTM" = executePGR merchPGR xmls (paytm pgr)
      & \upgr -> if promoCampId /= "null"
        then upgr
          { offer         = Just promoCampId
          , offer_type    = justNa
          , offer_availed = Just $ show $ promoStatus == "PROMO_SUCCESS"
          , discount_amount = Just T.empty
          } :: MerchantPaymentGatewayResponse'
        else upgr
      where
        promoCampId = lookupXML xmls "PROMO_CAMP_ID" "null"
        promoStatus = lookupXML xmls "PROMO_STATUS"  "null"

    match "OLAMONEY" = executePGR merchPGR xmls (olamoney txn pgr)
      & \upgr -> if couponCode /= "null"
        then upgr
          { offer         = Just couponCode
          , offer_type    = justNa
          , offer_availed = Just $ show $ strToBool isCashbackSuc
          , discount_amount = Just T.empty
          } :: MerchantPaymentGatewayResponse'
        else upgr
      where
        couponCode    = lookupXML xmls "couponCode"           "null"
        isCashbackSuc = lookupXML xmls "isCashbackSuccessful" "null"
        strToBool :: Text -> Bool
        strToBool "true" = True
        strToBool _      = False

    match "AMAZONPAY" = executePGR merchPGR xmls (amazonpay txn pgr)
      & \upgr -> if sellerNote /= "null"
        then upgr
          { offer           = Just sellerNote
          , offer_type      = justNa
          , offer_availed   = Just T.empty
          , discount_amount = Just T.empty
          } :: MerchantPaymentGatewayResponse'
        else upgr
      where
        sellerNote = lookupXML xmls "sellerNote" "null"

    match _ = find (\val -> gateway' == show val) (eulerUpiGateways <> [GOOGLEPAY])
      & maybe
          (executePGR merchPGR xmls $ otherGateways pgr)
          (const (executePGR merchPGR xmls $ eulerUpiGWs txn pgr))
