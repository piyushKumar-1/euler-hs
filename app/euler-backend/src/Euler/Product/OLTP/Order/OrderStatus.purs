module Product.OLTP.OrderStatus where

import ECPrelude
import Product.OLTP.OrderStatusTransformer (airpay, airtelmoney, amazonpay, amex, atom, axis, axisnb, axisupi, billdesk, blazepay, cash, ccavenue_v2, citi, citrus, cybersource, dummy, ebs, ebs_v3, epaylater, eulerUpiGWs, executePGR, freecharge, freechargev2, fssatmpin, fsspay, gocashfree, hdfc, hdfc_ebs_vas, hdfcnb, icici, icicinb, ipg, jiomoney, justNa, kotak, lazypay, linepay, lookupXML, lookupXMLKeys, migs, mobikwik, mpesa, olamoney, olapostpaid, otherGateways, paylater, paypal, paytm, paytm_v2, payu, phonepe, pinelabs, razorpay, sbibuddy, simpl, sodexo, stripe, tpsl, zaakpay, zestmoney)
import Types.Lenses (_amount, _apiKey, _authType, _auth_id_code, _bankErrorCode, _bankErrorMessage, _card, _cardIsin, _cardType, _cavv, _chargebacks, _completeResponse, _created, _dateCreated, _discount_amount, _ebs_bin_country, _ebs_payment_status, _ebs_risk_level, _ebs_risk_percentage, _eci, _emiBank, _emiTenure, _enableSendingCardIsin, _enableUnauthenticatedOrderStatusApi, _enabled, _epg_txn_id, _errorCode, _errorMessage, _flagged, _flow, _flowStatus, _gateway, _gatewayAuthReqParams, _gateway_id, _gateway_reference_id, _id, _initiated_by, _isDBMeshEnabled, _isEmi, _isMemCacheEnabled, _juspay, _mandate, _merchantId, _message, _metadata, _offer, _offer_availed, _offer_failure_reason, _offer_type, _orderId, _orderType, _order_id, _payer_app_name, _payer_vpa, _paymentMethod, _paymentMethodType, _paymentSource, _payment_gateway_response, _payment_gateway_response', _payment_method, _payment_method_type, _promotion, _provider, _recommendedAction, _recommended_action, _refund_source, _refund_type, _refunds, _resellerId, _responseXml, _returnUrl, _risk, _riskManagementAccountId, _riskStatus, _rrn, _second_factor_response, _sourceObject, _status, _successResponseId, _txnId, _txnUuid, _txn_detail, _udf2, _value, _xid)

import Cache.Internal (readStringMaybe)
import Config (ecTempCardCred, expectationsSystem, getBaseUrl, getECRConfig, getExpectationsUrl)
import Config as Config
import Config.Constants (ecDB, ecRedis, eulerOrderStatusCachingKey, orderStatusCacheTTL)
import Config.Constants as C
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import DB.Types (_dateToString, getCurrentDateStringWithOffset)
import Data.Array (filterA, head, last, length, singleton, (!!))
import Data.Default (def)
import Data.Foldable (find)
import Data.Int (fromString, toNumber)
import Data.Maybe (fromJust)
import Data.Number (fromString) as Number
import Data.StrMap (StrMap, delete, empty, insert, lookup)
import Data.StrMap as StrMap
import Data.String as S
import EC.Chargeback (Chargeback, mapChargeback)
import EC.Customer (Customer(..))
import EC.Feature (Feature)
import EC.Mandate (Mandate, mapMandate)
import EC.Mandate (PaymentMethodType(..)) as PMT
import EC.MerchantAccount (MerchantAccount)
import EC.MerchantIframePreferences (MerchantIframePreferences)
import EC.MerchantKey (MerchantKey(..))
import EC.OrderMetadataV2 (OrderMetadataV2)
import EC.OrderReference (OrderReference(OrderReference), OrderType(MANDATE_REGISTER), orderStatusToInt)
import EC.PaymentGatewayResponse (PaymentGatewayResponse(PaymentGatewayResponse), findMaybePGRById)
import EC.PaymentGatewayResponse (getResponseXml) as O
import EC.Promotions (Promotions, getRulesFromString)
import EC.Refund (RefundStatus(..), Refund)
import EC.ResellerAccount (ResellerAccount)
import EC.RiskManagementAccount (RiskManagementAccount)
import EC.SecondFactor (SecondFactor, findMaybeSecondFactorByTxnDetailId)
import EC.SecondFactorResponse (SecondFactorResponse, findMaybeSFRBySfId)
import EC.ServiceConfiguration (ServiceConfiguration)
import EC.TxnCardInfo (TxnCardInfo)
import EC.TxnDetail (TxnDetail(TxnDetail), TxnStatus(..), Gateway(..), txnStatusToInt)
import EC.TxnRiskCheck (TxnRiskCheck)
import Engineering.Common (continue, delCachedValEC, getCachedValEC, isNotNull, isPresent, isTrue, isTrueString, setCacheEC, throwErr, unNull, unNullOrErr500)
import Engineering.Flow (ecAccessDenied, ecForbidden, orderNotFound, internalError, badRequest, merchantIdMissing, merchantAccountNull)
import Engineering.Middlewares.Monitoring (Metric)
import Euler.Utils.Encryption (decryptAESRaw)
import Euler.Utils.Operators (skipIfB)
import Euler.Utils.Utils (decodeBase64, getValueFromRecJ)
import Externals.EC.Common (Card(Card), PaymentInfo(PaymentInfo))
import Presto.Backend.APIHandler (mkNativeRequest)
import Presto.Backend.Flow (BackendFlow, ask, doAffRR', get, log, put, setCacheWithExpiry)
import Presto.Backend.Flow (log, getCache) as Presto
import Presto.Backend.Language.Types.UnitEx (UnitEx(..))
import Presto.Backend.Types.API (Headers(..), Method(..), Request(..))
import Product.Gateway.Utils (gatewayIdFromGateway, stringToGateway)
import Product.Network.Vies.Types (ViesGatewayAuthReqParams)
import Product.OLTP.Services.AuthenticationService (ipAddressFilters)
import Product.OLTP.Services.RedisService (ResourceType(..))
import Product.OLTP.Services.RedisService as RedisService
import Sequelize.Query.Options (where_, order)
import Sequelize.Types (null)
import Sequelize.Where (Literal(..), WHERE(..))
import Types.Alias (AuthToken, just, nothing)
import Types.App (Configs, RouteParameters)
import Types.Communication.Expectations.Expectations as EXP
import Types.Communication.Interface (SyncStatusState)
import Types.Communication.OLTP.Order (OrderCreateReq(..), OrderTokenExpiryData(..), OrderTokenResp(..))
import Types.Communication.OLTP.OrderStatus (ClientAuthTokenData(ClientAuthTokenData), JsonToXmlType(..), JsonToXmlTypeForeign(..), MerchantPaymentGatewayResponse(..), MerchantPaymentGatewayResponse'(..), MerchantSecondFactorResponse(..), OrderStatusRequest(OrderStatusRequest), OrderStatusResponse(OrderStatusResponse), Paymentlinks(Paymentlinks), Promotion'(..), Refund', Risk(..), Risk'(..), TxnFlowInfo(..), getOrderStatusRequest, mapTxnDetail, defaultPaymentGatewayResponse, mapRefund)
import Types.Communication.OLTP.Transaction (TxnStatusResponse(..), getBooleanValue)
import Utils.Card (getCardBrandFromIsin)
import Utils.Encoding (defaultEncodeJSON)
import Utils.Monitoring (addMerchantIdToTrackingInfo, incrementOrderStatusCacheAddCount, incrementOrderStatusCacheMissCount, incrementOrderStatusCacheHitCount) as Monitoring
import Utils.Monitoring (incrementClientAuthTokenGeneratedCount)
import Utils.PrestoBackend (TState, getCache, getUUID32, isDBMeshEnabled, liftErr, mapToHeaders)
import Utils.PrestoBackend as DB
import Utils.PrestoUtils (api)
import Utils.Utils (base64ToHex, camelCaseToSnakeCase, decodeString, emptyObj, getEmptyBooleanVal, getMaskedAccNo, getRecordValues, isNotString, jsonParse, jsonStringify, logOrderIdAndTxnUuid, lookupJson, nullOrUndefinedToAny, nullOrUndefinedToStr, nullValue, parseAndDecodeJson, parseAndReplaceWithStringNull, parseInt, parseJson, replaceObjValWithForeignNull, roundOff2, snakeCaseToCamelCase, strToBool, unNullEmptyStringAsNothing, xml2Json, (.^.), (^..))
import Utils.XMLParser (getResponseXMLTuple)

-- Will be removing gateways one by one from here as part of direct upi integrations.
eulerUpiGateways :: Array Gateway
eulerUpiGateways = [HDFC_UPI, INDUS_UPI, KOTAK_UPI, SBI_UPI, ICICI_UPI, HSBC_UPI, VIJAYA_UPI, YESBANK_UPI, PAYTM_UPI]

updateState :: String -> Maybe String -> BackendFlow SyncStatusState Configs SyncStatusState
updateState merchantId orderId = do
  state       <- get
  meshEnabled <- isDBMeshEnabled  merchantId orderId false
  memCacheEnabled <- getCache C.ecRedis C.memCacheEnabled <#> (fromMaybe false)
  _           <- log "isDBMeshEnabled" meshEnabled
  let state'   = state # _merchantId .~ (Just merchantId)
      updState' = state' # _orderId .~ orderId
      updState'' = updState' # _isMemCacheEnabled .~ memCacheEnabled
      updState  = updState'' # _isDBMeshEnabled .~ (Just meshEnabled)
  _           <- put updState
  put updState

createOrderStatusResponse :: forall st rt e. Newtype st (TState e)
                => String -> String -> MerchantAccount -> BackendFlow st _ Unit
createOrderStatusResponse orderId merchantId merchantAccount =  do
  _ <- Presto.log "createOrderStatusResponse" $ "Creating order status cache for " <> merchantId <> " and order_id " <> orderId
  orderStatusReq <- pure $ getOrderStatusRequest orderId
  _              <- getOrderStatusWithoutAuth orderStatusReq empty merchantAccount true Nothing Nothing
  pure unit

processOrderStatus :: OrderStatusRequest -> RouteParameters -> BackendFlow SyncStatusState _ Foreign
processOrderStatus req routeParams = do
  let orderId = maybe (unNullOrUndefined $ req ^. _order_id) pure $ routeParams ^. (at "order_id")
  _ <- logOrderIdAndTxnUuid orderId Nothing
  {merchantAccount, isAuthenticated} <- authenticateReqAndGetMerchantAcc req routeParams
  _ <- updateState (merchantAccount .^. _merchantId) orderId
  _ <- Monitoring.addMerchantIdToTrackingInfo $ merchantAccount .^. _merchantId
  _  <- rejectIfUnauthenticatedCallDisabled merchantAccount `skipIfB` isAuthenticated
  response <- getOrderStatusWithoutAuth req routeParams merchantAccount isAuthenticated Nothing Nothing
  _ <- Presto.log "Process Order Status Response" $ response
  pure response

getOrderStatusWithoutAuth :: forall st r e rt. Newtype st (TState e) =>
  OrderStatusRequest -> RouteParameters -> MerchantAccount -> Boolean -> (Maybe OrderCreateReq) -> (Maybe OrderReference) -> BackendFlow st {sessionId :: String, trackers :: StrMap Metric | rt} Foreign
getOrderStatusWithoutAuth req routeParams merchantAccount isAuthenticated maybeOrderCreateReq maybeOrd = do
  cachedResp <- getCachedOrdStatus isAuthenticated req merchantAccount routeParams --TODO check for txn based refund
  resp <- case cachedResp of
            Nothing -> do
              resp <- getOrdStatusResp req merchantAccount isAuthenticated routeParams
              _    <- addToCache req isAuthenticated merchantAccount routeParams resp
              pure resp
            Just resp -> pure resp

  ordResp'   <- versionSpecificTransforms routeParams resp

  ordResp    <- if isJust maybeOrderCreateReq && isJust maybeOrd  then do
                    let order = unsafePartial $ fromJust $ maybeOrd
                    let orderCreateReq = unsafePartial $ fromJust $ maybeOrderCreateReq
                    checkAndAddOrderToken req orderCreateReq routeParams ordResp' merchantAccount order
                  else pure ordResp'

  checkEnableCaseForResponse req routeParams ordResp

checkAndAddOrderToken :: forall st r e rt. Newtype st (TState e) =>
   OrderStatusRequest -> OrderCreateReq -> RouteParameters -> OrderStatusResponse -> MerchantAccount -> OrderReference -> BackendFlow st _ OrderStatusResponse
checkAndAddOrderToken orderStatusRequest orderCreateReq routeParams resp merchantAccount order = do
  let orderIdPrimary = order .^. _id
  merchantId <- unNullOrErr500 (merchantAccount ^. _merchantId)
  let version = lookup "version" routeParams
  if version >= Just "2018-07-01" then do
      orderTokenData  <- addOrderTokenToOrderStatus orderIdPrimary orderCreateReq merchantId
      pure $ resp # _juspay .~ (orderTokenData)
    else pure resp

addOrderTokenToOrderStatus :: forall st r e rt. Newtype st (TState e) =>
   String -> OrderCreateReq -> String -> BackendFlow st _ (NullOrUndefined OrderTokenResp)
addOrderTokenToOrderStatus orderId (OrderCreateReq orderCreateReq) merchantId = do
  case (unNullOrUndefined orderCreateReq."options.get_client_auth_token") of
    Just true -> do
      {token,expiry} <- RedisService.tokenizeResource (ResourceStr $ orderId) "ORDER" merchantId
      incrementClientAuthTokenGeneratedCount
      pure $ just $ OrderTokenResp {
              client_auth_token : just $ token
            , client_auth_token_expiry : just $ expiry
            }
    _ -> pure $ nothing

checkEnableCaseForResponse ::forall st rt e. Newtype st (TState e) => OrderStatusRequest -> RouteParameters -> OrderStatusResponse -> BackendFlow st _ Foreign
checkEnableCaseForResponse req params resp =
  if isJust (StrMap.lookup "orderId" params) || isPresent (req ^. _orderId) then pure $ snakeCaseToCamelCase (encode resp)
   else pure (encode resp)

authenticateReqAndGetMerchantAcc ::
     OrderStatusRequest
  -> RouteParameters
  -> BackendFlow SyncStatusState _ {merchantAccount :: MerchantAccount, isAuthenticated :: Boolean}
authenticateReqAndGetMerchantAcc ostatusReq@(OrderStatusRequest req) headers = do
    let optApiKey = getApiKeyFromHeader headers
        maybeAuthToken = StrMap.lookup "client_auth_token" headers
    case [optApiKey, maybeAuthToken] of
      [Just apiKey, _] -> authenticateMerchantWithApiKey apiKey
      [_, Just authToken] -> authenticateReqWithClientAuthToken ostatusReq authToken headers
      _ -> getMerchantAccountForUnauthenticatedCalls req headers
  where authenticateMerchantWithApiKey apiKeyStr = do
          merchantKey <- DB.findOne ecDB
           (where_ := WHERE ["api_key" /\ String apiKeyStr, "status" /\ String "ACTIVE"])
          case merchantKey of
            Just (MerchantKey merchantKey) -> do
              merchantAcc <- DB.findOneWithErr ecDB
                (where_ := WHERE ["id" /\ Int (fromMaybe 0 (unNullOrUndefined merchantKey.merchantAccountId))]) ecAccessDenied
              let merchantAccount = merchantAcc # _apiKey .~ (just $ apiKeyStr)
              _ <- ipAddressFilters merchantAccount headers   --- checking IP whitelist in case of authenticated call
              pure $ {merchantAccount, isAuthenticated: true}
            Nothing -> liftErr ecAccessDenied
        getApiKeyFromHeader :: RouteParameters -> Maybe String
        getApiKeyFromHeader headers = do
         ((S.split (S.Pattern ":") <<< decodeBase64) <$> ((S.split (S.Pattern " ") <$> (StrMap.lookup "Authorization" headers)) >>= last))
          >>= head
        getMerchantAccountForUnauthenticatedCalls req headers = do
          let maybeMerchantId = unNullOrUndefined req.merchantId
                                <|> (unNullOrUndefined req.merchant_id)
          merchantId <- maybe (liftErr merchantIdMissing) pure maybeMerchantId
          merchantAccount <- DB.findOneWithErr ecDB (where_ := WHERE ["merchantId" /\ String merchantId]) merchantAccountNull
          pure $ {merchantAccount, isAuthenticated : false }


updateAuthTokenUsage :: AuthToken -> ClientAuthTokenData -> BackendFlow SyncStatusState _ Unit
updateAuthTokenUsage authToken (ClientAuthTokenData clientAuthToken) = do
  let usageCount = maybe 1 (add 1) $ unNullOrUndefined clientAuthToken.usageCount
  case (usageCount == clientAuthToken.tokenMaxUsage) of
    true -> delCachedValEC authToken *> pure unit
    false -> do
      tokenExpiryData <- unwrap <$> getTokenExpiryData
      let ttl = convertDuration $ Seconds $ toNumber tokenExpiryData.expiryInSeconds
      -- {expiryInSeconds,tokenMaxUsage} <- RedisService.getTokenExpiryData "ORDER"
      -- let ttl = show expiryInSeconds
      _ <- setCacheEC ttl authToken (ClientAuthTokenData clientAuthToken {usageCount = just usageCount})
      pure unit

authenticateReqWithClientAuthToken ::
  OrderStatusRequest
  -> AuthToken
  -> RouteParameters
  -> BackendFlow SyncStatusState _ {merchantAccount :: MerchantAccount, isAuthenticated :: Boolean}
authenticateReqWithClientAuthToken (OrderStatusRequest req) authToken headers = do
  maybeAuthTokenData :: Maybe ClientAuthTokenData <- getCachedValEC authToken
  case maybeAuthTokenData of
    Just authTokenData -> do
      _ <- updateAuthTokenUsage authToken authTokenData
      merchantAccount <- getMerchantAccountForAuthToken authTokenData
      pure {merchantAccount, isAuthenticated: true}
    Nothing -> liftErr ecAccessDenied

getMerchantAccountForAuthToken ::forall st rt e. Newtype st (TState e) => ClientAuthTokenData -> BackendFlow st _ MerchantAccount
getMerchantAccountForAuthToken (ClientAuthTokenData otokenData@{resourceType: "ORDER"}) = do
  OrderReference orderReference <- DB.findOneWithErr ecDB (where_ := WHERE ["id" /\ Int (parseInt otokenData.resourceId)]) ecAccessDenied
  merchantId <- unNullOrErr500 orderReference.merchantId
  DB.findOneWithErr ecDB (where_ := WHERE ["merchant_id" /\ String merchantId]) ecAccessDenied

getMerchantAccountForAuthToken (ClientAuthTokenData otokenData@{resourceType: "CUSTOMER"}) = do
  Customer customer <- DB.findOneWithErr ecDB (where_ := WHERE ["id" /\ String otokenData.resourceId]) ecAccessDenied
  DB.findOneWithErr ecDB (where_ := WHERE ["id" /\ Int customer.merchantAccountId]) ecAccessDenied

getMerchantAccountForAuthToken (ClientAuthTokenData otokenData@{resourceType: _}) =
  liftErr ecAccessDenied

rejectIfUnauthenticatedCallDisabled :: MerchantAccount -> BackendFlow SyncStatusState _ Unit
rejectIfUnauthenticatedCallDisabled mAccnt =
  if isTrue (mAccnt ^. _enableUnauthenticatedOrderStatusApi)
  then continue unit
  else liftErr ecForbidden

rejectIfMerchantIdMissing :: OrderStatusRequest -> BackendFlow SyncStatusState _ Unit
rejectIfMerchantIdMissing (OrderStatusRequest req) =
  let maybeMerchantId = unNullOrUndefined req.merchantId
                        <|> (unNullOrUndefined req.merchant_id)
  in
  if isJust maybeMerchantId
  then continue unit
  else liftErr merchantIdMissing

getCachedOrdStatus :: forall st e rt r.
  Newtype st (TState e)
  => Boolean
  -> OrderStatusRequest
  -> MerchantAccount
  -> RouteParameters
  -> BackendFlow st {sessionId :: String, trackers :: StrMap Metric | rt} (Maybe OrderStatusResponse)
getCachedOrdStatus isAuthenticated req mAccnt routeParam = do
  maybeFeature <- DB.findOne ecDB (where_ := WHERE [ "name" /\ String eulerOrderStatusCachingKey] :: WHERE Feature)
  maybe (pure Nothing) (getCachedVal isAuthenticated mAccnt routeParam) maybeFeature
    where
      getCachedVal isAuthenticated mAccnt routeParam feature = do
        case (feature ^. _enabled) of
          true -> do
            _ <- Presto.log "Fetch orderStatus From Cache" $ "Order status cache feature is enabled"
            orderId <- getOrderId req routeParam
            merchantId <- unNullOrErr500 (mAccnt ^. _merchantId)
            val <- getCachedResp ((keyPrefix isAuthenticated) <> merchantId <> "_" <> orderId)
            case val of
              Just value -> do
                _ <- Monitoring.incrementOrderStatusCacheHitCount merchantId
                _ <- log "order status api response from cache" ("merchant_id " <> merchantId <> " orderId " <> orderId)
                _ <- Presto.log "Fetch orderStatus From Cache" $ "Order status response found in cache for merchant_id " <> merchantId <> " orderId " <> orderId
                pure val
              Nothing -> do
                _ <- Monitoring.incrementOrderStatusCacheMissCount merchantId
                _ <- Presto.log "Fetch orderStatus From Cache" $ "Could not find order status response in cache for merchant_id " <> merchantId <> " orderId " <> orderId
                pure val
          false -> do
            _ <- Presto.log "Fetch orderStatus From Cache" $ "Order status cache feature is not enabled"
            pure Nothing
      keyPrefix true = "euler_ostatus_"
      keyPrefix false = "euler_ostatus_unauth_"


addToCache :: forall st rt e r.
  Newtype st (TState e)
  => OrderStatusRequest
  -> Boolean
  -> MerchantAccount
  -> RouteParameters
  -> OrderStatusResponse
  -> BackendFlow st {sessionId :: String, trackers :: StrMap Metric | rt} Unit
addToCache req isAuthenticated mAccnt routeParam ordStatusResp = do
  maybeFeature <- DB.findOne ecDB (where_ := WHERE [ "name" /\ String eulerOrderStatusCachingKey] :: WHERE Feature)
  maybe (continue unit) (setCachedValue isAuthenticated mAccnt routeParam) maybeFeature
    where
      setCachedValue isAuthenticated mAccnt routeParam feature = do
        case feature ^. _enabled of
          true -> do
            orderId <- getOrderId req routeParam
            merchantId <- unNullOrErr500 (mAccnt ^. _merchantId)
            _ <- Presto.log "Order Status add to cache" $ "adding order status response to cache for merchant_id " <> merchantId <> " orderId " <> orderId
            _ <- setRespInCache orderStatusCacheTTL ((keyPrefix isAuthenticated) <> merchantId <> "_" <> orderId) ordStatusResp
            Monitoring.incrementOrderStatusCacheAddCount merchantId
          false -> continue unit
      keyPrefix true = "euler_ostatus_"
      keyPrefix false = "euler_ostatus_unauth_"

setRespInCache :: forall a b. Encode a => Milliseconds -> String -> a -> BackendFlow _ b Unit
setRespInCache expiry key val = do
  eitherRes <- setCacheWithExpiry ecRedis key (jsonStringify (snakeCaseToCamelCase (encode val))) expiry
  case eitherRes of
    Right x -> continue unit
    Left err -> log "cache_save_error_" ("Error while persisting " <> key <> "_" <> (show err)) *> continue unit

getCachedResp :: forall a. String -> BackendFlow _ a (Maybe OrderStatusResponse)
getCachedResp key = do
  eitherVal <- Presto.getCache ecRedis key
  case eitherVal of
    Right (Just v) -> do
        let resp = fromMaybe (toForeign "") (parseAndReplaceWithStringNull Just Nothing v)
        _ <- Presto.log ("Cache value for this order status cache key " <> key) v
        case (runExcept (decode (camelCaseToSnakeCase resp))) of
          Right typedVal -> pure (replaceObjValWithForeignNull typedVal Just Nothing)
          Left err -> log "decode_error" ("Error while decoding cached value for " <> key <> "_" <> show err) *> pure Nothing
    Right Nothing -> log "redis_cache_value_not_found" ("value not found for this key " <> key) *> pure Nothing
    Left err -> log "redis_fetch_error" ("Error while getting value from cache " <> key <> "_" <> show err) *> pure Nothing

getOrdStatusResp :: forall st rt e r.
  Newtype st (TState e)
  => OrderStatusRequest
  -> MerchantAccount
  -> Boolean
  -> RouteParameters
  -> BackendFlow st {sessionId :: String, trackers :: StrMap Metric | rt} OrderStatusResponse
getOrdStatusResp req@(OrderStatusRequest ordReq) mAccnt isAuthenticated routeParam = do
    orderId     <- getOrderId req routeParam
    merchantId  <- unNullOrErr500 (mAccnt ^. _merchantId)
    _           <- Presto.log "Get order status from DB" $ "fetching order status from DB for merchant_id " <> merchantId <> " orderId " <> orderId
    order       <- DB.findOneWithErr ecDB (where_ := WHERE ["order_id" /\ String orderId, "merchant_id" /\ String merchantId]) (orderNotFound orderId)
    let maybeTxnUuid = (unNullOrUndefined ordReq.txnUuid) <|> (unNullOrUndefined ordReq.txn_uuid)
    maybeTxn    <- runMaybeT $ MaybeT (getTxnFromTxnUuid order maybeTxnUuid) <|> MaybeT (getLastTxn order)
    paymentlink <- getPaymentLink order
    ordResp'    <- fillOrderDetails isAuthenticated paymentlink order def
                    >>= addPromotionDetails order
    ordResp     <- addMandateDetails order ordResp'
    let shouldSendFullGatewayResponse = fromMaybe false $ getBooleanValue <$> StrMap.lookup "options.add_full_gateway_response" routeParam
    case maybeTxn of
      Just txn -> do
        addTxnDetailsToResponse txn order ordResp
        >>= addRiskCheckInfoToResponse txn
        >>= addPaymentMethodInfo mAccnt txn
        >>= addRefundDetails txn
        >>= addChargeBacks txn
        >>= addGatewayResponse txn shouldSendFullGatewayResponse
      Nothing ->  pure ordResp

getTxnFromTxnUuid ::forall st rt e. Newtype st (TState e) => OrderReference -> Maybe String -> BackendFlow st _ (Maybe TxnDetail)
getTxnFromTxnUuid order maybeTxnUuid = do
  case maybeTxnUuid of
    Just txnUuid -> do
      orderId <- unNullOrErr500 $ order ^. _orderId
      merchantId <- unNullOrErr500 $ order ^. _merchantId
      DB.findOne ecDB $ where_ := WHERE
        [ "order_id" /\ String orderId
        , "merchant_id" /\ String merchantId
        , "txn_uuid" /\ String txnUuid ] :: WHERE TxnDetail
    Nothing -> pure Nothing

getOrderId :: forall st e r rt. Newtype st (TState e) => OrderStatusRequest -> RouteParameters -> BackendFlow st rt String
getOrderId (OrderStatusRequest req) routeParam = do
  ordId <- pure $ (StrMap.lookup "orderId" routeParam) <|> (StrMap.lookup "order_id" routeParam)
  let orderid = ordId <|> (unNullOrUndefined req.orderId) <|> (unNullOrUndefined req.order_id)
  maybe (liftErr badRequest) pure orderid

getLastTxn ::forall st rt e. Newtype st (TState e) => OrderReference -> BackendFlow st _ (Maybe TxnDetail)
getLastTxn orderRef = do
  orderId <- unNullOrErr500 $ orderRef ^. _orderId
  merchantId <- unNullOrErr500 $ orderRef ^. _merchantId
  txns <- DB.findAll ecDB $
          order := [["dateCreated" , "DESC"]]
          <>  where_ := WHERE [ "order_id" /\ String orderId
                              , "merchant_id" /\ String merchantId] :: WHERE TxnDetail
  case (length txns) of
    0 -> do
      _ <- log "get_last_txn" ("No last txn found for:" <> orderId <> ":merchant:"<> merchantId)
      pure Nothing
    _ -> do
      let chargetxn = find (\txn -> (txn ^. _status == CHARGED)) txns
      case chargetxn of
        Just chrgtxn -> pure $ Just chrgtxn
        Nothing -> pure (txns !! 0)


fillOrderDetails :: forall st e rt r.
  Newtype st (TState e)
  => Boolean
  -> Paymentlinks
  -> OrderReference
  -> OrderStatusResponse
  -> BackendFlow st {sessionId :: String, trackers :: StrMap Metric | rt} OrderStatusResponse
fillOrderDetails isAuthenticated paymentLinks ord status = do
  let OrderStatusResponse resp = status
      OrderReference ordObj = ord
  id <- unNullOrErr500 ordObj.orderUuid
  let nullVal = nullValue unit
  customerId    <- case (unNullOrUndefined ordObj.customerId) of
                    Just customerId -> pure $ toForeign customerId
                    Nothing -> pure nullVal
  customerEmail <- case (unNullOrUndefined ordObj.customerEmail) of
                      Just customerEmail -> if isAuthenticated then pure $ toForeign $ customerEmail else pure nullVal
                      Nothing -> pure nullVal
  customerPhone <- case (unNullOrUndefined ordObj.customerPhone) of
                      Just customerPhone -> if isAuthenticated then pure $ toForeign $ customerPhone else pure nullVal
                      Nothing -> pure nullVal
  returnUrl     <- getReturnUrl ord true
  amount        <- formatAmount ordObj.amount
  amountRefunded <- formatAmount ordObj.amountRefunded
  pure $ wrap resp
       { id = id
       , merchant_id = ordObj.merchantId
       , order_id = ordObj.orderId
       , customer_id = just customerId
       , product_id = unNull ordObj.productId ""
       , status = show $ ordObj.status
       , status_id = orderStatusToInt ordObj.status
       , amount = amount
       , currency = ordObj.currency
       , refunded = ordObj.refundedEntirely
       , payment_links = paymentLinks
       , amount_refunded = amountRefunded
       , date_created = (show ordObj.dateCreated) -- Handle return url
       , customer_email = just customerEmail
       , customer_phone = just customerPhone
       , return_url = just returnUrl
       , udf1 = unNull ordObj.udf1 ""
       , udf2 = unNull ordObj.udf2 ""
       , udf3 = unNull ordObj.udf3 ""
       , udf4 = unNull ordObj.udf4 ""
       , udf5 = unNull ordObj.udf5 ""
       , udf6 = unNull ordObj.udf6 ""
       , udf7 = unNull ordObj.udf7 ""
       , udf8 = unNull ordObj.udf8 ""
       , udf9 = unNull ordObj.udf9 ""
       , udf10 = unNull ordObj.udf10 ""
       }

formatAmount ::forall st rt. NullOrUndefined Number -> BackendFlow st rt (NullOrUndefined Number)
formatAmount (NullOrUndefined Nothing) = pure $ nothing
formatAmount (NullOrUndefined (Just amt)) = do
  amount <- roundOff2 amt
  pure $ just $ amount

addMandateDetails :: forall st r e rt. Newtype st (TState e) => OrderReference -> OrderStatusResponse -> BackendFlow st {sessionId :: String, trackers :: StrMap Metric | rt} OrderStatusResponse
addMandateDetails ordRef orderStatus =
  case (unNullOrUndefined $ ordRef ^._orderType) of
    Just orderType ->
      if orderType == MANDATE_REGISTER then do
        orderId <- unNullOrErr500 $ ordRef ^. _id
        mandate :: Maybe Mandate <- DB.findOne ecDB (where_ := WHERE ["auth_order_id" /\ String orderId, "merchant_id" /\ String (unNull (ordRef ^._merchantId) "")])
        case mandate of
          Just mandateVal -> pure $ orderStatus # _mandate .~ (just $ mapMandate $ mandateVal)
          Nothing -> pure $ orderStatus
        else pure $ orderStatus
    Nothing -> pure $ orderStatus

getPaymentLink :: forall st e rt r.
  Newtype st (TState e)
  => OrderReference
  -> BackendFlow st {sessionId :: String, trackers :: StrMap Metric | rt} Paymentlinks
getPaymentLink orderRef = do
  merchantAccount <- DB.findOne ecDB (where_ := WHERE ["merchant_id" /\ String (unNull (orderRef ^._merchantId) "")] :: WHERE MerchantAccount)
  case merchantAccount of
    Just account -> do
      maybeResellerAccount :: Maybe ResellerAccount <- DB.findOne ecDB (where_ := WHERE ["reseller_id" /\ String (unNull (account ^._resellerId) "")] :: WHERE ResellerAccount)
      let maybeResellerEndpoint =  maybe Nothing (unNullOrUndefined <<< _.resellerApiEndpoint <<< unwrap) maybeResellerAccount
      pure $ createPaymentLinks (nullOrUndefinedToStr (unwrap orderRef).orderUuid) maybeResellerEndpoint
    Nothing -> liftErr internalError --check correct Error

getReturnUrl ::forall st rt e. Newtype st (TState e) => OrderReference -> Boolean -> BackendFlow st _ String
getReturnUrl orderRef includeParams = do
  merchantAccount <- DB.findOne ecDB (where_ := WHERE ["merchant_id" /\ String (unNull (orderRef ^._merchantId) "")] :: WHERE MerchantAccount)
  case merchantAccount of
    Just merchantAcc -> do
      merchantIframePreferences :: Maybe MerchantIframePreferences <- DB.findOne ecDB (where_ := WHERE ["merchant_id" /\ String (unNull (merchantAcc ^._merchantId) "")] :: WHERE MerchantIframePreferences)
      let merchantIframeReturnUrl = fromMaybe "" (maybe Nothing (unNullOrUndefined <<< _.returnUrl <<< unwrap) merchantIframePreferences)
          mirrorGatewayResponse   = maybe Nothing (unNullOrUndefined <<< _.mirrorGatewayResponse <<< unwrap) merchantIframePreferences
          orderRefReturnUrl       = fromMaybe "" (unNullOrUndefined (orderRef ^._returnUrl))
      finalReturnUrl <- if (orderRefReturnUrl == "") then pure $ fromMaybe merchantIframeReturnUrl (unNullOrUndefined (merchantAcc ^._returnUrl)) else pure orderRefReturnUrl
      pure $ finalReturnUrl
    Nothing -> pure $ ""

getChargedTxn ::forall st rt e. Newtype st (TState e) => OrderReference -> BackendFlow st _ (Maybe TxnDetail)
getChargedTxn orderRef = do
  orderId    <- unNullOrErr500 $ orderRef ^. _orderId
  merchantId <- unNullOrErr500 $ orderRef ^. _merchantId
  txns <- DB.findAll ecDB (where_ := WHERE ["order_id" /\ String orderId, "merchant_id" /\ String merchantId] :: WHERE TxnDetail)
  case (length txns) of
    0 -> pure Nothing
    _ -> pure $ find (\txn -> (txn ^. _status == CHARGED)) txns

createPaymentLinks :: String -> Maybe String -> Paymentlinks
createPaymentLinks orderUuid maybeResellerEndpoint =
  Paymentlinks {
    web: NullOrUndefined $ Just (host <> "/merchant/pay/") <> Just orderUuid,
    mobile: NullOrUndefined $ Just (host <> "/merchant/pay/") <> Just orderUuid <> Just "?mobile=true",
    iframe: NullOrUndefined $ Just (host <> "/merchant/ipay/") <> Just orderUuid
  }
  where
    config = getECRConfig
    protocol = (unwrap config).protocol
    host = maybe (protocol <> "://" <> (unwrap config).host) id maybeResellerEndpoint

addPromotionDetails :: forall st r e rt. Newtype st (TState e) => OrderReference -> OrderStatusResponse -> BackendFlow st { sessionId :: String | rt} OrderStatusResponse
addPromotionDetails orderRef orderStatus = do
  let orderId  = unNull (orderRef ^. _id) ""
      ordId = unNull (orderRef ^. _orderId) ""
  promotions <- DB.findAll ecDB (where_ := WHERE ["order_reference_id" /\ String orderId] :: WHERE Promotions)
  case (length promotions) of
    0 -> pure $ orderStatus
    _ -> do
      promotion' <- pure $ find (\promotion -> (promotion ^. _status == "ACTIVE")) promotions
      case promotion' of
        Just promotionVal -> do
          promotion  <- decryptPromotionRules ordId promotionVal
          amount     <- roundOff2 ((orderStatus ^.. _amount $ 0.0) + (promotion ^.. _discount_amount $ 0.0))
          let ordS    = orderStatus # _amount .~ (just amount)
          pure $ ordS # _promotion .~ (just promotion)
        Nothing -> pure orderStatus

decryptPromotionRules :: forall st e r rt. Newtype st (TState e) => String -> Promotions -> BackendFlow st rt Promotion'
decryptPromotionRules ordId promotions = do
  let promotion = unwrap promotions
  keyForDecryption <- doAffRR' "ecTempCardCred" (ecTempCardCred)
  resp <- case (decryptAESRaw "aes-256-ecb" (base64ToHex (promotion.rules)) keyForDecryption Nothing) of
    Right result -> pure result
    Left err -> throwErr $ show err
  rules  <- pure $ getRulesFromString resp
  rValue <- pure $ getMaskedAccNo (rules ^. _value)
  pure $ Promotion'
          { id : just $ show (promotion.id)
          , order_id : just ordId
          , rules : just $ singleton (rules # _value .~ rValue)
          , created : just $ _dateToString (promotion.dateCreated)
          , discount_amount : just $ promotion.discountAmount
          , status :just $ promotion.status
          }

addTxnDetailsToResponse :: forall r st e.
  Newtype st (TState e)
  => TxnDetail
  -> OrderReference
  -> OrderStatusResponse
  -> BackendFlow st _ OrderStatusResponse
addTxnDetailsToResponse txn ordRef orderStatus = do
  let ordStatus = unwrap orderStatus
      txnDetail = unwrap txn
      gateway   = unNull txnDetail.gateway ""
      gatewayId = maybe 0 gatewayIdFromGateway $ stringToGateway gateway
  gatewayRefId <- getGatewayReferenceId txn ordRef
  _ <- log "gatewayRefId " gatewayRefId
  pure $ wrap $ (unwrap orderStatus) {
                status = show $ txnDetail.status,
                status_id = txnStatusToInt txnDetail.status,
                txn_id = just $ txnDetail.txnId,
                txn_uuid = txnDetail.txnUuid,
                gateway_id = just gatewayId,
                gateway_reference_id = just gatewayRefId,
                bank_error_code = just $ unNull txnDetail.bankErrorCode "",
                bank_error_message = just $ unNull txnDetail.bankErrorMessage "",
                gateway_payload =  addGatewayPayload txnDetail,
                txn_detail = just (mapTxnDetail txn)
        }
  where addGatewayPayload txnDetail =
         if (isTrueString txnDetail.gatewayPayload)
           then txnDetail.gatewayPayload
           else nothing

getGatewayReferenceId ::forall st rt e. Newtype st (TState e) => TxnDetail -> OrderReference -> BackendFlow st _ Foreign
getGatewayReferenceId txn ordRef = do
  ordMeta <- DB.findOne ecDB (where_ := WHERE ["order_reference_id" /\ String (unNull (ordRef ^. _id) "")] :: WHERE OrderMetadataV2)
  case ordMeta of
    Just ordM ->
      case (unNullEmptyStringAsNothing (ordM ^. _metadata)) of
        Just md -> do
          gRefId <- pure $ lookupJson ((unNull (txn ^._gateway) "") <> ":gateway_reference_id") (jsonParse md)
          jusId  <- pure $ lookupJson "JUSPAY:gateway_reference_id" (jsonParse md)
          case (gRefId <|> jusId) of
            Just v -> pure $ toForeign v
            Nothing -> checkGatewayRefIdForVodafone ordRef txn
        Nothing -> checkGatewayRefIdForVodafone ordRef txn
    Nothing -> checkGatewayRefIdForVodafone ordRef txn

checkGatewayRefIdForVodafone ::forall st rt e. Newtype st (TState e) => OrderReference -> TxnDetail -> BackendFlow st _ Foreign
checkGatewayRefIdForVodafone ordRef txn = do
  meybeFeature <- DB.findOne ecDB (where_ := WHERE ["name" /\ String ("USE_UDF2_FOR_GATEWAY_REFERENCE_ID"), "merchant_id" /\ String (ordRef .^. _merchantId)] :: WHERE Feature)
  case meybeFeature of
    Just feature -> if ((unNull (txn ^._gateway) "") == "HSBC_UPI") && (feature ^. _enabled) && (isPresent (ordRef ^. _udf2)) then pure $ toForeign (unNull (ordRef ^. _udf2) "") else pure $ nullValue unit
    Nothing -> pure $ nullValue unit

addRiskCheckInfoToResponse :: forall st e rt r.
  Newtype st (TState e)
  => TxnDetail
  -> OrderStatusResponse
  -> BackendFlow st { sessionId :: String | rt} OrderStatusResponse
addRiskCheckInfoToResponse txn orderStatus = do
  txnRiskCheck <- DB.findOne ecDB (where_ := WHERE ["txn_detail_id" /\ String (unNull (txn ^. _id) "")] :: WHERE TxnRiskCheck)
  case txnRiskCheck of
    Just trc -> do
        riskMngAcc  <- DB.findOne ecDB (where_ := WHERE ["id" /\ Int (trc ^. _riskManagementAccountId)] :: WHERE RiskManagementAccount)
        let risk = Risk' {  provider : NullOrUndefined $ maybe Nothing (Just <<< _.provider <<< unwrap) riskMngAcc
                          , status : (trc ^. _status)
                          , message : (trc ^. _message)
                          , flagged : getEmptyBooleanVal (trc ^. _flagged)
                          , recommended_action : just (unNull (trc ^. _recommendedAction) "")
                          , ebs_risk_level : NullOrUndefined Nothing
                          , ebs_payment_status : NullOrUndefined Nothing
                          , ebs_risk_percentage : NullOrUndefined Nothing
                          , ebs_bin_country : NullOrUndefined Nothing
                         }
        if (unNull (risk ^. _provider) "") == "ebs" then do
            completeResponseJson <- xml2Json (trc ^. _completeResponse)
            outputObjectResponseJson <- xml2Json (trc ^. _completeResponse)
            responseObj <- pure $ fromMaybe emptyObj (lookupJson "RMSIDResult" completeResponseJson)
            outputObj   <- pure $ fromMaybe emptyObj (maybe Nothing (lookupJson "Output") (lookupJson "RMSIDResult" outputObjectResponseJson))
            let r' = wrap (unwrap risk) {
                                        ebs_risk_level = NullOrUndefined $ maybe Nothing (Just <<< getString) (lookupJson "RiskLevel" responseObj) ,
                                        ebs_payment_status = (trc ^. _riskStatus),
                                        ebs_risk_percentage = NullOrUndefined $ maybe Nothing fromString (lookupJson "RiskPercentage" responseObj) ,
                                        ebs_bin_country = NullOrUndefined $ maybe Nothing (Just <<< getString) (lookupJson "Bincountry" (outputObj))
                                        }
            r  <- addRiskObjDefaultValueAsNull r'
            pure $ orderStatus # _risk .~ (just r)
          else do
            r <- addRiskObjDefaultValueAsNull risk
            pure $ orderStatus # _risk .~ (just r)
    Nothing -> pure orderStatus
    where getString a = if (isNotString a) then "" else a

addRiskObjDefaultValueAsNull ::forall st rt e. Newtype st (TState e) => Risk' -> BackendFlow st _ Risk
addRiskObjDefaultValueAsNull risk' = do
  let risk = Risk {   provider : if (isJust $ unNullOrUndefined (risk' ^. _provider)) then just (toForeign (unNull (risk' ^. _provider) "")) else just (nullValue unit)
                    , status : if (isJust $ unNullOrUndefined (risk' ^. _status)) then just (toForeign (unNull (risk' ^. _status) "")) else just (nullValue unit)
                    , message : if (isJust $ unNullOrUndefined (risk' ^. _message)) then just (toForeign (unNull (risk' ^. _message) "")) else just (nullValue unit)
                    , flagged : if (isJust $ unNullOrUndefined (risk' ^. _flagged)) then just (toForeign (unNull (risk' ^. _flagged) false)) else just (nullValue unit)
                    , recommended_action : if (isJust $ unNullOrUndefined (risk' ^. _recommended_action)) then just (toForeign (unNull (risk' ^. _recommended_action) "")) else just (nullValue unit)
                    , ebs_risk_level : NullOrUndefined Nothing
                    , ebs_payment_status : NullOrUndefined Nothing
                    , ebs_risk_percentage : NullOrUndefined Nothing
                    , ebs_bin_country : NullOrUndefined Nothing
                  }
  if (unNull (risk' ^. _provider) "") == "ebs" then do
      pure $ wrap (unwrap risk) {
                                  ebs_risk_level = if (isJust $ unNullOrUndefined (risk' ^. _ebs_risk_level)) then just (toForeign (unNull (risk' ^. _ebs_risk_level) "")) else just (nullValue unit)
                                , ebs_payment_status = if (isJust $ unNullOrUndefined (risk' ^. _ebs_payment_status)) then just (toForeign (unNull (risk' ^. _ebs_payment_status) "")) else just (nullValue unit)
                                , ebs_risk_percentage = if (isJust $ unNullOrUndefined (risk' ^. _ebs_risk_percentage)) then just (toForeign (unNull (risk' ^. _ebs_risk_percentage) 0)) else just (nullValue unit)
                                , ebs_bin_country = if (isJust $ unNullOrUndefined (risk' ^. _ebs_bin_country)) then just (toForeign (unNull (risk' ^. _ebs_bin_country) "")) else just (nullValue unit)
                              }
   else pure risk

addPaymentMethodInfo :: forall e st r.
  Newtype st (TState e)
  => MerchantAccount
  -> TxnDetail
  -> OrderStatusResponse
  -> BackendFlow st _ OrderStatusResponse
addPaymentMethodInfo mAccnt txn ordStatus = do
  optCard <- DB.findOne ecDB (where_ := WHERE ["txn_detail_id" /\ String (unNull (txn ^._id) "")])
  let enableSendingCardIsin = unNull (mAccnt ^. _enableSendingCardIsin) false
  case optCard of
    Just card -> do
       cardBrandMaybe <- getCardBrandFromIsin (unNull (card ^. _cardIsin) "")
       orderStatus  <- updatePaymentMethodAndType txn card ordStatus
       let orderStatus' = addCardInfo txn card enableSendingCardIsin cardBrandMaybe
                            <<< addAuthType card
                            <<< addEmi txn $ orderStatus
       addSecondFactorResponseAndTxnFlowInfo txn card orderStatus'
    Nothing -> pure ordStatus

addSecondFactorResponseAndTxnFlowInfo :: forall st e r.
  Newtype st (TState e)
  => TxnDetail
  -> TxnCardInfo
  -> OrderStatusResponse
  -> BackendFlow st _ OrderStatusResponse
addSecondFactorResponseAndTxnFlowInfo txn card orderStatus = do
  if (card ^.. _authType $ "") == "VIES" then do
    maybeSf <- findMaybeSecondFactorByTxnDetailId (txn .^. _id)
    case maybeSf of
      Just sf -> do
        (authReqParams :: ViesGatewayAuthReqParams) <- parseAndDecodeJson (sf ^.. _gatewayAuthReqParams $  "{}") "INTERNAL_SERVER_ERROR" "INTERNAL_SERVER_ERROR"
        let orderStatus' = wrap $ (unwrap orderStatus) { txn_flow_info = just $ mkTxnFlowInfo authReqParams }
        maybeSfr <- findMaybeSFRBySfId (sf .^. _id)
        case maybeSfr of
          Just sfr -> do
            let newSf = mkMerchantSecondFactorResponse sfr
            pure $ orderStatus' # _second_factor_response .~ (just newSf)
          _ -> do
            pure $ orderStatus' -- SFR not available.
      Nothing ->  pure orderStatus -- NO Sf present
    else pure orderStatus --NON VIES Txn

mkTxnFlowInfo :: ViesGatewayAuthReqParams -> TxnFlowInfo
mkTxnFlowInfo params =  TxnFlowInfo
  {  flow_type : maybe null encode $ unNullOrUndefined $ params ^. _flow
  ,  status : maybe null toForeign $ unNullOrUndefined $ params ^. _flowStatus
  ,  error_code : maybe null toForeign $ unNullOrUndefined $ params ^. _errorCode
  ,  error_message : maybe null toForeign $ unNullOrUndefined $ params ^. _errorMessage
  }

mkMerchantSecondFactorResponse :: SecondFactorResponse -> MerchantSecondFactorResponse
mkMerchantSecondFactorResponse sfr = MerchantSecondFactorResponse
  {  cavv : maybe null toForeign $ unNullOrUndefined $ sfr ^. _cavv
  ,  eci : sfr ^. _eci
  ,  xid : sfr ^. _xid
  ,  pares_status : sfr ^. _status
  }

addAuthType :: TxnCardInfo -> OrderStatusResponse -> OrderStatusResponse
addAuthType card ordStatus = wrap $ (unwrap ordStatus) { auth_type = just $ unNull (card ^. _authType) "" }

addEmi :: TxnDetail -> OrderStatusResponse -> OrderStatusResponse
addEmi txn ordStatus =
  if(isTrue (txn ^. _isEmi))
  then wrap $ (unwrap ordStatus) { emi_tenure = txn ^. _emiTenure, emi_bank = txn ^. _emiBank }
  else ordStatus

addCardInfo :: TxnDetail -> TxnCardInfo -> Boolean -> Maybe String -> OrderStatusResponse -> OrderStatusResponse
addCardInfo txn card shouldSendCardIsin cardBrandMaybe ordStatus = do
  let ordStatObj = unwrap ordStatus
      cardObj = unwrap card
  case isCardTxn cardObj of
    true  -> wrap $ ordStatObj
                     { payment_method = if isJust cardBrandMaybe then NullOrUndefined cardBrandMaybe else just "UNKNOWN"
                     , payment_method_type = just $ "CARD"
                     , card = just $ getCardDetails card txn shouldSendCardIsin
                     }
    false -> ordStatus
  where isCardTxn cardObj = isTrueString cardObj.cardIsin

getCardDetails :: TxnCardInfo -> TxnDetail -> Boolean -> Card
getCardDetails card txn shouldSendCardIsin = do
  let cardObj = unwrap card
      txnObj = unwrap txn
  Card $
    {  expiry_year: just $ unNull cardObj.cardExpYear ""
    ,  card_reference: just $ unNull cardObj.cardReferenceId ""
    ,  saved_to_locker: isSavedToLocker cardObj txnObj
    ,  expiry_month: just $ unNull cardObj.cardExpMonth ""
    ,  name_on_card: just $ unNull cardObj.nameOnCard ""
    ,  card_issuer: just $ unNull cardObj.cardIssuerBankName ""
    ,  last_four_digits: just $ unNull cardObj.cardLastFourDigits ""
    ,  using_saved_card: isUsingSavedCard txnObj
    ,  card_fingerprint: just $ unNull cardObj.cardFingerprint ""
    ,  card_isin: if shouldSendCardIsin then cardObj.cardIsin else (just "")
    ,  card_type: just $ unNull cardObj.cardType  "" -- TODO (Some transformation required)
    ,  card_brand: just $ unNull cardObj.cardSwitchProvider ""
    }
  where isSavedToLocker cardObj txnObj = just $ isTrue txnObj.addToLocker && (isTrueString cardObj.cardReferenceId)
        isUsingSavedCard {expressCheckout: NullOrUndefined (Just true)} = just true
        isUsingSavedCard _ = just false

getPayerVpa ::forall st rt e. Newtype st (TState e) =>
               TxnDetail -> TxnCardInfo -> BackendFlow st _ (NullOrUndefined Foreign)
getPayerVpa txn txnCardInfo = do
  if((txnCardInfo ^.. _paymentMethod $ "") == "GOOGLEPAY") then do
      -- pgr <- DB.findOne ecDB (where_ := WHERE ["id" /\ String (unNull (txn ^. _successResponseId) "")])
      pgr <- sequence $ findMaybePGRById <$> (unNullOrUndefined (txn ^. _successResponseId))
      case join pgr of
        Just (PaymentGatewayResponse pg) -> do
          pgResponse  <- O.getResponseXml (unNull pg.responseXml "")
          payervpa <- lookupRespXml' pgResponse "payerVpa" ""
          if (payervpa == "") then pure nothing else pure $ just $ toForeign payervpa
        Nothing -> pure nothing
    else pure nothing


updatePaymentMethodAndType ::forall st rt e. Newtype st (TState e) =>
                              TxnDetail -> TxnCardInfo -> OrderStatusResponse -> BackendFlow st _ OrderStatusResponse
updatePaymentMethodAndType txn card ordStatus = do
  let cardObj = unwrap card
      resp = unwrap ordStatus
  case (unNullOrUndefined $ card ^. _cardType) of
    Just "NB" ->
      pure $ wrap $ resp
                    { payment_method = just $ unNull cardObj.cardIssuerBankName ""
                    , payment_method_type = NullOrUndefined $ Just "NB"
                    }
    Just "WALLET" -> do
      payerVpa <- getPayerVpa txn card
      pure $ wrap $ resp
                    { payment_method =  just $ unNull cardObj.cardIssuerBankName ""
                    , payment_method_type = NullOrUndefined $ Just "WALLET"
                    , payer_vpa = payerVpa
                    }
    Just "UPI" -> do
      let ord  = ordStatus # _payment_method .~ (just "UPI")
          ordS = ord # _payment_method_type .~ (just "UPI")
          paymentSource = if (card ^.. _paymentSource $ "null") == "null" then just $ nullValue unit else just $ toForeign (card ^.. _paymentSource $ "null")
      if (txn ^.. _sourceObject $ "") == "UPI_COLLECT" || (txn ^.. _sourceObject $ "") == "upi_collect" then do
         pure (ordS # _payer_vpa .~ paymentSource)
       else do
        addPayerVpaToResponse txn ordS paymentSource
    Just "PAYLATER" ->
      pure $ wrap $ resp
                    { payment_method = NullOrUndefined $ Just "JUSPAY"
                    , payment_method_type = NullOrUndefined $ Just "PAYLATER"
                    }
    Just "CARD" ->
      pure $ wrap $ resp
              { payment_method =  just $ unNull cardObj.cardIssuerBankName ""-- TODO : add logic of adding card type
              , payment_method_type = NullOrUndefined $ Just "CARD"
              }
    Just "REWARD" ->
      pure $ wrap $ resp
                    { payment_method =  just $ unNull cardObj.cardIssuerBankName ""
                    , payment_method_type = NullOrUndefined $ Just "REWARD"
                    }
    Just "ATM_CARD" ->
      pure $ wrap $ resp
                    { payment_method =  just $ unNull cardObj.cardIssuerBankName ""
                    , payment_method_type = NullOrUndefined $ Just "CARD"
                    }
    Just _ -> checkPaymentMethodType card resp
    Nothing -> checkPaymentMethodType card resp

    where checkPaymentMethodType card resp = case unNullOrUndefined (card ^. _paymentMethodType) of
                                                Just PMT.CASH ->
                                                    pure $ wrap $ resp
                                                            { payment_method = just $ unNull (card ^. _paymentMethod) ""
                                                            , payment_method_type = NullOrUndefined $ Just "CASH"
                                                            }
                                                Just PMT.CONSUMER_FINANCE ->
                                                    pure $ wrap $ resp
                                                            { payment_method = just $ unNull (card ^. _paymentMethod) ""
                                                            , payment_method_type = NullOrUndefined $ Just "CONSUMER_FINANCE"
                                                            }
                                                _ -> pure ordStatus

addRefundDetails ::forall st rt e. Newtype st (TState e) => TxnDetail -> OrderStatusResponse -> BackendFlow st _ OrderStatusResponse
addRefundDetails (TxnDetail txn) ordStatus = do
  refunds <- DB.findAll ecDB ( order := [["dateCreated" , "ASC"]]
     <> where_ := WHERE ["txn_detail_id" /\ String  (nullOrUndefinedToAny (txn.id) "")] :: WHERE Refund)
  case (length refunds) of
    -- No Refunds
    0 -> pure ordStatus
    -- Has Refunds
    _ -> pure $ ordStatus # _refunds .~ (just $ mapRefund <$> refunds)


addChargeBacks ::forall st rt e. Newtype st (TState e) => TxnDetail -> OrderStatusResponse -> BackendFlow st _ OrderStatusResponse
addChargeBacks (TxnDetail txnDetail) orderStatus = do
  txn <- pure $ mapTxnDetail (TxnDetail txnDetail)
  chargeBacks <- DB.findAll ecDB (where_ := WHERE ["txn_detail_id" /\ String (nullOrUndefinedToAny (txnDetail.id) "")] :: WHERE Chargeback)
  case length chargeBacks of
    0 -> pure orderStatus
    _ -> pure $ orderStatus # _chargebacks .~ (just $ mapChargeback txn <$> chargeBacks)

lookupPgRespXml :: String -> String -> String -> BackendFlow _ _ String
lookupPgRespXml respxml key defaultValue = do
  respxmlVal <- O.getResponseXml respxml
  let keyVal = find (\val -> (key == (fromMaybe "" (val.string !! 0)))) respxmlVal
  pure $ case keyVal of
    Just val -> if isNotString $ fromMaybe defaultValue (val.string !! 1) then defaultValue else fromMaybe defaultValue (val.string !! 1)
    Nothing -> defaultValue

lookupRespXml :: forall st r e rt. Newtype st (TState e) => (Array _)  -> String -> String -> BackendFlow st rt String
lookupRespXml xml str1 str2 = do
  str1   <- pure $ find (\val -> (str1 == (fromMaybe "" (val.string !! 0)))) xml
  str2   <- pure $ find (\val -> (str2 == (fromMaybe "" (val.string !! 0)))) xml
  str3   <- case str2 of
              Just value -> if isNotString $ fromMaybe "" (value.string !! 1) then pure "" else pure $ fromMaybe "" (value.string !! 1)
              Nothing -> pure $ "null"
  output <- case str1 of
              Just value -> if isNotString $ fromMaybe "" (value.string !! 1) then pure "" else pure $ fromMaybe "" (value.string !! 1)
              Nothing -> pure $ str3
  pure $ output

lookupRespXml' :: forall st r e rt. Newtype st (TState e) =>(Array _) -> String -> String -> BackendFlow st rt String
lookupRespXml' xml str1 defaultValue = do
  str1 <- pure $ find (\val -> (str1 == (fromMaybe "" (val.string !! 0)))) xml
  case str1 of
    Just val -> if isNotString $ fromMaybe defaultValue (val.string !! 1) then pure $ defaultValue else pure $ fromMaybe defaultValue (val.string !! 1)
    Nothing -> pure $ defaultValue

lookupRespXmlVal :: String-> String -> String -> BackendFlow _ _ String
lookupRespXmlVal respXml str1 defaultValue = do
  xml <- O.getResponseXml respXml :: (BackendFlow _ _ (Array {string :: Array _}))
  let upVal = find (\val -> (str1 == (fromMaybe "" ((val.string) !! 0)))) xml
  pure $ case upVal of
    Just val  -> if isNotString $ fromMaybe defaultValue (val.string !! 1) then defaultValue else fromMaybe defaultValue (val.string !! 1)
    Nothing -> defaultValue

tempLookup :: forall st r e rt. Newtype st (TState e) => String -> String -> String -> BackendFlow st rt String
tempLookup xml str1 defaultValue = do
  x <- O.getResponseXml xml
  str1 <- pure $ find (\val -> str1 == val.string) x
  case str1 of
    Just val -> do
      obj <- pure $ getRecordValues val
      pure $ fromMaybe defaultValue (obj !! 1)
    Nothing -> pure defaultValue

hierarchyObjectLookup :: forall st r e rt. Newtype st (TState e) => String -> String -> String -> BackendFlow st rt String
hierarchyObjectLookup xml key1 key2 = do
  xmlVal <- O.getResponseXml xml
  val    <- pure $ find (\val -> (key1 == val.string)) xmlVal
  case val of
    Just v -> do
      let vA = (v."org.codehaus.groovy.grails.web.json.JSONObject".myHashMap.entry) -- ::  Array { string :: Array String | t594}
      lookupRespXml' vA key2 ""
    Nothing -> pure ""

addGatewayResponse ::forall st rt e. Newtype st (TState e) => TxnDetail -> Boolean -> OrderStatusResponse -> BackendFlow st _ OrderStatusResponse
addGatewayResponse txn shouldSendFullGatewayResponse orderStatus = do
  paymentGatewayResp <- sequence $ findMaybePGRById <$> unNullOrUndefined (txn ^. _successResponseId)
  case join paymentGatewayResp of
    Just paymentGatewayResponse -> do
      ordStatus <- getPaymentGatewayResponse txn paymentGatewayResponse orderStatus
      nullVal   <- pure $ nullValue unit
      case (unNullOrUndefined (ordStatus ^._payment_gateway_response')) of
        Just (MerchantPaymentGatewayResponse' pgr') -> do
            gatewayResponse <- getGatewayResponseInJson paymentGatewayResponse shouldSendFullGatewayResponse
            let pgr = MerchantPaymentGatewayResponse {
                           resp_code : just $ checkNull pgr'.resp_code nullVal
                        ,  rrn : just $ checkNull pgr'.rrn nullVal
                        ,  created : just $ checkNull pgr'.created nullVal
                        ,  epg_txn_id : just $ checkNull pgr'.epg_txn_id nullVal
                        ,  resp_message : just $ checkNull pgr'.resp_message nullVal
                        ,  auth_id_code : just $ checkNull pgr'.auth_id_code nullVal
                        ,  txn_id : just $ checkNull pgr'.txn_id nullVal
                        ,  offer : pgr'.offer
                        ,  offer_type : pgr'.offer_type
                        ,  offer_availed : pgr'.offer_availed
                        ,  discount_amount : pgr'.discount_amount
                        ,  offer_failure_reason : pgr'.offer_failure_reason
                        ,  gateway_response : gatewayResponse
                      }
                ordStatus' = ordStatus # _payment_gateway_response' .~ (NullOrUndefined Nothing)
            pure $ ordStatus' # _payment_gateway_response .~ (just pgr)
        Nothing -> pure $ orderStatus
    Nothing -> pure $ orderStatus
  where checkNull resp nullVal      = if (unNull resp "") == "null" then nullVal else toForeign $ (unNull resp "")

getGatewayResponseInJson ::forall st rt e. Newtype st (TState e) =>
                        PaymentGatewayResponse -> Boolean -> BackendFlow st _ (NullOrUndefined Foreign)
getGatewayResponseInJson paymentGatewayResponse shouldSendFullGatewayResponse =
  if shouldSendFullGatewayResponse then do
    jsonPgr <- createJsonFromPGRXmlResponse <$> (O.getResponseXml (paymentGatewayResponse ^.. _responseXml $ ""))
    pure $ just $ jsonPgr
    else pure nothing

casematch :: TxnDetail → PaymentGatewayResponse → MerchantPaymentGatewayResponse' → String → _ → MerchantPaymentGatewayResponse'
casematch txn pgr dfpgresp gw xmls = match gw
  where
        match "CCAVENUE_V2"    = executePGR dfpgresp xmls $ ccavenue_v2  txn pgr xmls
        match "BLAZEPAY"       = executePGR dfpgresp xmls $ blazepay     txn pgr xmls
        match "STRIPE"         = executePGR dfpgresp xmls $ stripe       txn pgr xmls
        match "CITI"           = executePGR dfpgresp xmls $ citi         txn xmls
        match "IPG"            = executePGR dfpgresp xmls $ ipg          txn xmls
        match "FSS_ATM_PIN_V2" = executePGR dfpgresp xmls $ fssatmpin    txn pgr
        match "HDFC_EBS_VAS"   = executePGR dfpgresp xmls $ hdfc_ebs_vas txn pgr
        match "FREECHARGE_V2"  = executePGR dfpgresp xmls $ freechargev2 txn pgr
        match "FSS_ATM_PIN"    = executePGR dfpgresp xmls $ fssatmpin    txn pgr
        match "AIRTELMONEY"    = executePGR dfpgresp xmls $ airtelmoney  txn pgr
        match "CYBERSOURCE"    = executePGR dfpgresp xmls $ cybersource  txn pgr
        match "OLAPOSTPAID"    = executePGR dfpgresp xmls $ olapostpaid  txn pgr
        match "GOCASHFREE"     = executePGR dfpgresp xmls $ gocashfree   txn pgr
        match "EPAYLATER"      = executePGR dfpgresp xmls $ epaylater    txn pgr
        match "ZESTMONEY"      = executePGR dfpgresp xmls $ zestmoney    txn pgr
        match "BILLDESK"       = executePGR dfpgresp xmls $ billdesk     txn pgr
        match "JIOMONEY"       = executePGR dfpgresp xmls $ jiomoney     txn pgr
        match "SBIBUDDY"       = executePGR dfpgresp xmls $ sbibuddy     txn pgr
        match "RAZORPAY"       = executePGR dfpgresp xmls $ razorpay     txn pgr
        match "AXIS_UPI"       = executePGR dfpgresp xmls $ axisupi      txn pgr
        match "PINELABS"       = executePGR dfpgresp xmls $ pinelabs     txn pgr
        match "MOBIKWIK"       = executePGR dfpgresp xmls $ mobikwik     txn pgr
        match "LINEPAY"        = executePGR dfpgresp xmls $ linepay      txn pgr
        match "PHONEPE"        = executePGR dfpgresp xmls $ phonepe      txn pgr
        match "ICICINB"        = executePGR dfpgresp xmls $ icicinb      txn pgr
        match "ZAAKPAY"        = executePGR dfpgresp xmls $ zaakpay      txn pgr
        match "AIRPAY"         = executePGR dfpgresp xmls $ airpay       txn pgr
        match "AXISNB"         = executePGR dfpgresp xmls $ axisnb       txn pgr
        match "SODEXO"         = executePGR dfpgresp xmls $ sodexo       txn pgr
        match "CITRUS"         = executePGR dfpgresp xmls $ citrus       txn pgr
        match "PAYPAL"         = executePGR dfpgresp xmls $ paypal       txn pgr
        match "HDFCNB"         = executePGR dfpgresp xmls $ hdfcnb       txn pgr
        match "KOTAK"          = executePGR dfpgresp xmls $ kotak        txn pgr
        match "MPESA"          = executePGR dfpgresp xmls $ mpesa        txn pgr
        match "SIMPL"          = executePGR dfpgresp xmls $ simpl        txn pgr
        match "CASH"           = executePGR dfpgresp xmls $ cash         txn pgr
        match "TPSL"           = executePGR dfpgresp xmls $ tpsl         txn pgr
        match "LAZYPAY"        = executePGR dfpgresp xmls $ lazypay      txn pgr
        match "FSSPAY"         = executePGR dfpgresp xmls $ fsspay       txn pgr
        match "AMEX"           = executePGR dfpgresp xmls $ amex         txn pgr
        match "ATOM"           = executePGR dfpgresp xmls $ atom         txn pgr
        match "PAYTM_V2"       = executePGR dfpgresp xmls $ paytm_v2     pgr
        match "EBS_V3"         = executePGR dfpgresp xmls $ ebs_v3       pgr
        match "AXIS"           = executePGR dfpgresp xmls $ axis         pgr
        match "HDFC"           = executePGR dfpgresp xmls $ hdfc         pgr
        match "EBS"            = executePGR dfpgresp xmls $ ebs          pgr
        match "MIGS"           = executePGR dfpgresp xmls $ migs         pgr
        match "ICICI"          = executePGR dfpgresp xmls $ icici        pgr
        match "PAYLATER"       = executePGR dfpgresp xmls $ paylater     txn
        match "DUMMY"          = executePGR dfpgresp xmls $ dummy
        match "FREECHARGE"     = executePGR dfpgresp xmls (freecharge txn pgr)
                                  # \upgr
                                    → if campaignCode /= "NA"
                                        then upgr
                                          # _offer           .~ (just campaignCode)
                                          # _offer_type      .~ justNa
                                          # _offer_availed   .~ (just $ toForeign "NA")
                                          # _discount_amount .~ (just $ nullValue unit)
                                        else upgr
                                 -- Freecharge doesn't return any information about offers in their response
                                 where
                                    campaignCode = lookupXML xmls "campaignCode" "NA"
        match "PAYU"           = executePGR dfpgresp xmls (payu  pgr)
                                  # \upgr → if offer /= "NA"
                                  -- * Payu allows merchants to send multiple offers and avails a valid offer among them
                                  -- * offer_availed contains the successfully availed offer.
                                        then upgr
                                                    # _offer           .~ (just offerVal)
                                                    # _offer_type      .~ offerType
                                                    # _offer_availed   .~ (just $ toForeign offerAvailed)
                                                    # _discount_amount .~ (just disAmount)
                                                    # \rec → if offerFailure /= "null"
                                                              then rec # _offer_failure_reason .~ (just offerFailure)
                                                              else rec
                                        else upgr
                                 where
                                     offer        = lookupXML     xmls "offer"                 "NA"
                                     offerVal     = lookupXMLKeys xmls "offer_availed" "offer" "null"
                                     offerType    = lookupXML     xmls "offer_type"            "null" # just
                                     offerFailure = lookupXML     xmls "offer_failure_reason"  "null"
                                     discount     = lookupXML     xmls "discount"              "NA"
                                     offerAvailed = offerVal /= "null"
                                     disAmount    = Number.fromString discount # maybe (nullValue unit) toForeign

        match "PAYTM"           = executePGR dfpgresp xmls (paytm pgr)
                                    # \upgr → if promoCampId /= "null"
                                              then upgr
                                                    # _offer           .~ (just promoCampId)
                                                    # _offer_type      .~ justNa
                                                    # _offer_availed   .~ (promoStatus == "PROMO_SUCCESS" # toForeign >>> just)
                                                    # _discount_amount .~ (just $ nullValue unit)
                                              else upgr
                                  where
                                    promoCampId = lookupXML xmls "PROMO_CAMP_ID" "null"
                                    promoStatus = lookupXML xmls "PROMO_STATUS"  "null"

        match "OLAMONEY"        = executePGR dfpgresp xmls (olamoney txn pgr)
                                    # \upgr → if couponCode /= "null"
                                              then upgr
                                                    # _offer           .~ (just couponCode)
                                                    # _offer_type      .~ justNa
                                                    # _offer_availed   .~ (strToBool isCashbackSuc # toForeign >>> just)
                                                    # _discount_amount .~ (just $ nullValue unit)
                                              else upgr
                                  where
                                    couponCode    = lookupXML xmls "couponCode"           "null"
                                    isCashbackSuc = lookupXML xmls "isCashbackSuccessful" "null"

        match "AMAZONPAY"       = executePGR dfpgresp xmls (amazonpay txn pgr)
                                    # \upgr → if sellerNote /= "null"
                                              then upgr
                                                    # _offer           .~ (just sellerNote)
                                                    # _offer_type      .~ justNa
                                                    # _offer_availed   .~ (nullValue unit # just)
                                                    # _discount_amount .~ (just $ nullValue unit)
                                              else upgr

                                  where
                                    sellerNote = lookupXML xmls "sellerNote" "null"

        match _             = find (\val → gw == show val) (eulerUpiGateways <> [GOOGLEPAY])
                                # maybe
                                    (executePGR dfpgresp xmls $ otherGateways pgr)
                                    (const (executePGR dfpgresp xmls $ eulerUpiGWs txn pgr))

getPaymentGatewayResponse
  :: ∀ st r.
      Newtype st
        { orderId           :: Maybe String
        , merchantId        :: Maybe String
        , isDBMeshEnabled   :: Maybe Boolean
        , isMemCacheEnabled :: Boolean
        | r }
      => TxnDetail
      → PaymentGatewayResponse
      → OrderStatusResponse
      → BackendFlow st _ OrderStatusResponse
getPaymentGatewayResponse txn pgr orderStatusResp = do
  getResponseXMLTuple pgrXml
    <#>
      (casematch txn pgr upgr gateway >>> just >>> \v → orderStatusResp # _payment_gateway_response' .~ v)

  where
        gateway = unNull (txn ^. _gateway) ""
        pgrXml  = unNull (pgr ^. _responseXml) ""
        date    = pgr ^. _dateCreated # unNullOrUndefined >>> maybe nothing (_dateToString >>> just)
        upgr    = defaultPaymentGatewayResponse # _created .~ date

versionSpecificTransforms ::forall st rt e. Newtype st (TState e) => RouteParameters -> OrderStatusResponse -> BackendFlow st _ OrderStatusResponse
versionSpecificTransforms headers orderStatus = do
  let pgResponse     = unNullOrUndefined (orderStatus ^. _payment_gateway_response)
      refunds        = unNull (orderStatus ^. _refunds) []
      gatewayId      = unNull (orderStatus ^. _gateway_id) 0
  version     <- pure $ StrMap.lookup "version" headers
  apiVersion  <- pure $ fromMaybe "" version
  refunds'    <- if (apiVersion < "2015-08-18" && apiVersion /= "") || apiVersion == "" then
                    filterA (\refund -> pure ((refund ^._status) /= (FAILURE))) refunds
                  else pure $ refunds
  refund      <- traverse (getRefundStatus apiVersion) refunds'
    -- Removing all the offer related params from the PG response
  pgResps <- case pgResponse of
                Just pgResp -> do
                    --let discountAmount = unNull (pgResp ^. _discount_amount) ""
                    pgResponse' <- if apiVersion < "2017-05-25" || apiVersion == "" then do
                                      let pgResp1 = pgResp  # _offer .~ NullOrUndefined Nothing
                                          pgResp2 = pgResp1 # _offer_availed .~ NullOrUndefined Nothing
                                          pgResp3 = pgResp2 # _offer_type .~ NullOrUndefined Nothing
                                          pgResp4 = pgResp3 # _offer_failure_reason .~ NullOrUndefined Nothing
                                          pgResp5 = pgResp4 # _discount_amount .~ NullOrUndefined Nothing
                                      pure $ pgResp5
                                    else pure $ pgResp

                    -- pgResponse' <- if apiVersion >= "2017-12-08" && apiVersion /= "" && discountAmount == "NA"
                    --                 then pure $ pgResp # _discount_amount .~ NullOrUndefined (Nothing)
                    --                 else pure $ pgResp

                    pgResp <- if (gatewayId == gatewayIdFromGateway PAYU && ((apiVersion < "2017-10-26" && apiVersion /= "") || apiVersion == "")) then do
                                  let pgResp = pgResponse' # _auth_id_code .~ pgResponse' ^. _rrn
                                  pure $ pgResp # _rrn .~ pgResp ^. _epg_txn_id
                                else pure $ pgResponse'
                    pure $ Just pgResp
                Nothing -> pure $ Nothing
  let orderStatus' = if length refund > 0 then orderStatus # _refunds .~ NullOrUndefined ( Just refund) else orderStatus
      ordStatus = orderStatus' # _payment_gateway_response .~ NullOrUndefined pgResps
  ordStatusResp <- if (isNotNull (ordStatus ^. _chargebacks)) && ((apiVersion < "2017-07-26" && apiVersion /= "") || apiVersion == "") then pure $ ordStatus # _chargebacks .~ NullOrUndefined Nothing
                      else if (apiVersion == "") then pure $ ordStatus # _chargebacks .~ NullOrUndefined Nothing
                      else pure $ ordStatus
  ordResp      <- if (isNotNull (ordStatusResp ^. _txn_detail)) && ((apiVersion < "2018-07-16" && apiVersion /= "") || apiVersion == "") then pure $ ordStatusResp # _txn_detail .~ NullOrUndefined Nothing
                      else if (apiVersion == "") then pure $ ordStatusResp # _txn_detail .~ NullOrUndefined Nothing
                      else pure $ ordStatusResp
  if (isNotNull (ordResp ^. _gateway_reference_id)) && ((apiVersion < "2018-10-25" && apiVersion /= "") || apiVersion == "") then pure $ ordResp # _gateway_reference_id .~ NullOrUndefined Nothing
    else if (apiVersion == "") then pure $ ordResp # _gateway_reference_id .~ NullOrUndefined Nothing
    else pure $ ordResp

getRefundStatus ::forall st rt e. Newtype st (TState e) => String  -> Refund' -> BackendFlow st _ Refund'
getRefundStatus apiVersion refund = do
   let status = refund ^. _status
   refunds <- if (apiVersion < "2018-09-20" && apiVersion /= "") || (apiVersion == "") then pure (refund # _initiated_by .~ nothing) else pure refund
   refunds <- if (apiVersion < "2019-03-12" && apiVersion /= "") || (apiVersion == "") then do
                                                                                            let refund = refunds # _refund_source .~ nothing # _refund_type .~ nothing
                                                                                            pure refund
                                                                                       else pure refunds
   if apiVersion >= "2015-01-09" && apiVersion /= "" then do
     if apiVersion < "2017-03-16" && apiVersion /= "" && status == MANUAL_REVIEW then pure $ refunds # _status .~ PENDING
      else pure $ refunds
    else pure $ refunds # _status .~ SUCCESS


-- TODO: Move to method decode once presto is fixed
postOrderStatus :: BackendFlow SyncStatusState Configs Foreign
postOrderStatus = proxyOrderStatus POST

postOrderCreate :: BackendFlow SyncStatusState Configs Foreign
postOrderCreate = proxyOrderCreate POST

getOrderStatus :: BackendFlow SyncStatusState Configs Foreign
getOrderStatus = proxyOrderStatus GET

proxyOrderStatus :: Method -> BackendFlow SyncStatusState Configs Foreign
proxyOrderStatus method = do
  config <- ask
  let url = S.replace (S.Pattern "proxy/") (S.Replacement "") config.url
      finalSystemName = if url == "txns" then "euler_txns" else expectationsSystem
      request = Request {
        method
      , url : getBaseUrl <> url
      , payload : config.rawBody
      , headers : modifyContentType config.rawHeaders
      }
  stringResponse <- doAffRR' "proxyOrderStatus. api mkNativeRequest" (api (mkNativeRequest request))
  let resp = fromMaybe (toForeign "") (parseJson stringResponse)
      responseHeaders = getValueFromRecJ resp ["headers"]
      requestHeaders = getValueFromRecJ resp ["requestHeaders"]
      responseStatus = getValueFromRecJ resp ["status"]
      responseStatusText = getValueFromRecJ resp ["statusText"]
      responseData = getValueFromRecJ resp ["data"]
      latency = getValueFromRecJ resp ["latency"]
      expectationsRequest = {
        method: POST
      , url: getExpectationsUrl <> "store/request-response"
      , payload: defaultEncodeJSON $ EXP.ExpectationsRequest {
          payload: EXP.ExpectationsPayload {
              request: EXP.ExpectationsReqResp {
                  header: jsonStringify requestHeaders
                , body: config.rawBody
                , url: getBaseUrl <> url
                , method: maybe "GET" (S.toUpper <<< show) config.method
                , status: 0
                , statusText: ""
              }
            , response: EXP.ExpectationsReqResp {
                  header: jsonStringify responseHeaders
                , body: jsonStringify responseData
                , url: ""
                , method: ""
                , status: responseStatus
                , statusText: responseStatusText
              }
            , latency
          }
          , systemName: finalSystemName
      }
      , headers: Headers []
      }
  -- TODO: WTF??
  void $ doAffRR' "proxyOrderStatus. unsafe forked mkNativeRequest" (do
    void $ unsafeCoerceAff $ forkAff $ api $ mkNativeRequest $ Request expectationsRequest
    pure UnitEx
    )
  pure $ toForeign {
      responseData
    , responseStatus
    }
  where
    modifyContentType :: StrMap String -> Headers
    modifyContentType rawHeaders =
      case lookup "Content-Type" rawHeaders <|> lookup "content-type" rawHeaders of
        Just val -> case val of
          "application/json" -> mapToHeaders rawHeaders
          _ ->  mapToHeaders
            <<< insert "Content-Type" "application/x-www-form-urlencoded"
            <<< delete "Content-Type"
            <<< delete "content-type"
            $ rawHeaders
        Nothing -> mapToHeaders rawHeaders


proxyOrderCreate :: Method -> BackendFlow SyncStatusState Configs Foreign
proxyOrderCreate method = do
  config <- ask
  let url = S.replace (S.Pattern "proxy/") (S.Replacement "") config.url
      finalSystemName = if url == "orders" then "euler_order_create" else expectationsSystem
      request = Request {
        method
      , url : getBaseUrl <> url
      , payload : config.rawBody
      , headers : modifyContentType config.rawHeaders
      }
  stringResponse <- doAffRR' "proxyOrderCreate. api mkNativeRequest"  do api (mkNativeRequest request)
  let resp = fromMaybe (toForeign "") (parseJson stringResponse)
      responseHeaders = getValueFromRecJ resp ["headers"]
      requestHeaders = getValueFromRecJ resp ["requestHeaders"]
      responseStatus = getValueFromRecJ resp ["status"]
      responseStatusText = getValueFromRecJ resp ["statusText"]
      responseData = getValueFromRecJ resp ["data"]
      latency = getValueFromRecJ resp ["latency"]
      expectationsRequest = {
        method: POST
      , url: getExpectationsUrl <> "store/request-response"
      , payload: defaultEncodeJSON $ EXP.ExpectationsRequest {
          payload: EXP.ExpectationsPayload {
              request: EXP.ExpectationsReqResp {
                  header: jsonStringify requestHeaders
                , body: config.rawBody
                , url: getBaseUrl <> url
                , method: maybe "GET" (S.toUpper <<< show) config.method
                , status: 0
                , statusText: ""
              }
            , response: EXP.ExpectationsReqResp {
                  header: jsonStringify responseHeaders
                , body: jsonStringify responseData
                , url: ""
                , method: ""
                , status: responseStatus
                , statusText: responseStatusText
              }
            , latency
          }
          , systemName: finalSystemName
      }
      , headers: Headers []
      }
  void $ doAffRR' "proxyOrderCreate. unsafe forked mkNativeRequest" (do
    void $ unsafeCoerceAff $ forkAff $ api $ mkNativeRequest $ Request expectationsRequest
    pure UnitEx
    )
  pure $ toForeign {
      responseData
    , responseStatus
    }
  where
    modifyContentType :: StrMap String -> Headers
    modifyContentType rawHeaders =
      case lookup "Content-Type" rawHeaders <|> lookup "content-type" rawHeaders of
        Just val -> case val of
          "application/json" -> mapToHeaders rawHeaders
          _ ->  mapToHeaders
            <<< insert "Content-Type" "application/x-www-form-urlencoded"
            <<< delete "Content-Type"
            <<< delete "content-type"
            $ rawHeaders
        Nothing -> mapToHeaders rawHeaders

getTxnStatusResponse ::forall st rt e. Newtype st (TState e) => TxnDetail -> MerchantAccount -> SecondFactor -> BackendFlow st _ TxnStatusResponse
getTxnStatusResponse txnDetail@(TxnDetail txn) merchantAccount sf = do
    ordStatusResponse <- addPaymentMethodInfo merchantAccount txnDetail def
                          >>= addRefundDetails txnDetail
                          >>= addGatewayResponse txnDetail false
    pure $ TxnStatusResponse {
        id : txnDetail .^. _txnUuid
      , order_id : txnDetail ^. _orderId
      , txn_id : txnDetail ^. _txnId
      , status : txnDetail ^. _status
      , gateway : txnDetail .^. _gateway
      , created : txnDetail .^. _dateCreated
      , resp_code : txnDetail ^.. _bankErrorCode  $ ""
      , resp_message : txnDetail ^.. _bankErrorMessage $ ""
      , payment_info : getPaymentInfo ordStatusResponse
      , payment_gateway_response : ordStatusResponse ^. _payment_gateway_response
      , refunds : ordStatusResponse ^. _refunds
      , payment : nothing
    }

getPaymentInfo :: OrderStatusResponse -> PaymentInfo
getPaymentInfo ordStatusResponse = PaymentInfo {
    payment_method_type : ordStatusResponse ^. _payment_method_type
  , payment_method      : ordStatusResponse ^. _payment_method
  , card                : ordStatusResponse ^. _card
  , auth_type           : nothing
  , authentication      : nothing
}

--- TODO: Move this to a common file
getTokenExpiryData :: BackendFlow SyncStatusState _ OrderTokenExpiryData
getTokenExpiryData = do
  orderToken <- ((append "tkn_") <$> getUUID32)
  currentDateWithOffset <- getCurrentDateStringWithOffset Config.orderTokenExpiry
  defaultTokenData <- pure $ OrderTokenExpiryData {expiryInSeconds : Config.orderTokenExpiry
                      , tokenMaxUsage : Config.orderTokenMaxUsage
                      , orderToken : NullOrUndefined $ Just orderToken
                      , currentDateWithExpiry : NullOrUndefined $ Just currentDateWithOffset}
  tokenExpiryData :: (Maybe ServiceConfiguration) <- DB.findOne ecDB (where_ := WHERE ["name" /\ String "ORDER_TOKEN_EXPIRY_DATA"])
  case tokenExpiryData of
    (Just toknExpData) -> do
      OrderTokenExpiryData decodedVal <- decodeString (_.value <<< unwrap $ toknExpData)
      pure $ OrderTokenExpiryData (unwrap defaultTokenData) { expiryInSeconds = decodedVal.expiryInSeconds
                                                            , tokenMaxUsage = decodedVal.tokenMaxUsage}
    Nothing -> pure $ defaultTokenData

getAxisUpiTxnIdFromPgr ::forall st rt e. Newtype st (TState e) =>
      TxnDetail -> (Array _)  -> BackendFlow st _ String
getAxisUpiTxnIdFromPgr txn pgResponse = do
  merchantRequestId <- lookupRespXml' pgResponse "merchantRequestId" ""
  if merchantRequestId == "" then do
    transactionRef <- lookupRespXml' pgResponse "transactionRef" ""
    if transactionRef /= "" then do
      pure transactionRef else pure (txn ^. _txnId)
    else pure merchantRequestId

getAxisUpiRequestIdFromPgr ::forall st rt e. Newtype st (TState e) =>
      TxnDetail -> (Array _) -> BackendFlow st _ String
getAxisUpiRequestIdFromPgr txn pgResponse = do
  gatewayTransactionId <- lookupRespXml' pgResponse "gatewayTransactionId" ""
  if gatewayTransactionId == "" then lookupRespXml' pgResponse "upiRequestId" "null"
    else pure gatewayTransactionId

createJsonFromPGRXmlResponse :: Array String -> Foreign
createJsonFromPGRXmlResponse arrayXml = toForeign $ foldl xmlToJsonPgrAccumulater empty arrayXml

xmlToJsonPgrAccumulater ::forall b.(StrMap String) -> String -> StrMap String
xmlToJsonPgrAccumulater strmap xml = do
  case runExcept $ decode $ toForeign $ xml of
    Right (JsonToXmlType val) -> do
      let array = val.string
      insert (fromMaybe "" (array !! 0))  (fromMaybe "" (array !! 1)) strmap
    Left err  -> do
      case runExcept $ decode $ toForeign $ xml of
        Right (JsonToXmlTypeForeign foreignVal) -> do
          -- To handle {} cases.
          let arrayForeign = foreignVal.string
          insert (fromMaybe "" (readStringMaybe $ (fromMaybe (toForeign $ "") (arrayForeign !! 0))))  "" strmap
        Left err -> strmap

-- In direct UPI flow, in case of AXIS_UPI, we've added one more lookup for customerVpa as gateway is sending payer vpa in customerVpa field. For other upi gateways also we've to add loopup based on their response.
--TODO-- Once all the upi gateways are migrated to euler from euler-upi, Remove payerVpa lookup and keep gateway specific loopup key for payer vpa param.

addPayerVpaToResponse :: forall st rt e. Newtype st (TState e)
  =>  TxnDetail
  -> OrderStatusResponse
  -> NullOrUndefined Foreign
  -> BackendFlow st _ OrderStatusResponse
addPayerVpaToResponse txnDetail ordStatusResp paymentSource = do
  let ord' = (ordStatusResp # _payer_app_name .~ paymentSource)
  pgr <- sequence $ findMaybePGRById <$> (unNullOrUndefined $ txnDetail ^. _successResponseId)
  case join pgr of
    Just (PaymentGatewayResponse pg) -> do
      pgResponse  <- O.getResponseXml (unNull pg.responseXml "")
      payervpa <- case txnDetail ^.. _gateway $ "" of
                    "AXIS_UPI"    -> lookupRespXml' pgResponse "payerVpa" =<< lookupRespXml' pgResponse "customerVpa" ""
                    "HDFC_UPI"    -> lookupRespXml' pgResponse "payerVpa" ""
                    "INDUS_UPI"   -> lookupRespXml' pgResponse "payerVpa" ""
                    "KOTAK_UPI"   -> lookupRespXml' pgResponse "payerVpa" ""
                    "SBI_UPI"     -> lookupRespXml' pgResponse "payerVpa" ""
                    "ICICI_UPI"   -> lookupRespXml' pgResponse "payerVpa" ""
                    "HSBC_UPI"    -> lookupRespXml' pgResponse "payerVpa" ""
                    "VIJAYA_UPI"  -> lookupRespXml' pgResponse "payerVpa" ""
                    "YESBANK_UPI" -> lookupRespXml' pgResponse "payerVpa" ""
                    "PAYTM_UPI"   -> lookupRespXml' pgResponse "payerVpa" ""
                    "PAYU"        -> lookupRespXml' pgResponse "field3" ""
                    "RAZORPAY"    -> lookupRespXml' pgResponse "vpa" ""
                    "PAYTM_V2"    -> lookupRespXml' pgResponse "VPA" ""
                    "GOCASHFREE"  -> lookupRespXml' pgResponse "payersVPA" ""
                    _             -> pure ""
      if (payervpa == "") then pure ord' else pure (ord' # _payer_vpa .~ just (toForeign payervpa))
    Nothing -> pure ord'
