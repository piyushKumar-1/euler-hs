{-# LANGUAGE DeriveAnyClass #-}

module Euler.API.Transaction where

import           EulerHS.Prelude

import           Euler.Common.Types.Transaction (AuthType, PaymentMethodType)

-- TODO: what is this?
-- foreign import modifyRequestBody :: Foreign -> Foreign

-- data TransactionType  = PG_S2S_REDIRECT | PG_REDIRECT | WALLET_DEBIT | SDK_PG_REDIRECT | MANDATE_TXN | DIRECT_OTP | UPI_COLLECT | GENERATE_QR_CODE
--
-- data TxnsReqType = DIRECT_DEBIT | TXN | MANDATE_DEBIT
-- data MandateReqType = MERCHANT | CLIENT_AUTH | DASHBOARD
--
--
-- type IsRecurringReq = Boolean
-- type IsSdkParamsPresentInReq = Boolean
-- type IsMerchantEnabled = Boolean
--


-- // A Stored card transaction
-- curl -X POST https://api.juspay.in/txns \
-- -d "order_id=:order_id" \
-- -d "merchant_id=:merchant_id" \
-- -d "payment_method_type=CARD" \
-- -d "auth_type=ATMPIN" \
-- -d "card_token=:card_token" \
-- -d "card_security_code=111" \
-- -d "redirect_after_payment=true" \
-- -d "format=json"

-- // A regular card transaction
-- curl -X POST https://api.juspay.in/txns \
-- -d "order_id=:order_id" \
-- -d "merchant_id=:merchant_id" \
-- -d "payment_method_type=CARD" \
-- -d "payment_method=MASTERCARD" \
-- -d "auth_type=ATMPIN" \
-- -d "card_number=5243681100075285" \
-- -d "card_exp_month=10" \
-- -d "card_exp_year=20" \
-- -d "name_on_card=Name" \
-- -d "card_security_code=111" \
-- -d "save_to_locker=true" \
-- -d "redirect_after_payment=true" \
-- -d "format=json"

-- Previously: TransactionCreateReq
data Transaction = Transaction
  { order_id               :: Text                -- TASK: https://www.notion.so/065d5a12d1be491c960f5a4dcb81b783?v=972782d46f8b4c69a15b1e057a97006f&p=20f6558b4b1441afb1b63128f5cc08d6
  , merchant_id            :: Text                -- ^
  , payment_method_type    :: PaymentMethodType   -- ^
  , payment_method         :: Maybe Text
  , redirect_after_payment :: Bool                -- ^
  , format                 :: Text                -- ^

  , auth_type              :: Maybe AuthType
  , card_token             :: Maybe Text
  , card_security_code     :: Maybe Text
  , card_number            :: Maybe Text
  , card_exp_month         :: Maybe Text
  , card_exp_year          :: Maybe Text
  , name_on_card           :: Maybe Text
  , save_to_locker         :: Maybe Bool
  , is_emi                 :: Maybe Bool
  , emi_bank               :: Maybe Text
  , emi_tenure             :: Maybe Int
  , upi_vpa                :: Maybe Text
  , txn_type               :: Maybe Text
  , direct_wallet_token    :: Maybe Text
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

  -- , card_alias            :: Maybe Text
  -- , mandate_debit_token   :: Maybe Text
  -- , gateway_id            :: Maybe Text
  -- , card_encoding_version :: Maybe Text
  -- , sdk_params            :: Maybe Text
  -- , upi_tr_field          :: Maybe Text
  -- , express_checkout      :: Maybe Text
  -- , bank_code             :: Maybe Text
  -- , upi_app               :: Maybe Text
  -- , authentication_method :: Maybe Text
  -- , auth_account_id       :: Maybe Text
  -- , payjs_version         :: Maybe Text
  -- , payment_channel       :: Maybe Text
  -- , offer_token           :: Maybe Text
  -- , client_auth_token     :: Maybe Text
  -- , mobile_number         :: Maybe Text
  -- , should_create_mandate :: Maybe Text
  -- , order                 :: Maybe OrderCreateReq
  -- , mandate_id            :: Maybe Text
  -- , mandate_token         :: Maybe Text
  -- , order_return_url      :: Maybe Text
  -- , internal_source_flow  :: Maybe Text
  -- }

-- Dummy
data Authentication = Authentication
  { method :: Text
  , url    :: Text
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

data PaymentAuth = PaymentAuth
  { authentication :: Authentication
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

-- Dummy. Only valid for Card.
-- TODO: make separate responses for different payment methods.
data TransactionResponse = TransactionResponse
  { order_id       ::  Text          -- ^ OrderID for which the payment is being started.
  , txn_id         ::  Text          -- ^ Transaction ID for the payment attempt.
  , status         ::  Text          -- ^ Status of the transaction. PENDING_VBV indicates that the transaction requires authentication to complete. Please do not validate this at your end.
  , payment        ::  PaymentAuth   -- ^ Contains the payment authentication details.
  , authentication ::  Text          -- ^ Contains the authentication details.
  , method         ::  Text          -- ^ HTTP Method for authentication. Can be one of GET or POST
  , url            ::  Text          -- ^ URL to which the user has to be taken to for completing the authentication
  , params         ::  Text          -- ^ Present only when method is POST. Parameter map that has to be sent along with the URL for authentication.
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

--   -- authenticationAccount //derived value -- used only for wallets
--   -- order //derived value - transient

-- newtype TransactionCreateReq' = TransactionCreateReq' TransactionCreateReq

-- Previously: TransactionProcessReq
-- data TransactionProcess
--   = VIESTxn ViesTxn
--   -- | Empty EmptyBody   -- ???
--
-- Previously: ViesTxnReq
-- data ViesTxn = ViesTxn
--   { card_alias         :: Text
--   , app_id             :: Text
--   , signed_device_data :: Text
--   , encrypted_data     :: Text
--   , first_time         :: Boolean
--   }

-- newtype CmpiLookupResponse = CmpiLookupResponse
--   { acsUrl :: Text
--   , payload :: Text
--   , transactionId :: Text
--   }
--
-- newtype AuthResp = AuthResp
--   { method :: Text
--   , url :: Text
--   , params :: Maybe Foreign
--   }
--
-- newtype PaymentResp = PaymentResp
--   { authentication :: AuthResp
--   , sdk_params :: Foreign
--   , qr_code    :: Foreign
--   }
--
-- data TransactionCreateResp = TransactionCreateJsonResp JsonResp
--                            | TransactionCreateRedirectResp RedirectResp
--                            | TransactionHtmlResp Text
--
-- newtype RedirectResp = RedirectResp { url :: Text }
--
-- newtype JsonResp = JsonResp
--   { order_id :: Text
--   , txn_id :: Text
--   , txn_uuid :: Maybe Text
--   , status :: Text
--   , payment :: PaymentResp
--   , juspay :: Maybe OrderTokenResp
--   }
--
-- newtype EcTxnsJsonResp = EcTxnsJsonResp {
--   code :: Int,
--   status :: Text,
--   response :: JsonResp
-- }
--
-- newtype EcTxnsRedirResp = EcTxnsRedirResp {
--   code :: Int,
--   status :: Text,
--   response :: Text
-- }
--
-- newtype StartPaymentNwJsonResp = StartPaymentNwJsonResp {
--   action :: Foreign,
--   retainIdToken :: Boolean
-- }
--
-- data ViesStartPayAction = Onboarding | Directdebit | Autoretry | ReEnroll | StepUp
--
-- derive instance genericViesStartPayAction :: Generic ViesStartPayAction _
-- instance encodeViesStartPayAction :: Encode ViesStartPayAction where
--   encode = defaultEnumEncode
-- instance decodeViesStartPayAction :: Decode ViesStartPayAction where
--   decode = defaultEnumDecode
-- instance showViesStartPayAction :: Show ViesStartPayAction where
--   show = genericShow
--
-- newtype StartPaymentViesResponse = StartPaymentViesResponse {
--   txnStatus :: TxnStatus,
--   retainIdToken :: Boolean,
--   content :: Maybe Text,
--   sfr :: Maybe SecondFactorResponse,
--   sf :: Maybe SecondFactor,
--   idToken :: Maybe Text,
--   action :: ViesStartPayAction
-- }
--
-- data StartPaymentResp = StartPaymentResp Text | StartPaymentRedirectResponse Text | StartPaymentJsonResp StartPaymentNwJsonResp
--
-- data StartPaymentNetworkResp = StartPaymentNetworkResp Text | StartPaymentViesResp StartPaymentViesResponse
--
-- newtype GatewayResponseResp = GatewayResponseResp
--   { data :: Text,
--     statusCode :: Int,
--     location :: Maybe Text -- Only used in case of 302 statusCode (redirect)
--   }
--
-- data HandleResponseResp = HandleResponseRedirectResp Text
--

-- newtype PGRedirectResponse = PGRedirectResponse {
--   order_id :: Text,
--   status :: Text,
--   status_id :: Int,
--   mandate_token :: Maybe Text,
--   mandate_id :: Maybe Text,
--   mandate_status :: Maybe MandateStatus,
--   udf1 :: Maybe Text,
--   udf2 :: Maybe Text,
--   udf3 :: Maybe Text,
--   udf4 :: Maybe Text,
--   udf5 :: Maybe Text,
--   gatewayId :: Maybe Text,
--   customerId :: Maybe Text,
--   amount :: Maybe Number
-- }
--
--
--
-- -- data StrToBoolable = StrToBoolable Boolean
--
-- makeNetworkAuthorizeReq :: Text -> Text -> Maybe Boolean -> Maybe Text -> Maybe Text -> Maybe Text -> NetworkAuthorizeReq
-- makeNetworkAuthorizeReq cavv txnId requireGatewayResponse walletRefId walletType correlationId = NetworkAuthorizeReq {
--     cavv : cavv
--   , txnId : txnId
--   , add_gateway_response : requireGatewayResponse
--   , "custom_params.wallet_merchantReferenceID" : walletRefId
--   , "custom_params.wallet_type" : walletType
--   , "custom_params.partnerOriginalTransactionID" : correlationId
-- }
--
-- makeNetworkAuthorizeReqWithRouteParams :: Text -> Text -> Text -> Maybe Boolean -> Maybe Text -> Maybe Text -> Maybe Text -> NetworkAuthorizeReqWithRouteParams
-- makeNetworkAuthorizeReqWithRouteParams txnUuid cavv txnId requireGatewayResponse walletRefId walletType correlationId = NetworkAuthorizeReqWithRouteParams {
--     networkAuthorizeReq : makeNetworkAuthorizeReq cavv txnId requireGatewayResponse walletRefId walletType correlationId
--   , routeParams : insert "txnUuid" txnUuid empty
-- }
--
-- newtype NetworkAuthorizeReqWithRouteParams = NetworkAuthorizeReqWithRouteParams {
--     networkAuthorizeReq :: NetworkAuthorizeReq
--   , routeParams :: RouteParams
-- }
--
-- newtype NetworkAuthorizeReq = NetworkAuthorizeReq
--   { cavv :: Text
--   , txnId :: Text
--   , add_gateway_response :: Maybe Boolean
--   , "custom_params.wallet_merchantReferenceID" :: Maybe Text
--   , "custom_params.wallet_type" :: Maybe Text
--   , "custom_params.partnerOriginalTransactionID" :: Maybe Text
--   }
--
-- newtype NetworkAuthorizeResp = NetworkAuthorizeResp {
--   txn_uuid :: Text
--   , order_id :: Text
--   , merchant_id :: Text
--   , status :: TxnStatus
--   , gateway_response :: Maybe AuthZGatewayResp
-- }
--
-- newtype AuthZGatewayResp = AuthZGatewayResp {
--   wallet_enrollmentID :: Maybe Text
--   , requestToken :: Text
--   , merchantReferenceCode :: Text
--   , requestID :: Text
--   , purchaseTotals_currency :: Text
--   , ccAuthReply_amount :: Maybe Text
--   , ccAuthReply_authorizationCode :: Maybe Text
--   , ccAuthReply_avsCode :: Maybe Text
--   , ccAuthReply_reconciliationID :: Maybe Text
--   , ccCaptureReply_reconciliationID :: Maybe Text
--   , ccAuthReply_authorizedDateTime :: Maybe Text
--   , ccCaptureReply_requestDateTime :: Maybe Text
-- }
--
-- addRouteParam :: Text -> RouteParams -> Text
-- addRouteParam url = fold (\url key value -> replace (Pattern $ ":" <> key) (Replacement value) url) url
--
-- ecCreateNetworkTransaction :: NetworkAuthorizeReqWithRouteParams -> Maybe Text -> BackendFlow _ Configs (Either ErrorPayload NetworkAuthorizeResp)
-- ecCreateNetworkTransaction req maybeAuth =  do
--   conf <- ask
--   let headers = addAdditionalHeaders conf.sessionId maybeAuth
--   callingGeneric headers req
--   where
--     addAdditionalHeaders sid maybeAuth = Headers $ [Header Const.eulerSid sid, Const.transactionSourceHeader] <> [Header "Authorization" (createBasicAuth (fromMaybe "INTERNAL4B76436E8A2C660A06099FEE" maybeAuth))]
--
--
-- getBooleanValue :: Text -> Boolean
-- getBooleanValue str =
--   if (str == "1" || toLower(str) == "true") then true
--     else false
--
-- fetchCardTypeFromPMType :: PaymentMethodType -> Maybe Text
-- fetchCardTypeFromPMType WALLET = just $ show $ CardType.WALLET
-- fetchCardTypeFromPMType UPI = just $ show $  CardType.UPI
-- fetchCardTypeFromPMType NB = just $ show $  CardType.NB
-- fetchCardTypeFromPMType PAYLATER = just $ show $  CardType.PAYLATER
-- fetchCardTypeFromPMType CONSUMER_FINANCE = nothing
-- fetchCardTypeFromPMType REWARD = just $ show $ CardType.REWARD
-- fetchCardTypeFromPMType CARD = just $ show $ CardType.CREDIT
-- fetchCardTypeFromPMType CASH = nothing
-- fetchCardTypeFromPMType (UNKNOWN _) = just $ show $ CardType.BLANK
--
-- instance validateTransactionCreateReq :: Validatable TransactionCreateReq where
--   validate = pure
--
-- checkAuthHeader :: RouteParameters -> Headers
-- checkAuthHeader params =
--   case (lookup "Authorization" params) of
--     Just auth -> Headers [Header "Authorization" auth]
--     Nothing -> Headers []
--
-- getTextifyHeaders :: RouteParameters -> Text
-- getTextifyHeaders = drop 14 <<< show <<< update maskAuth "Authorization"
--   where maskAuth a = Just "***"
--
--
-- mkTempTxn :: TransactionCreateReq -> Maybe Text -> Maybe Text -> Int -> Text -> TempTxn
-- mkTempTxn txnReq resp txnUuid status headers = TempTxn
--   { id : nothing
--     , orderId : txnReq .^. _order_id
--     , format : txnReq ^. _format
--     , txnUuid : txnUuid
--     , request : just $ jsonTextify $ sanitizeObject $ encode txnReq
--     , response : resp
--     , statusCode : status
--     , headers : headers
--   }
--
--
-- ecCreateTxn :: forall st r. Newtype st { orderId :: Maybe Text, merchantId :: Maybe Text, isDBMeshEnabled :: Maybe Boolean, isMemCacheEnabled :: Boolean | r } => TransactionCreateReq -> RouteParameters -> BackendFlow st Configs TransactionCreateResp
-- ecCreateTxn req params = do
--   _ <- forkFlow (createOrUpdateTempTxn (req .^. _order_id) $ mkTempTxn req nothing nothing 1 (getTextifyHeaders  params))
--   let config = getECRConfig
--       host = (unwrap config).internalECHost
--       url = host <> "/txns"
--       labels =  getLabels req url
--   timer <- trackExternalApiCallStart (ExternalApiCall labels)
--   conf <- ask
--   if (req ^.. _format $ "") == "json"
--     then do
--       eitherResp <- (callAPI (addAdditionalHeaders conf.sessionId (checkAuthHeader params)) req)
--       case eitherResp of
--         Left err@(Response {code}) -> do
--           let respLabels = ExternalApiCall $ (labels {status_code = just code})
--           _ <- trackExternalApiCallEnd respLabels timer
--           _ <- forkFlow (createOrUpdateTempTxn (req .^. _order_id) $ mkTempTxn req (just $ jsonTextify err) nothing code (getTextifyHeaders params))
--           throwECExceptionT err
--         Right res@(EcTxnsJsonResp {response, code}) -> do
--           let respLabels = ExternalApiCall $ (labels {status_code = just code})
--           _ <- trackExternalApiCallEnd respLabels timer
--           _ <- forkFlow (createOrUpdateTempTxn (req .^. _order_id) $ mkTempTxn req (just $ jsonTextify res) (just $  response ^.. _txn_uuid $ "NIL" ) code (getTextifyHeaders params))
--           pure $ TransactionCreateJsonResp response
--     else do
--       eitherResp <- (callAPI (addAdditionalHeaders conf.sessionId (checkAuthHeader params)) (TransactionCreateReq' req))
--       case eitherResp of
--         Left err@(Response {code}) -> do
--           let respLabels = ExternalApiCall $ (labels {status_code = just code})
--           _ <- forkFlow (createOrUpdateTempTxn (req .^. _order_id) $ mkTempTxn req (just $ jsonTextify err) nothing code (getTextifyHeaders params))
--           _ <- trackExternalApiCallEnd respLabels timer
--           throwECExceptionT err
--         Right res@(EcTxnsRedirResp resp) -> do
--           let respLabels = ExternalApiCall $ (labels {status_code = just resp.code})
--           _ <- trackExternalApiCallEnd respLabels timer
--           _ <- forkFlow (createOrUpdateTempTxn (req .^. _order_id) $ mkTempTxn req (just $ jsonTextify res) nothing resp.code (getTextifyHeaders params))
--           pure $ if resp.code == 302
--             then TransactionCreateRedirectResp $ RedirectResp { url: resp.response }
--             else TransactionHtmlResp resp.response
--   where getLabels r url =  { merchant_id: just (r ^. _merchant_id)
--                   , url: just url
--                   , method: just "POST"
--                   , status_code: nothing
--                   , gateway: just "EC"}
--         addAdditionalHeaders sid (Headers h) = Headers $ [Header Const.eulerSid sid, Const.transactionSourceHeader] <> h
--
-- instance restEndpointJsonCreateTxnReq :: RestEndpoint TransactionCreateReq EcTxnsJsonResp where
--   makeRequest req headers = urlEncodedMakeRequestPresto POST url headers req
--                             where config = getECRConfig
--                                   host = (unwrap config).internalECHost
--                                   url = host <> "/txns"
--   decodeResponse = defaultDecodeResponse
--
-- instance restEndpointCreateTxnReq :: RestEndpoint TransactionCreateReq' EcTxnsRedirResp where
--   makeRequest req headers = urlEncodedMakeRequestPresto POST url headers req
--                             where config = getECRConfig
--                                   host = (unwrap config).internalECHost
--                                   url = host <> "/txns"
--   decodeResponse = defaultDecodeResponse
--
-- newtype DirectOTPAuthenticateRequest = DirectOTPAuthenticateRequest {
--     challenge_id  :: Maybe Text
--   , "answer.otp"  :: Text
--   , auto_capture  :: Maybe Boolean
-- }
--
-- newtype TxnStatusResponse = TxnStatusResponse {
--     id :: Text
--   , order_id :: Text
--   , txn_id :: Text
--   , status :: TxnStatus
--   , gateway :: Text
--   , created :: Date
--   , resp_code :: Text
--   , resp_message :: Text
--   , payment_info :: PaymentInfo
--   , payment_gateway_response :: Maybe MerchantPaymentGatewayResponse
--   , refunds :: Maybe (Array Refund')
-- }
--
-- data Action = OnBoarding { content :: Text, data :: Text }
--             | DirectDebit { idToken :: Maybe Text, txn_status_response :: PGRedirectResponseWithSignature }
--             | Renroll { txn_response :: JsonResp }
--             | Stepup  { url :: Text, userMessage :: Text }
--             | AutoRetry { url :: Text, idToken :: Maybe Text, userMessage :: Text }
--             | ExitSDK { errorCode :: Text, errorMessage :: Text, userMessage :: Text, txn_status_response :: Maybe PGRedirectResponseWithSignature }
--
-- newtype PGRedirectResponseWithSignature  = PGRedirectResponseWithSignature
--   { order_id :: Text
--   , status :: Text
--   , status_id :: Int
--   , mandate_token :: Maybe Text
--   , mandate_id :: Maybe Text
--   , mandate_status :: Maybe MandateStatus
--   , udf1 :: Maybe Text
--   , udf2 :: Maybe Text
--   , udf3 :: Maybe Text
--   , udf4 :: Maybe Text
--   , udf5 :: Maybe Text
--   , gatewayId :: Maybe Text
--   , customerId :: Maybe Text
--   , amount :: Maybe Number
--   , signature :: Maybe Text
--   , signature_algorithm :: Maybe Text
--   }
--
-- data TxnFilterRequest = TxnFilterRequest (Array TxnFilter)
--
-- newtype TxnFilterResponse = TxnFilterResponse
--   {
--    code :: Int,
--    status :: Text,
--    response :: TxnFilterRespType
--   }
--
-- data TxnFilterRespType = FilterResponse TxnFilterResp | ErrorResponse ErrorPayload
--
-- newtype TxnFilterResp = TxnFilterResp
--   { isAllowed :: Boolean
--   , reason :: Maybe Text
--   , sourceType :: Maybe Text
--   }
--
-- newtype TxnFilter = TxnFilter
--   { merchantId :: Text
--   , value :: Text
--   , sourceType :: Text
--   }
--
