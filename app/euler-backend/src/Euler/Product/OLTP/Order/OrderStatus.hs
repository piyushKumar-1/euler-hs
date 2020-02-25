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
import qualified Data.ByteString.Lazy as BSL
import           Data.Char (toLower)
import           Data.Either.Extra
import           Data.Generics.Product.Fields
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Time
import           Servant.Server
import           Control.Comonad hiding ((<<=))
import           Data.Semigroup as S
import           Control.Monad.Except

import           Euler.API.Order as AO
import           Euler.API.RouteParameters (RouteParameters(..), lookupRP)
import           Euler.API.Transaction
import           Euler.API.Types

import qualified Euler.Common.Metric as Metric
import           Euler.Common.Types.DefaultDate
import           Euler.Common.Types.Gateway
import           Euler.Common.Types.Mandate as Mandate
import           Euler.Common.Types.Merchant
import           Euler.Common.Types.Order (OrderTokenExpiryData (..), defaultOrderTokenExpiryData)
import qualified Euler.Common.Types.Order as C
import           Euler.Common.Types.Promotion
import           Euler.Common.Types.Refund as Refund
import           Euler.Common.Types.TxnDetail
import           Euler.Common.Utils
import           Euler.Config.Config as Config

import           Euler.Product.Domain.Order (Order, OrderId(..))

import           Euler.Product.OLTP.Card.Card
import           Euler.Product.OLTP.Services.AuthenticationService (extractApiKey, getMerchantId)

import           Euler.Storage.Types.Chargeback
import           Euler.Storage.Types.Customer
import           Euler.Storage.Types.Feature
import           Euler.Storage.Types.Mandate
import           Euler.Storage.Types.MerchantAccount
import           Euler.Storage.Types.MerchantIframePreferences
import           Euler.Storage.Types.MerchantKey
import           Euler.Storage.Types.OrderMetadataV2
import           Euler.Storage.Types.OrderReference
import           Euler.Storage.Types.PaymentGatewayResponse
import           Euler.Storage.Types.Promotions
import           Euler.Storage.Types.Refund
import           Euler.Storage.Types.ResellerAccount
import           Euler.Storage.Types.RiskManagementAccount
import           Euler.Storage.Types.SecondFactor
import           Euler.Storage.Types.SecondFactorResponse
import           Euler.Storage.Types.ServiceConfiguration
import           Euler.Storage.Types.TxnCardInfo
import           Euler.Storage.Types.TxnDetail
import           Euler.Storage.Types.TxnRiskCheck

import           Euler.Storage.Types.EulerDB as EDB

import           Euler.Storage.Repository.Refund as RR
import           Euler.Storage.Repository.Chargeback as RC

import           Euler.Version.Services.OrderStatusResponse

import           Database.Beam ((&&.), (/=.), (<-.), (==.))
import qualified Database.Beam as B
import qualified Database.Beam.Backend.SQL as B
import           Euler.Storage.DBConfig

-- porting statistics:
-- to port '-- TODO port' - 21
-- to update '-- TODO update' - 21
-- completed '-- done' - 31
-- refactored '-- refactored' - 4
-- total number of functions = 73

-- "xml cases"
-- - TxnRiskCheck.completeResponse
-- - PaymentGatewayResponse.responseXml
--

-- 40x error with error message
myerr    n = err403 { errBody = "Err # " <> n }
myerr400 n = err400 { errBody = "Err # " <> n }


-- PS Will be removing gateways one by one from here as part of direct upi integrations.
-- used in `casematch` function
eulerUpiGateways :: [Gateway]
eulerUpiGateways = [HDFC_UPI, INDUS_UPI, KOTAK_UPI, SBI_UPI, ICICI_UPI, HSBC_UPI, VIJAYA_UPI, YESBANK_UPI, PAYTM_UPI]


data FlowError = FlowError -- do we have something similar?
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

-- ----------------------------------------------------------------------------
-- refactored top-level functions (subjects to be exported)



-- API-aware handler -- naming convention?
-- TODO inside authentication used "x-forwarded-for" header
-- TODO can we collect all headers in Map and save them in state? before we run the flow?
--processOrderStatus :: Text -> APIKey -> Flow OrderStatusResponse

handleByOrderId :: OrderId -> APIKey -> RouteParameters -> Flow (Either FlowError OrderStatusResponse)
handleByOrderId orderId apiKey routeParams = do

  -- TODO use AuthService
  -- if merchantAccount don't exists - throw access denied exception
  (merchantAccount, isAuthenticated) <- authenticateWithAPIKey apiKey
  -- _ <- updateState (merchantAccount .^. _merchantId) orderId

  -- TODO investigate: how is it used?
  --instead of updateState we can save parameters with options (if they not shared over all api handlers)
  _ <- setOption FlowStateOption $ FlowState (getField @"merchantId" merchantAccount) (runOrderId orderId)
  -- set metrics/log info
  -- * field "merchantId" is mandatory, so we can define it as Text instead of Maybe Text in MerchantAccount data type

  -- TODO rework using sth like imaginary "FlowError"?
  -- * if unauthenticated calls disabled by merchant - throw access forbidden
  _  <- unless isAuthenticated $ rejectIfUnauthenticatedCallDisabled merchantAccount
  -- * use unless/when instead of skipIfB/execIfB
  --rejectIfUnauthenticatedCallDisabled merchantAccount `skipIfB` isAuthenticated

  -- turn into the request or not?

  let query = OrderStatusQuery
        { orderId         = orderId
        , merchantId      = fromMaybe T.empty $ getField @"merchantId" merchantAccount -- can it be empty?
        , resellerId      = getField @"resellerId" merchantAccount
        , isAuthenticated = isAuthenticated
        , sendCardIsin    = fromMaybe False $ getField @"enableSendingCardIsin" merchantAccount -- was Maybe Bool
        , txnId           = undefined :: Maybe Text -- TODO
        , sendFullGatewayResponse = getSendFullGatewayResponse routeParams
        }

  --response <- getOrderStatusWithoutAuth2 q query Nothing Nothing
  response <- execOrderStatusQuery query

 -- _ <- log "Process Order Status Response" $ response
  pure $ mapLeft (const FlowError) response -- TODO fix error hadler


getSendFullGatewayResponse :: RouteParameters -> Bool
getSendFullGatewayResponse routeParams =
    case Map.lookup "options.add_full_gateway_response" (unRP routeParams) of
      Nothing -> False
      Just str -> str == "1" || T.map toLower str == "true"



-- apparently, this case exists in PS-verison
-- not sure regarding auth concerns in this case, merchant id is present in the request but the real MAcc goes along in calls
handleByOrderStatusRequest :: OrderStatusRequest -> Flow (Either Text OrderStatusResponse)
handleByOrderStatusRequest ordRequest = let q = undefined :: OrderStatusQuery
  in execCachedOrderStatusQuery q



-- main specific API-agnostic handler
-- former "getOrderStatusWithoutAuth(2)"
-- no request params, no auth concerns here (the last is not exactly the case)
execCachedOrderStatusQuery :: OrderStatusQuery -> Flow (Either Text OrderStatusResponse)

-- execOrderStatusQuery
--   :: -- OrderStatusRequest -- remove after fix checkAndAddOrderToken
--   -> OrderStatusQuery
--   -- -> Text {-RouteParameters-}
--   -- -> MerchantAccount
--   -- -> Bool
--   -- -> (Maybe OrderCreateRequest)
--   -- -> (Maybe OrderReference)
--   -> Flow OrderStatusResponse -- Foreign
execCachedOrderStatusQuery query@OrderStatusQuery{..} = do
  -- merchId <- case (getField @"merchantId" merchantAccount) of
  --               Nothing -> throwException $ myerr "3"
  --               Just v  -> pure v

  cachedResp <- getCachedOrdStatus isAuthenticated (runOrderId orderId) merchantId  --TODO check for txn based refund
  resp <- case cachedResp of
            Nothing -> do


              -- TODO no details here!
              -- resp <- getOrdStatusResp req merchantAccount isAuthenticated orderId --route params can be replaced with orderId?
              -- orderReference <- getOrderReference query
              -- paymentlink <- getPaymentLink resellerId orderReference
              -- resp <- fillOrderStatusResponse query orderReference paymentlink

              resp <- execOrderStatusQuery query
          -- *     _    <- addToCache req isAuthenticated merchantAccount routeParams resp   -- req needed to get orderId
              pure resp
            Just resp -> pure $ Right resp
  ordResp'   <- pure resp -- versionSpecificTransforms routeParams resp


  -- TODO figure out, what parts of OrderCreateRequest and OrderReference are used in
  -- checkAndAddOrderToken, add this info to OrderStatusQuery, default it to Nothing for common query

  -- I absolutely hate the idea to modify cache entries! Did I get right this is the case?
  -- Can we have  different cached results for regular status/order create case instead?
  -- If not, we should pop up this logic up above the stack

  -- * This part probably should be moved to the separate function, used only for orderCreate request
  -- ^ I don't think so
  ordResp    <- undefined -- TODO:
                  -- if isJust maybeOrderCreateReq && isJust maybeOrd  then do
                  --   let order = fromJust maybeOrd
                  --   let orderCreateReq = fromJust maybeOrderCreateReq
                  --   checkAndAddOrderToken req orderCreateReq "routeParams" ordResp' merchantId order
                  -- else pure ordResp'
 ------------------------------------------------
 -- * encode response to Foreign inside, why?
  -- *checkEnableCaseForResponse req routeParams ordResp' -- ordResp
  pure ordResp





-- former getOrdStatusResp
execOrderStatusQuery :: OrderStatusQuery -> Flow (Either Text OrderStatusResponse)
execOrderStatusQuery query@OrderStatusQuery{..} = do

  orderRef <- undefined :: Flow OrderReference -- TODO loadOrder orderId merchantId

  let orderUuid = fromMaybe T.empty $ getField @"orderUuid" orderRef

  links <- mkPaymentLinks resellerId orderUuid



  -- ordResp <- fillOrderStatusResponse query order links
  mPromotion' <- getPromotion orderRef
  mMandate' <- loadMandate orderRef

  -- ids, customer info, status info, amount, payment links
  -- ordResp'    <- fillOrderDetails isAuthenticated paymentlink order def
  -- -- amount(!), promotion
  --                 >>= addPromotionDetails order
  -- mandate
  -- ordResp     <- addPromotionDetails order response >>= addMandateDetails order

  -- TODO has to go to Server.hs or somewhere else RouteParams are still available
  --let shouldSendFullGatewayResponse = fromMaybe false $ getBooleanValue <$> StrMap.lookup "options.add_full_gateway_response" routeParam

  mTxnDetail1 <- getTxnFromTxnUuid orderRef txnId
  mTxnDetail2 <- getLastTxn orderRef
  let mTxn = (mTxnDetail1 <|> mTxnDetail2)

  -- case (mTxnDetail1 <|> mTxnDetail2) of
  --   Just txn -> Right <$> fillOrderStatusResponseTxn query txn order ordResp
  --   Nothing  -> pure $ Right ordResp
  gatewayRefId <- case mTxn of
    Nothing -> getGatewayReferenceId2 T.empty orderRef
    Just txn -> do
      let gateway = fromMaybe "" $ getField @"gateway" txn
      getGatewayReferenceId2 gateway orderRef

  (mRisk, mTxnCard, mRefunds', mChargeback') <- case mTxn of
    Nothing -> pure (Nothing, Nothing, Nothing, Nothing)
    Just txn -> do
      case getField @"id" txn of
        Nothing -> pure (Nothing, Nothing, Nothing, Nothing)
        Just txnId -> do
          mRisk <- getRisk txnId
          mTxnCard <- loadTxnCardInfo txnId
          mRefunds' <- maybeList <$> (refundDetails txnId)
          mChargeback' <- maybeList <$> (chargebackDetails txnId $ mapTxnDetail txn)
          pure (mRisk, mTxnCard, mRefunds', mChargeback')

  mCardBrand <- case mTxnCard of
    Nothing -> pure Nothing
    Just (card :: TxnCardInfo) -> getCardBrandFromIsin (fromMaybe "" $ getField @"cardIsin" card)

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

  -- mbTxnId <- txnId <|> (getLastTxn orderId) -- getLastTxn returns txnDetails not id

  -- case mbTxnId of
  --   Just txnId -> do
  --     txn <- txnData txnId
  --     -- status info (!), txn ids, gateway ids + payload, txn detail
  --     addTxnDetailsToResponse txnId order ordResp
  --     -- risk
  --     >>= addRiskCheckInfoToResponse txnId
  --     -- investigate, second_factor_response
  --     >>= addPaymentMethodInfo mAccnt txtxnIdn
  --     -- refunds
  --     >>= addRefundDetails txnId
  --     -- chargebacks
  --     >>= addChargeBacks txnId
  --     -- payment_gateway_response
  --     >>= addGatewayResponse txnId sendFullGatewayResponse
    -- Nothing ->  pure ordResp

-- should we divide this method to:
-- 1) getStatusResp, where we just get it from DB

-- 2) fillStatusResp, where we apply:
--  * fillOrderDetails
--  * addPromotionDetails
--  * addMandateDetails

-- 3) addTxnInfo (this method only for POST orderStatus api method) where we:
--  * addTxnDetailsToResponse
--  * addRiskCheckInfoToResponse
--  * addPaymentMethodInfo
--  * addRefundDetails
--  * addChargeBacks
--  * addGatewayResponse



---------------------------------------------------------------------------------------------------
-- Builders
---------------------------------------------------------------------------------------------------

-- Use functions from Data.Semigroup only. Data.Monoid and Universum.Monoid depricated
-- Hide <<= from Control.Comonad and use custome one equal to =>> for intuitive percieving
(<<=) :: Comonad w => w a -> (w a -> b) -> w b
(<<=) = (=>>)

type ResponseBuilder = OrderStatusResponseTemp -> OrderStatusResponse

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




-- ----------------------------------------------------------------------------
-- function: updateState
-- TODO unused
-- Not needed, since we have no state, use `setOption` instead
-- ----------------------------------------------------------------------------

{-PS
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
-}

-- ----------------------------------------------------------------------------
-- function: createOrderStatusResponse
-- TODO unused
-- ----------------------------------------------------------------------------

{-PS
createOrderStatusResponse :: forall st rt e. Newtype st (TState e)
                => String -> String -> MerchantAccount -> BackendFlow st _ Unit
createOrderStatusResponse orderId merchantId merchantAccount =  do
  _ <- Presto.log "createOrderStatusResponse" $ "Creating order status cache for " <> merchantId <> " and order_id " <> orderId
  orderStatusReq <- pure $ getOrderStatusRequest orderId
  _              <- getOrderStatusWithoutAuth orderStatusReq empty merchantAccount true Nothing Nothing
  pure unit
-}

-- ----------------------------------------------------------------------------
-- processOrderStatus
-- TODO update
-- ----------------------------------------------------------------------------

{-PS
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
-}

type APIKey = Text

data FlowState = FlowState
  { merchantId :: Maybe Text
  , orderId    :: Text
  }
  deriving (Generic, Show, Eq, ToJSON, FromJSON )


data FlowStateOption = FlowStateOption
  deriving (Generic, Show, Eq, ToJSON, FromJSON )

instance OptionEntity FlowStateOption FlowState


-- ----------------------------------------------------------------------------
-- getOrderStatusWithoutAuth
-- TODO update
-- ----------------------------------------------------------------------------

{-PS
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
-}


getOrderStatusWithoutAuth
  :: OrderStatusRequest -- remove after fix checkAndAddOrderToken
  -> Text {-RouteParameters-}
  -> MerchantAccount
  -> Bool
  -> (Maybe OrderCreateRequest)
  -> (Maybe OrderReference)
  -> Flow OrderStatusResponse -- Foreign
getOrderStatusWithoutAuth req orderId merchantAccount isAuthenticated maybeOrderCreateReq maybeOrd = undefined

getOrderStatusWithoutAuth2
  :: OrderStatusRequest -- remove after fix checkAndAddOrderToken
  -> OrderStatusQuery
  -- -> Text {-RouteParameters-}
  -- -> MerchantAccount
  -- -> Bool
  -> (Maybe OrderCreateRequest)
  -> (Maybe OrderReference)
  -> Flow OrderStatusResponse -- Foreign
getOrderStatusWithoutAuth2 req query@OrderStatusQuery{..} maybeOrderCreateReq maybeOrd = do
  -- merchId <- case (getField @"merchantId" merchantAccount) of
  --               Nothing -> throwException $ myerr "3"
  --               Just v  -> pure v

  cachedResp <- getCachedOrdStatus isAuthenticated (runOrderId orderId) merchantId  --TODO check for txn based refund
  resp <- case cachedResp of
            Nothing -> do
              -- resp <- getOrdStatusResp req merchantAccount isAuthenticated orderId --route params can be replaced with orderId?
              orderReference <- getOrderReference query
              paymentlink <- mkPaymentLinks resellerId (fromMaybe "" $ orderUuid orderReference)
              resp <- fillOrderStatusResponse query orderReference paymentlink
         -- *     _    <- addToCache req isAuthenticated merchantAccount routeParams resp   -- req needed to get orderId
              pure resp
            Just resp -> pure resp
  ordResp'   <- pure resp -- versionSpecificTransforms routeParams resp

  -- * This part probably shoul be moved to the separate function, used only for orderCreate request
  ordResp    <- if isJust maybeOrderCreateReq && isJust maybeOrd  then do
                    let order = fromJust maybeOrd
                    let orderCreateReq = fromJust maybeOrderCreateReq
                    checkAndAddOrderToken req orderCreateReq "routeParams" ordResp' merchantId order
                  else pure ordResp'
 ------------------------------------------------
 -- * encode response to Foreign inside, why?
  -- *checkEnableCaseForResponse req routeParams ordResp' -- ordResp
  pure ordResp

-- ----------------------------------------------------------------------------
-- function: checkAndAddOrderToken
-- TODO update
-- TODO use Euler.Constant.Version?
-- ----------------------------------------------------------------------------

{-PS
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
-}

checkAndAddOrderToken :: OrderStatusRequest -> OrderCreateRequest -> {-RouteParameters-} Text -> OrderStatusResponse -> Text {-MerchantAccount-} -> OrderReference -> Flow OrderStatusResponse
checkAndAddOrderToken orderStatusRequest orderCreateReq routeParams resp merchantId order = do
  orderIdPrimary <- maybe (throwException err500) pure (getField @"id" order)
 -- merchantId <- unNullOrErr500 (merchantAccount ^. _merchantId)
  let version = Just "2018-07-01" -- lookup "version" routeParams -- version should be in headers?
  if version >= Just "2018-07-01" then do
      orderTokenData  <- addOrderTokenToOrderStatus orderIdPrimary orderCreateReq merchantId
      pure $ setField @"juspay" orderTokenData resp
    else pure resp

-- ----------------------------------------------------------------------------
-- function: addOrderTokenToOrderStatus
-- TODO update
-- ----------------------------------------------------------------------------

{-PS
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
-}

addOrderTokenToOrderStatus :: Text -> OrderCreateRequest -> Text -> Flow (Maybe OrderTokenResp)
addOrderTokenToOrderStatus orderId orderCreateReq merchantId = do
  case  (Just True) of -- (getField @"options.get_client_auth_token" orderCreateReq) of
    Just True -> do
      (token, expiry) <- pure ("token", "expiry") -- RedisService.tokenizeResource (toForeign orderId) "ORDER" merchantId

      runIO $ Metric.incrementClientAuthTokenGeneratedCount merchantId

      pure $ Just $ OrderTokenResp {
              client_auth_token = Just token
            , client_auth_token_expiry = Just expiry
            }
    _ -> pure $ Nothing


-- ----------------------------------------------------------------------------
-- function: checkEnableCaseForResponse
-- TODO use flex casing functionality instead ()
-- ----------------------------------------------------------------------------

{-PS
checkEnableCaseForResponse ::forall st rt e. Newtype st (TState e) => OrderStatusRequest -> RouteParameters -> OrderStatusResponse -> BackendFlow st _ Foreign
checkEnableCaseForResponse req params resp =
  if isJust (StrMap.lookup "orderId" params) || isPresent (req ^. _orderId) then pure $ snakeCaseToCamelCase (encode resp)
   else pure (encode resp)
-}


-- ----------------------------------------------------------------------------
-- function: authenticateReqAndGetMerchantAcc / authenticateWithAPIKey
-- TODO update/use common auth service
-- ----------------------------------------------------------------------------

{-PS
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
-}

-- part of authenticateReqAndGetMerchantAcc
-- looks like authenticateRequest src/Product/OLTP/Services/AuthenticationService.purs
authenticateWithAPIKey :: APIKey -> Flow (MerchantAccount, Bool)
authenticateWithAPIKey apiKeyStr = do
  let eApiKey = extractApiKey apiKeyStr
  case eApiKey of
    Right key -> do
      logDebug "Extracted API key" key
      conn <- getConn eulerDB
      merchantKey <- runDB conn $ do
        let predicate MerchantKey {apiKey, status} = (apiKey ==. (B.just_ $ B.val_ key))
              &&. (status ==. B.just_ "ACTIVE")
        findRow
          $ B.select
          $ B.limit_ 1
          $ B.filter_ predicate
          $ B.all_ (merchant_key eulerDBSchema)
      --pure $ Just defaultMerchantKey--DB.findOne ecDB
      --(where_ := WHERE ["api_key" /\ String apiKeyStr, "status" /\ String "ACTIVE"])
      case merchantKey of
        Right (Just mKey) -> do
          merchantAcc <- runDB conn $ do
            let predicate MerchantAccount {id} = id ==. B.val_ (mKey ^. _merchantAccountId  )
            findRow
              $ B.select
              $ B.limit_ 1
              $ B.filter_ predicate
              $ B.all_ (merchant_account eulerDBSchema)
           -- pure defaultMerchantAccount --DB.findOneWithErr ecDB
           -- (where_ := WHERE ["id" /\ Int (fromMaybe 0 (unNullOrUndefined merchantKey.merchantAccountId))]) ecAccessDenied
          merchantAccount <- case merchantAcc of
            Right (Just ma) -> pure $ setField @"apiKey" (Just key) ma-- merchantAcc # _apiKey .~ (just $ apiKeyStr)
            _               -> throwException err403
          _ <- pure True -- ipAddressFilters merchantAccount headers   --- checking IP whitelist in case of authenticated call
          pure $ (merchantAccount, True)
        Right Nothing -> throwException err403
        Left err -> do
          runIO $ putTextLn $ toText $ P.show err
          throwException $ myerr "2" -- liftErr ecAccessDenied
    Left err -> do
      logError "Authentication" $ "Invalid API key: " <> err
      throwException err403 {errBody = "Invalid API key."}

-- ----------------------------------------------------------------------------
-- function: updateAuthTokenUsage
-- TODO update
-- ----------------------------------------------------------------------------

{-PS
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
-}

-- from src/Types/Alias.purs
type AuthToken = Text

updateAuthTokenUsage :: AuthToken -> C.ClientAuthTokenData -> Flow ()
updateAuthTokenUsage authToken clientAuthToken@C.ClientAuthTokenData {..} = do
  let newUsageCount = maybe 1 (+1) usageCount
  case (newUsageCount == tokenMaxUsage) of
    True -> pure () -- delCachedValEC authToken *> pure ()
    False -> do
      tokenExpiryData <- pure C.defaultOrderTokenExpiryData -- unwrap <$> getTokenExpiryData
     -- let ttl = convertDuration $ Seconds $ toNumber tokenExpiryData.expiryInSeconds
      _ <- pure () --setCacheEC ttl authToken (clientAuthToken {usageCount = Just newUsageCount})
      pure ()


-- ----------------------------------------------------------------------------
-- function: authenticateReqWithClientAuthToken
-- TODO update
-- ----------------------------------------------------------------------------

{-PS
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
-}

authenticateReqWithClientAuthToken ::
 -- OrderStatusRequest
 -- ->
   AuthToken
 -- -> RouteParameters
  -> Flow (MerchantAccount, Bool)
authenticateReqWithClientAuthToken authToken = do
  maybeAuthTokenData :: Maybe C.ClientAuthTokenData <- pure $ Just C.defaultClientAuthTokenData -- getCachedValEC authToken
  case maybeAuthTokenData of
    Just authTokenData -> do
      _ <- pure () -- updateAuthTokenUsage authToken authTokenData
      merchantAccount <- getMerchantAccountForAuthToken authTokenData
      pure (merchantAccount, True)
    Nothing -> throwException err403


-- ----------------------------------------------------------------------------
-- function: getMerchantAccountForAuthToken
-- TODO update
-- ----------------------------------------------------------------------------

{-PS
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

-}

getMerchantAccountForAuthToken :: C.ClientAuthTokenData -> Flow MerchantAccount
getMerchantAccountForAuthToken (C.ClientAuthTokenData {..}) = do
  case resourceType of
    "ORDER" -> do
      OrderReference{..}  <- pure defaultOrderReference -- DB.findOneWithErr ecDB (where_ := WHERE ["id" /\ Int (parseInt otokenData.resourceId)]) ecAccessDenied
      merchantId <- maybe (throwException err500) pure merchantId
      pure defaultMerchantAccount -- DB.findOneWithErr ecDB (where_ := WHERE ["merchant_id" /\ String merchantId]) ecAccessDenied
    "CUSTOMER" -> do
      Customer {..} <- pure defaultCustomer -- DB.findOneWithErr ecDB (where_ := WHERE ["id" /\ String otokenData.resourceId]) ecAccessDenied
      pure defaultMerchantAccount -- DB.findOneWithErr ecDB (where_ := WHERE ["id" /\ Int merchantAccountId]) ecAccessDenied
    _          -> throwException err403


-- ----------------------------------------------------------------------------
-- function: rejectIfUnauthenticatedCallDisabled
-- TODO update
-- ----------------------------------------------------------------------------

{-PS
rejectIfUnauthenticatedCallDisabled :: MerchantAccount -> BackendFlow SyncStatusState _ Unit
rejectIfUnauthenticatedCallDisabled mAccnt =
  if isTrue (mAccnt ^. _enableUnauthenticatedOrderStatusApi)
  then continue unit
  else liftErr ecForbidden

-}

rejectIfUnauthenticatedCallDisabled :: MerchantAccount -> Flow ()
rejectIfUnauthenticatedCallDisabled mAccnt =
  case  (enableUnauthenticatedOrderStatusApi mAccnt) of
    Just True -> pure ()
    _         -> throwException $ myerr "1" -- ecForbidden
     -- * alot of errors predefined in Servant.Server
     -- * https://hackage.haskell.org/package/servant-server-0.16.2/docs/Servant-Server.html#v:err404
     -- * body message (and another parameters) can be redefined
     -- * err403 { errBody = "Please login first." }
     {- err403 :: ServerError

        data ServerError = ServerError
            { errHTTPCode     :: Int
            , errReasonPhrase :: String
            , errBody         :: LBS.ByteString
            , errHeaders      :: [HTTP.Header]
            }
          deriving (Show, Eq, Read, Typeable)

        instance Exception ServerError
     -}


-- ----------------------------------------------------------------------------
-- function: rejectIfMerchantIdMissing
-- TODO unsed
-- ----------------------------------------------------------------------------

{-PS
rejectIfMerchantIdMissing :: OrderStatusRequest -> BackendFlow SyncStatusState _ Unit
rejectIfMerchantIdMissing (OrderStatusRequest req) =
  let maybeMerchantId = unNullOrUndefined req.merchantId
                        <|> (unNullOrUndefined req.merchant_id)
  in
  if isJust maybeMerchantId
  then continue unit
  else liftErr merchantIdMissing
-}


-- ----------------------------------------------------------------------------
-- function: getCachedOrdStatus
-- TODO update
-- ----------------------------------------------------------------------------

{-PS
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
-}

getCachedOrdStatus -- really need orderId and merchantId
  :: Bool
 -- -> OrderStatusRequest
 -- -> MerchantAccount
 -- -> RouteParameters
  -> Text
  -> Text
  -> Flow  (Maybe OrderStatusResponse)
getCachedOrdStatus isAuthenticated orderId merchantId = do -- req mAccnt routeParam = do
  conn <- getConn eulerDB
  (maybeFeature :: Maybe Feature) <- do-- pure Nothing -- DB.findOne ecDB (where_ := WHERE [ "name" /\ String eulerOrderStatusCachingKey] :: WHERE Feature)
    res <- runDB conn $ do
      let predicate Feature {name} = name ==. B.val_ "eulerOrderStatusCachingKey"
      findRow
        $ B.select
        $ B.limit_ 1
        $ B.filter_ predicate
        $ B.all_ (feature eulerDBSchema)
    case res of
      Right (Just f) -> pure $ Just f
      Right Nothing -> pure Nothing
      Left err -> do
        logError "Find Feature" $ toText $ P.show err
        pure Nothing
  maybe (pure Nothing) (getCachedVal isAuthenticated merchantId orderId) maybeFeature
    where
      getCachedVal isAuthenticated merchantId orderId feature = do
        case (getField @"enabled" feature) of
          True -> do
            _ <- logInfo "Fetch cache from order status" $ "Order status cache feature is enabled"
           -- orderId <- getOrderId req routeParam
           -- merchantId <- unNullOrErr500 (mAccnt ^. _merchantId)
            val <- getCachedResp ((keyPrefix isAuthenticated) <> merchantId <> "_" <> orderId)
            case val of
              Just value -> do
                runIO $ Metric.incrementOrderStatusCacheHitCount merchantId
                _ <- logInfo "order status api response from cache" ("merchant_id " <> merchantId <> " orderId " <> orderId)
                _ <- logInfo "Fetch cache from order status" $ "Order status response found in cache for merchant_id " <> merchantId <> " orderId " <> orderId
                pure val
              Nothing -> do
                runIO $ Metric.incrementOrderStatusCacheMissCount merchantId
                _ <- logInfo "Fetch cache from order status" $ "Could not find order status response in cache for merchant_id " <> merchantId <> " orderId " <> orderId
                pure val
          False -> do
            _ <- logInfo "Fetch cache from order status" $ "Order status cache feature is not enabled"
            pure Nothing
      keyPrefix True  = "euler_ostatus_"
      keyPrefix False = "euler_ostatus_unauth_"


-- ----------------------------------------------------------------------------
-- function: addToCache
-- TODO port
-- ----------------------------------------------------------------------------

{-PS
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
-}

addToCache ::
     OrderStatusRequest
  -> Bool
  -> MerchantAccount
  -> RouteParameters
  -> OrderStatusResponse
  -> Flow ()
addToCache = undefined

-- ----------------------------------------------------------------------------
-- function: setRespInCache
-- TODO port
-- ----------------------------------------------------------------------------

{-PS
setRespInCache :: forall a b. Encode a => Milliseconds -> String -> a -> BackendFlow _ b Unit
setRespInCache expiry key val = do
  eitherRes <- setCacheWithExpiry ecRedis key (jsonStringify (snakeCaseToCamelCase (encode val))) expiry
  case eitherRes of
    Right x -> continue unit
    Left err -> log "cache_save_error_" ("Error while persisting " <> key <> "_" <> (show err)) *> continue unit
-}


-- ----------------------------------------------------------------------------
-- function: getCachedResp
-- TODO port
-- ----------------------------------------------------------------------------

{-PS
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
-}

getCachedResp :: Text -> Flow (Maybe OrderStatusResponse)
getCachedResp key = do
  eitherVal <- pure $ Right Nothing -- Presto.getCache ecRedis key
  case eitherVal of
    Right (Just v) -> pure $ Just v -- * do
      -- *  --let val' = S.replaceAll (S.Pattern "null") (S.Replacement (show "null")) v
      -- *  --    val = S.replaceAll (S.Pattern "\"\"null\"\"") (S.Replacement (show "null")) val'
      -- *  -- * Interesting transformations (maybe js - purescript related and we dont need it)
      -- *  -- * parseAndReplaceWithStringNull
      -- *  -- * camelCaseToSnakeCase
      -- *  -- * replaceObjValWithForeignNull
      -- *  let resp = fromMaybe (toForeign "") (parseAndReplaceWithStringNull Just Nothing v)
      -- *  _ <- Presto.log ("Cache value for this order status cache key " <> key) v
      -- *  case (runExcept (decode (camelCaseToSnakeCase resp))) of
      -- *    Right typedVal -> pure (replaceObjValWithForeignNull typedVal Just Nothing)
      -- *    Left err -> pure Nothing -- log "decode_error" ("Error while decoding cached value for " <> key <> "_" <> show err) *> pure Nothing
    Right Nothing  -> pure Nothing -- log "redis_cache_value_not_found" ("value not found for this key " <> key) *> pure Nothing
    Left err       -> pure Nothing -- log "redis_fetch_error" ("Error while getting value from cache " <> key <> "_" <> show err) *> pure Nothing

-- ----------------------------------------------------------------------------
-- function: getOrdStatusResp
-- TODO update
-- ----------------------------------------------------------------------------

{-PS
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
-}

-- should we divide this method to:
-- 1) getStatusResp, where we just get it from DB

-- 2) fillStatusResp, where we apply:
--  * fillOrderDetails
--  * addPromotionDetails
--  * addMandateDetails

-- 3) addTxnInfo (this method only for POST orderStatus api method) where we:
--  * addTxnDetailsToResponse
--  * addRiskCheckInfoToResponse
--  * addPaymentMethodInfo
--  * addRefundDetails
--  * addChargeBacks
--  * addGatewayResponse

-- TODO Можно использовать Сашину функцию, или в репозиторий
-- TODO naming (load)
getOrderReference
  :: OrderStatusQuery
  -- :: OrderStatusRequest
  -- -> MerchantAccount
  -- -> Bool
  -- -> Text -- RouteParameters
  -> Flow OrderReference
-- getOrdStatusResp req {- @(OrderStatusRequest ordReq) -} mAccnt isAuthenticated routeParam = do
getOrderReference query = do
    -- orderId'     <- pure routeParam -- getOrderId req routeParam
    let OrderId orderId' = getField @"orderId" query
    -- merchantId'  <- getMerchantId mAccnt -- unNullOrErr500 (mAccnt ^. _merchantId)
    let merchantId' = getField @"merchantId" query

    (order :: OrderReference) <- do
      conn <- getConn eulerDB
      res <- runDB conn $ do
        let predicate OrderReference {orderId, merchantId} = (orderId ==. B.just_ (B.val_ orderId'))
              &&. (merchantId ==. B.just_ (B.val_ merchantId'))
        findRow
          $ B.select
          $ B.limit_ 1
          $ B.filter_ predicate
          $ B.all_ (order_reference eulerDBSchema)
      case res of
        Right (Just ordRef) -> pure ordRef
        Right Nothing -> throwException err404 {errBody = "Order " <> show orderId' <> " not found."}
        Left err -> do
          logError "Find OrderReference" $ toText $ P.show err
          throwException err500

    pure order

      -- pure defaultOrderReference -- DB.findOneWithErr ecDB (where_ := WHERE ["order_id" /\ String orderId, "merchant_id" /\ String merchantId]) (orderNotFound orderId) ???

 -- WARNING Seemingly, txnUuid & txn_uuid are always Nothing in OrderStatusRequest
 -- Looks like `getLastTxn` always do the job.

 --   let maybeTxnUuid = (unNullOrUndefined ordReq.txnUuid) <|> (unNullOrUndefined ordReq.txn_uuid)
 --   maybeTxn    <- runMaybeT $ MaybeT (getTxnFromTxnUuid order maybeTxnUuid) <|> MaybeT (getLastTxn order)

    -- paymentlink <- getPaymentLink resellerId order
    -- ordResp    <- fillOrderDetails isAuthenticated paymentlink order defaultOrderStatusResponse
    --                 >>= addPromotionDetails order >>= addMandateDetails order
    -- ordResp     <- addMandateDetails order ordResp'


fillOrderStatusResponseTxn :: OrderStatusQuery -> TxnDetail -> OrderReference -> OrderStatusResponse -> Flow OrderStatusResponse
fillOrderStatusResponseTxn OrderStatusQuery{..} txn order ordResp = do
  addTxnDetailsToResponse txn order ordResp
    >>= addRiskCheckInfoToResponse txn
    >>= addPaymentMethodInfo sendCardIsin txn
    >>= addRefundDetails txn
    >>= addChargeBacks txn
    >>= addGatewayResponse txn sendFullGatewayResponse

   -- used in POST method
   --  case maybeTxn of
   --    Just txn -> do
   --      addTxnDetailsToResponse txn order ordResp
   --      >>= addRiskCheckInfoToResponse txn
   --      >>= addPaymentMethodInfo mAccnt txn
   --      >>= addRefundDetails txn
   --      >>= addChargeBacks txn
   --      >>= addGatewayResponse txn
   --    Nothing ->  pure ordResp
    -- pure ordResp


fillOrderStatusResponse :: OrderStatusQuery -> OrderReference -> Paymentlinks -> Flow OrderStatusResponse
fillOrderStatusResponse OrderStatusQuery{..} ordReference payLinks = do
  mkResponse isAuthenticated payLinks ordReference defaultOrderStatusResponse
    >>= addPromotionDetails ordReference
    >>= addMandateDetails ordReference


-- ----------------------------------------------------------------------------
-- function: getTxnFromTxnUuid
-- done
-- ----------------------------------------------------------------------------

{-PS
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
-}

-- TODO OS rewrite to Text -> TxnDetail and lift?
getTxnFromTxnUuid :: OrderReference -> Maybe Text -> Flow (Maybe TxnDetail)
getTxnFromTxnUuid order maybeTxnUuid =
  case maybeTxnUuid of
    Just txnUuid' -> do
      orderId' <- whenNothing (getField @"orderId" order) (throwException err500) -- unNullOrErr500 $ order ^. _orderId
      merchantId' <- whenNothing (getField @"merchantId" order) (throwException err500)--unNullOrErr500 $ order ^. _merchantId

      withDB eulerDB $ do
        let predicate TxnDetail {orderId, merchantId, txnUuid} =
              orderId ==. B.val_ orderId'
              &&. merchantId ==. B.just_ (B.val_ merchantId')
              &&. txnUuid ==. B.just_ (B.val_ txnUuid')
        findRow
          $ B.select
          $ B.limit_ 1
          $ B.filter_ predicate
          $ B.all_ (EDB.txn_detail eulerDBSchema)
      -- DB.findOne ecDB $ where_ := WHERE
      --   [ "order_id" /\ String orderId
      --   , "merchant_id" /\ String merchantId
      --   , "txn_uuid" /\ String txnUuid ] :: WHERE TxnDetail
    Nothing -> pure Nothing


-- ----------------------------------------------------------------------------
-- function: getOrderId
-- done
-- ----------------------------------------------------------------------------

{-PS
getOrderId :: forall st e r rt. Newtype st (TState e) => OrderStatusRequest -> RouteParameters -> BackendFlow st rt String
getOrderId (OrderStatusRequest req) routeParam = do
  ordId <- pure $ (StrMap.lookup "orderId" routeParam) <|> (StrMap.lookup "order_id" routeParam)
  let orderid = ordId <|> (unNullOrUndefined req.orderId) <|> (unNullOrUndefined req.order_id)
  maybe (liftErr badRequest) pure orderid
-}

getOrderId :: OrderStatusRequest -> RouteParameters -> Flow Text
getOrderId orderReq routeParam = do
  let ordId = lookupRP @OrderId routeParam
  let orderid = ordId <|> getField @"orderId" orderReq <|> getField @"order_id" orderReq
  maybe (throwException $ myerr400 "invalid_request") pure orderid


-- ----------------------------------------------------------------------------
-- function: getLastTxn
-- done
-- ----------------------------------------------------------------------------

{-PS
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
-}

-- TODO add sorting by dateCreated!
getLastTxn :: OrderReference -> Flow (Maybe TxnDetail)
getLastTxn orderRef = do
  orderId' <- whenNothing (getField @"orderId" orderRef) (throwException err500)
  merchantId' <- whenNothing (getField @"merchantId" orderRef) (throwException err500)

  txnDetails <- withDB eulerDB $ do
    let predicate TxnDetail {orderId, merchantId} =
          orderId ==. B.val_ orderId'
            &&. merchantId ==. B.just_ (B.val_ merchantId')
    findRows
      $ B.select
      $ B.filter_ predicate
      $ B.all_ (EDB.txn_detail eulerDBSchema)

  case txnDetails of
    [] -> do
      logError "get_last_txn" ("No last txn found for orderId: " <> orderId' <> " :merchant:" <> merchantId')
      pure Nothing
    _ -> do
      let chargetxn = find (\txn -> (getField @"status" txn == CHARGED)) txnDetails
      maybe (pure . Just $ head txnDetails) (pure . Just) chargetxn


-- ----------------------------------------------------------------------------
-- function: fillOrderDetails
-- done
-- ----------------------------------------------------------------------------

{-PS
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
-}

-- former fillOrderDetails
mkResponse
  :: Bool
  -> Paymentlinks
  -> OrderReference
  -> OrderStatusResponse
  -> Flow OrderStatusResponse
mkResponse isAuthenticated paymentLinks ord status = do
  -- let --resp = status
      -- ordObj = ord
  id <- whenNothing (orderUuid ord) (throwException $ myerr "4")-- unNullOrErr500 ordObj.orderUuid
  let nullVal = Nothing -- nullValue unit -- create foreign (JS) null value ???
  customerId    <- case (customerId ord) of
                    Just customerId -> pure customerId
                    Nothing         -> pure "" -- nullVal -- What is this? (Foreign null) ???
  customerEmail <- case (customerEmail ord) of
                      Just customerEmail -> if isAuthenticated then pure customerEmail else pure "" -- nullVal
                      Nothing -> pure "" -- nullVal
  customerPhone <- case (customerPhone ord) of
                      Just customerPhone -> if isAuthenticated then pure customerPhone else pure "" -- nullVal
                      Nothing -> pure "" -- nullVal
  returnUrl     <- getReturnUrl ord True -- &&
  pure $ (status :: OrderStatusResponse) -- wrap resp
       { id = id
       , merchant_id = (getField @"merchantId" ord)
       , order_id = (getField @"orderId" ord)
       , customer_id = Just customerId
       , product_id = fromMaybe "" (productId ord)
       , status = show $ (getField @"status" ord)
       , status_id = C.orderStatusToInt (getField @"status" ord) -- &&
       , amount =  sanitizeAmount <$> (getField @"amount" ord) -- &&
       , currency = (getField @"currency" ord)
       , refunded = (refundedEntirely ord)
       , payment_links = paymentLinks
       , amount_refunded = sanitizeNullAmount (amountRefunded ord) -- &&
       , date_created = (show (getField @"dateCreated" ord) )
       , customer_email = Just customerEmail
       , customer_phone = Just customerPhone
       , return_url = Just returnUrl
       , udf1 = fromMaybe "" (getField @"udf1" ord)
       , udf2 = fromMaybe "" (getField @"udf2" ord)
       , udf3 = fromMaybe "" (getField @"udf3" ord)
       , udf4 = fromMaybe "" (getField @"udf4" ord)
       , udf5 = fromMaybe "" (getField @"udf5" ord)
       , udf6 = fromMaybe "" (getField @"udf6" ord)
       , udf7 = fromMaybe "" (getField @"udf7" ord)
       , udf8 = fromMaybe "" (getField @"udf8" ord)
       , udf9 = fromMaybe "" (getField @"udf9" ord)
       , udf10 = fromMaybe "" (getField @"udf10" ord)
       }


-- main builder
makeOrderStatusResponse
  :: OrderReference
  -> Paymentlinks
  -> Maybe Promotion'
  -> Maybe Mandate'
  -> OrderStatusQuery
  -> Maybe TxnDetail
  -> Text -- use getGatewayReferenceId to get it
  -> Maybe Risk
  -> Maybe TxnCardInfo
  -> Maybe Text -- CardBrand
  -> Maybe [Refund']
  -> Maybe [Chargeback']
  -- After OrderReference be validated, it makeOrderStatusResponse will return OrderStatusResponse only
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
  = do

  -- Validation

  ordId <- liftEither $ maybe (Left "4") Right $ getField @ "orderUuid" ordRef -- (throwException $ myerr "4")

  let mCustomerId = whenNothing (getField @"customerId" ordRef) (Just "")
      email = (\email -> if isAuthenticated then email else Just "") (getField @"customerEmail" ordRef)
      phone = (\phone -> if isAuthenticated then phone else Just "") (getField @"customerPhone" ordRef)
      amount = fmap sanitizeAmount $ getField @ "amount" ordRef
      amountRefunded = fmap sanitizeAmount $ getField @"amountRefunded" ordRef

      getStatus = show . getField @"status"
      getStatusId = txnStatusToInt . getField @"status"
      getGatewayId txn = maybe 0 gatewayIdFromGateway $ join $ stringToGateway <$> getField @"gateway" txn
      getBankErrorCode txn = whenNothing (getField @"bankErrorCode" txn) (Just "")
      getBankErrorMessage txn = whenNothing (getField @"bankErrorMessage" txn) (Just "")
      getGatewayPayload txn = if isBlankMaybe (mGatewayPayload' txn) then (mGatewayPayload' txn) else Nothing
        where mGatewayPayload' t = getField @"gatewayPayload" t

      isEmi txn = isTrueMaybe (getField @"isEmi" txn)
      emiTenure txn = getField @"emiTenure" txn
      emiBank txn = getField @"emiBank" txn

      paymentMethod = whenNothing mCardBrand (Just "UNKNOWN")

      -- Abstract functions to elemenate boilerplate
      maybeTxnCard f = maybe emptyBuilder f mTxnCard
      maybeTxn f = maybe emptyBuilder f mTxn

-- Build pipeline

  pure $ extract $ buildStatusResponse
    -- end

    -- TODO addGatewayResponse here
    -- addGatewayResponse

    <<= changeChargeBacks mChargebacks
    -- addChargeBacks

    <<= changeRefund mRefunds
    -- addRefundDetails

      -- TODO: add addSecondFactorResponseAndTxnFlowInfo here
      -- addSecondFactorResponseAndTxnFlowInfo

    <<= maybeTxn (\txn -> maybeTxnCard (\txnCard ->
      if isBlankMaybe (getField @"cardIsin" txnCard)
        then changeCard (getCardDetails txnCard txn sendCardIsin)
        else emptyBuilder))

    <<= maybeTxn (const $ maybeTxnCard (\txnCard ->
      if isBlankMaybe (getField @"cardIsin" txnCard)
        then changePaymentMethodType "CARD"
        else emptyBuilder))

    <<= maybeTxn (const $ maybeTxnCard (\txnCard ->
      if isBlankMaybe (getField @"cardIsin" txnCard)
        then changeEmiPaymentMethod paymentMethod
        else emptyBuilder))
      -- addCardInfo

    <<= maybeTxn (const $ maybeTxnCard (\txnCard -> changeAuthType $ whenNothing (getField @"authType" txnCard) (Just "")))
      -- addAuthType
    <<= maybeTxn (\txn -> maybeTxnCard (const $ if isEmi txn then changeEmiTenureEmiBank (emiTenure txn) (emiBank txn) else emptyBuilder))
      -- addEmi
      -- TODO: add updatePaymentMethodAndType here
    -- addPaymentMethodInfo

    <<= changeRisk mRisk
    -- addRiskCheckInfoToResponse

    <<= maybeTxn (changeTxnDetails . mapTxnDetail)
    <<= maybeTxn (changeGatewayPayload . getGatewayPayload)
    <<= maybeTxn (changeBankErrorMessage . getBankErrorMessage)
    <<= maybeTxn (changeBankErrorCode . getBankErrorCode)
    <<= maybeTxn (const $ changeGatewayRefId gatewayRefId)
    <<= maybeTxn (changeGatewayId . getGatewayId)
    <<= maybeTxn (changeTxnUuid . getField @"txnUuid")
    <<= maybeTxn (changeTxnId . getField @"txnId")
    <<= maybeTxn (changeStatusId . getStatusId)
    <<= maybeTxn (changeStatus . getStatus) -- second status change when Just TxnDetail
    -- addTxnDetailsToResponse

    <<= changeMandate mMandate
    -- addMandateDetails

    <<= changeAmountIf mPromotion
    <<= changePromotion mPromotion
    -- addPromotionDetails

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
    <<= changeReturnUrl (getField @"returnUrl" ordRef)
    <<= changeCustomerPhone phone
    <<= changeCustomerEmail email
    <<= changeDateCreated (show $ getField @"dateCreated" ordRef)
    <<= changeAmountRefunded amountRefunded
    <<= changePaymentLinks paymentLinks
    <<= changeRefunded (getField @"refundedEntirely" ordRef)
    <<= changeCurrency (getField @"currency" ordRef)
    <<= changeAmount amount
    <<= changeStatus (show $ getField @ "status" ordRef) -- first status change
    <<= changeProductId (getField @"productId" ordRef)
    <<= changeCustomerId mCustomerId
    <<= changeOrderId (getField @"orderId" ordRef)
    <<= changeMerchantId (getField @"merchantId" ordRef)
    <<= changeId ordId
    -- mkResponse

    -- begin (reverse direction)
    -- последовательность имеет значение


-- > extract $ buildStatusResponse <<= changeId "hello" <<= changeId "world"
-- OrderStatusResponse {id = "world", merchant_id = Nothing, ...


-- Begin

-- TODO abstract change functions to reduce boilerplate

-- former fillOrderDetails
-- mkResponse
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

changeReturnUrl :: Maybe Text -> ResponseBuilder -> OrderStatusResponse
changeReturnUrl returnUrl builder = builder $ mempty {return_urlT = map Last returnUrl}

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


-- addPromotionDetails


  --         let amount  = (fromMaybe 0 $ getField @"amount" orderStatus) + (fromMaybe 0 $ getField @"discount_amount" promotion)
  --             ordS    = setField @"amount" (Just $ sanitizeAmount amount) orderStatus -- # _amount .~ (just $ sanitizeAmount amount)

  --         pure $ setField @"promotion" (Just promotion) ordS -- # _promotion .~ (just promotion)

-- wrong logic
-- > extract $ buildStatusResponse <<= changePromotion (Just promotion2) <<= changePromotion (Just promotion1)
-- OrderStatusResponse {id = "", merchant_id = Nothing, amount = Just 7.0, ...

-- > extract $ buildStatusResponse <<= changePromotion (Just promotion1) <<= changePromotion (Just promotion2)
-- OrderStatusResponse {id = "", merchant_id = Nothing, amount = Just 5.0, ...

changePromotion :: Maybe Promotion' -> ResponseBuilder -> OrderStatusResponse
changePromotion Nothing builder = builder mempty
changePromotion mNewProm builder = builder mempty { promotionT = fmap Last mNewProm }

changeAmountIf :: Maybe Promotion' -> ResponseBuilder -> OrderStatusResponse
changeAmountIf Nothing builder = builder mempty
changeAmountIf (Just newProm) builder =
  let oldStatus = extract builder
  -- TODO: amount logic is wrong now, when used more then one changePromotion
      mOldAmount = getField @"amount" oldStatus
      mOldPromotion = getField @"discount_amount" newProm
      newAmount = sanitizeAmount $ (fromMaybe 0 mOldAmount) + (fromMaybe 0 mOldPromotion)
  in builder mempty
      { amountT = Just (Last newAmount)
      }


-- addMandateDetails


changeMandate :: Maybe Mandate' -> ResponseBuilder -> OrderStatusResponse
changeMandate mandate builder = builder $ mempty { mandateT = fmap Last mandate}


-- addTxnDetailsToResponse


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


-- addRiskCheckInfoToResponse

changeRisk :: Maybe Risk -> ResponseBuilder -> OrderStatusResponse
changeRisk risk builder = builder $ mempty {riskT = fmap Last risk}


-- addPaymentMethodInfo

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


-- addRefundDetails

changeRefund :: Maybe [Refund'] -> ResponseBuilder -> OrderStatusResponse
changeRefund mRefunds builder = builder $ mempty {refundsT = fmap Last mRefunds}


-- addChargeBacks
changeChargeBacks :: Maybe [Chargeback'] -> ResponseBuilder -> OrderStatusResponse
changeChargeBacks mChargebacks builder = builder $ mempty {chargebacksT = fmap Last mChargebacks}

-- End



-- For checking. TODO remove
promotion1 :: Promotion'
promotion1 = Promotion'
  { id              = Nothing
  , order_id        = Nothing
  , rules           = Nothing
  , created         = Nothing
  , discount_amount = Just 1
  , status          = Just "hello"
  }

-- For checking. TODO remove
promotion2 :: Promotion'
promotion2 = Promotion'
  { id              = Nothing
  , order_id        = Nothing
  , rules           = Nothing
  , created         = Nothing
  , discount_amount = Just 3
  , status          = Just "world"
  }

-- For checking. TODO remove
-- extract $ buildStatusResponse <<= changeMandate (Just mandate1) <<= changePromotion (Just promotion2) <<= changeAmount (Just 2)
mandate1 = Mandate'
  { mandate_token  = "token"
  , mandate_status = Just "status"
  , mandate_id     = "man_id"
  }


-- ----------------------------------------------------------------------------
-- function: formatAmount
-- TODO unused
-- ported version uses sanitizeAmount/sanitizeNullAmount instead
-- ----------------------------------------------------------------------------

{-PS
formatAmount ::forall st rt. NullOrUndefined Number -> BackendFlow st rt (NullOrUndefined Number)
formatAmount (NullOrUndefined Nothing) = pure $ nothing
formatAmount (NullOrUndefined (Just amt)) = do
  amount <- roundOff2 amt
  pure $ just $ amount
-}


-- ----------------------------------------------------------------------------
-- function: addMandateDetails
-- TODO update
-- ----------------------------------------------------------------------------

{-PS
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
-}

-- Became loadMandate and changeMandate
addMandateDetails :: OrderReference -> OrderStatusResponse -> Flow OrderStatusResponse
addMandateDetails ordRef orderStatus =
  case (orderType ordRef) of
    Just orderType ->
      if orderType == C.MANDATE_REGISTER then do
        orderId <- pure $ getField @"id" ordRef  -- unNullOrErr500 $ ordRef ^. _id
        mandate :: Maybe Mandate <- do
          conn <- getConn eulerDB
          let predicate Mandate {authOrderId, merchantId} = authOrderId ==. B.val_ orderId
                &&. merchantId ==. (B.val_ $ fromMaybe "" $ ordRef ^. _merchantId)
          res <- runDB conn $
            findRow
              $ B.select
              $ B.limit_ 1
              $ B.filter_ predicate
              $ B.all_ (eulerDBSchema ^. _mandate)
          case res of
            Right m -> pure m
            Left err -> do
              logError "Find Mandate" $ toText $ P.show err
              throwException err500
          -- pure Nothing -- DB.findOne ecDB (where_ := WHERE ["auth_order_id" /\ Int orderId, "merchant_id" /\ String (unNull (ordRef ^._merchantId) "")])
        case mandate of
          Just mandateVal -> pure $ setField @"mandate" (Just $ mapMandate $ mandateVal) orderStatus -- # _mandate .~ (just $ mapMandate $ mandateVal)
          Nothing -> pure $ orderStatus
        else pure $ orderStatus
    Nothing -> pure $ orderStatus



-- from src/Types/Storage/EC/Mandate.purs
mapMandate :: Mandate -> Mandate'
mapMandate Mandate {..} =
   Mandate' {  mandate_token = token
             , mandate_status = Just $ show $ status
             , mandate_id = mandateId
            }


-- refactoring


loadMandate :: OrderReference -> Flow (Maybe Mandate')
loadMandate ordRef =
  -- TODO: should we move logic from here?
  case (orderType ordRef) of
    Just orderType ->
      if orderType == C.MANDATE_REGISTER then do
        let orderId = getField @"id" ordRef
        mandate <- do
            conn <- getConn eulerDB
            let predicate Mandate {authOrderId, merchantId} = authOrderId ==. B.val_ orderId
                  &&. merchantId ==. (B.val_ $ fromMaybe "" $ ordRef ^. _merchantId)
            res <- runDB conn $
              findRow
                $ B.select
                $ B.limit_ 1
                $ B.filter_ predicate
                $ B.all_ (eulerDBSchema ^. _mandate)
            case res of
              Right mMandate -> pure mMandate
              Left err -> do
                logError "Find Mandate" $ toText $ P.show err
                throwException err500
        pure $ mapMandate <$> mandate
      else pure Nothing
    Nothing -> pure Nothing






-- ----------------------------------------------------------------------------
-- function: getPaymentLink
-- TODO update
-- ----------------------------------------------------------------------------

{-PS
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
-}

-- former getPaymentLink
mkPaymentLinks
  :: Maybe Text -- resellerId
  -> Text       -- orderUuid (from OrderReference)
  -> Flow Paymentlinks
mkPaymentLinks resellId orderUuid = do
 -- maybeResellerAccount :: Maybe ResellerAccount <- DB.findOne ecDB (where_ := WHERE ["reseller_id" /\ String (unNull (account ^._resellerId) "")] :: WHERE ResellerAccount)
 -- (maybeResellerAccount :: Maybe ResellerAccount) <- pure $ Just defaultResellerAccount


  -- TODO I suppose this contradicts the common sence -- reseller is optional
  -- TODO use ResellerAccount's repository
  maybeResellerEndpoint <- do
    conn <- getConn eulerDB
    res  <- runDB conn $ do
      let predicate ResellerAccount {resellerId} = resellerId ==. B.val_ (fromMaybe "" resellId)
      findRow
        $ B.select
        $ B.limit_ 1
        $ B.filter_ predicate
        $ B.all_ (reseller_account eulerDBSchema)
    case res of
      Right mRAcc -> pure $ (^. _resellerApiEndpoint) =<< mRAcc
     -- Right Nothing -> pure Nothing
      Left err -> do
        logError "Find ResellerAccount" $ toText $ P.show err
        throwException err500

  -- let maybeResellerEndpoint = maybe Nothing ( getField @"resellerApiEndpoint") maybeResellerAccount
  -- TODO I think the link doesn't make sense if orderUuid is empty
  pure $ createPaymentLinks orderUuid maybeResellerEndpoint


-- ----------------------------------------------------------------------------
-- function: createPaymentLinks
-- TODO update
-- ----------------------------------------------------------------------------

{-PS
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
-}



createPaymentLinks
  :: Text           -- orderUuid (possibly blank string)
  -> Maybe Text     -- maybeResellerEndpoint
  -> Paymentlinks
createPaymentLinks orderUuid maybeResellerEndpoint =
  Paymentlinks
    { web =   Just (host <> "/merchant/pay/") <> Just orderUuid
    , mobile =   Just (host <> "/merchant/pay/") <> Just orderUuid <> Just "?mobile=true"
    , iframe =   Just (host <> "/merchant/ipay/") <> Just orderUuid
  }
  where
    config = defaultConfig -- getECRConfig -- from (src/Config/Config.purs) looks like constant, but depend on ENV
    {-
     data Config = Config
      { protocol :: String
      , host :: String
      , internalECHost :: String
      }
    -}
    protocol = getField @"protocol" config-- (unwrap config).protocol
    host = maybe (protocol <> "://" <> (getField @"host" config)) P.id maybeResellerEndpoint


-- ----------------------------------------------------------------------------
-- function: getReturnUrl
-- TODO update
-- ----------------------------------------------------------------------------

{-PS
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
-}

getReturnUrl ::  OrderReference -> Bool -> Flow Text
getReturnUrl orderRef somebool = do
  conn <- getConn eulerDB
  merchantAccount <- do
    res <- runDB conn $ do
      let predicate MerchantAccount {merchantId} = merchantId ==. B.just_ (B.val_ $ fromMaybe "" $ orderRef ^. _merchantId)
      findRow
        $ B.select
        $ B.limit_ 1
        $ B.filter_ predicate
        $ B.all_ (merchant_account eulerDBSchema)
    case res of
      Right mMAcc -> pure mMAcc
      Left err -> do
        logError "Find MerchantAccount" $ toText $ P.show err
        throwException err500
   -- pure $ Just defaultMerchantAccount -- DB.findOne ecDB (where_ := WHERE ["merchant_id" /\ String (unNull (orderRef ^._merchantId) "")] :: WHERE MerchantAccount)
  case merchantAccount of
    Just merchantAcc -> do
          merchantIframePreferences <- do
            res <- runDB conn $ do
              let predicate MerchantIframePreferences {merchantId} = merchantId ==. (B.val_ $ fromMaybe "" $ merchantAcc ^. _merchantId)
              findRow
                $ B.select
                $ B.limit_ 1
                $ B.filter_ predicate
                $ B.all_ (merchant_iframe_preferences eulerDBSchema)
            case res of
              Right mMIP -> pure mMIP
              Left err -> do
                logError "SQLDB Interraction." $ toText $ P.show err
                throwException err500
          -- pure $ Just defaultMerchantIframePreferences -- DB.findOne ecDB (where_ := WHERE ["merchant_id" /\ String merchantId] :: WHERE MerchantIframePreferences)
          let merchantIframeReturnUrl = fromMaybe "" (getField @"returnUrl"  =<< merchantIframePreferences)
     -- not used         mirrorGatewayResponse   = maybe Nothing (unNullOrUndefined <<< _.mirrorGatewayResponse <<< unwrap) merchantIframePreferences
              orderRefReturnUrl       = fromMaybe "" (getField @"returnUrl" orderRef )
          if (orderRefReturnUrl == "")
            then pure $ fromMaybe merchantIframeReturnUrl (getField @"returnUrl" merchantAcc )
            else pure orderRefReturnUrl
    Nothing -> pure $ ""

-- ----------------------------------------------------------------------------
-- function: getChargedTxn
-- done
-- ----------------------------------------------------------------------------

{-PS
getChargedTxn ::forall st rt e. Newtype st (TState e) => OrderReference -> BackendFlow st _ (Maybe TxnDetail)
getChargedTxn orderRef = do
  orderId    <- unNullOrErr500 $ orderRef ^. _orderId
  merchantId <- unNullOrErr500 $ orderRef ^. _merchantId
  txns <- DB.findAll ecDB (where_ := WHERE ["order_id" /\ String orderId, "merchant_id" /\ String merchantId] :: WHERE TxnDetail)
  case (length txns) of
    0 -> pure Nothing
    _ -> pure $ find (\txn -> (txn ^. _status == CHARGED)) txns
-}

getChargedTxn :: OrderReference -> Flow (Maybe TxnDetail)
getChargedTxn orderRef = do
  orderId' <- whenNothing (getField @"orderId" orderRef) (throwException err500)
  merchantId' <- whenNothing (getField @"merchantId" orderRef) (throwException err500)

  txnDetails <- withDB eulerDB $ do
    let predicate TxnDetail {orderId, merchantId} =
          orderId ==. B.val_ orderId'
            &&. merchantId ==. B.just_ (B.val_ merchantId')
    findRows
      $ B.select
      $ B.filter_ predicate
      $ B.all_ (EDB.txn_detail eulerDBSchema)

  case txnDetails of
    [] -> pure Nothing
    _  -> pure $ find (\txn -> (getField @"status" txn == CHARGED)) txnDetails


-- ----------------------------------------------------------------------------
-- function: addPromotionDetails
-- TODO update
-- It workarounded while Promotions's orderReferenceId is not changed to Text
-- ----------------------------------------------------------------------------

{-PS
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
-}

-- Became loadPromotions, decryptActivePromotion and changePromotion
addPromotionDetails :: OrderReference -> OrderStatusResponse -> Flow OrderStatusResponse
addPromotionDetails orderRef orderStatus = do
  -- Order contains two id -like fields (better names?)
  let orderId = fromMaybe "" $ getField @"id" orderRef
      -- OrderReference's orderId was changed to Text type, Promotions's orderReferenceId is still Int type at PS.
      -- readMaybe used to equal them in predicate. It is just workaround while Promotions not solved
      orderId' = fromMaybe 0 $ readMaybe $ T.unpack orderId
      ordId = fromMaybe "" $ getField @"orderId" orderRef
  promotions <- do
    conn <- getConn eulerDB
    res  <- runDB conn $ do
      let predicate Promotions{orderReferenceId} =
            orderReferenceId ==. B.just_ (B.val_ orderId')
      findRows
        $ B.select
        $ B.filter_ predicate
        $ B.all_ (promotions eulerDBSchema)
    case res of
      Right proms -> pure proms
      Left err -> do
        logError "Find Promotions" $ toText $ P.show err
        throwException err500
   -- pure [] -- DB.findAll ecDB (where_ := WHERE ["order_reference_id" /\ Int orderId] :: WHERE Promotions)
  case (length promotions) of
    0 -> pure orderStatus
    _ -> do
      promotion' <- pure $ find (\promotion -> (getField @"status" promotion == "ACTIVE" )) promotions
      case promotion' of
        Just promotionVal -> do
          promotion  <- decryptPromotionRules ordId promotionVal
          let amount  = (fromMaybe 0 $ getField @"amount" orderStatus) + (fromMaybe 0 $ getField @"discount_amount" promotion)
              ordS    = setField @"amount" (Just $ sanitizeAmount amount) orderStatus -- # _amount .~ (just $ sanitizeAmount amount)
          pure $ setField @"promotion" (Just promotion) ordS -- # _promotion .~ (just promotion)
        Nothing -> pure orderStatus


-- refactoring


loadPromotions :: OrderReference -> Flow (Text, [Promotions])
loadPromotions orderRef = do
  -- Order contains two id -like fields (better names?)
  let orderId = fromMaybe "" $ getField @"id" orderRef
      -- OrderReference's orderId was changed to Text type, Promotions's orderReferenceId is still Int type at PS.
      -- readMaybe used to equal them in predicate. It is just workaround while Promotions not solved
      orderId' = fromMaybe 0 $ readMaybe $ T.unpack orderId
      ordId = fromMaybe "" $ getField @"orderId" orderRef
  promotions <- do
    conn <- getConn eulerDB
    res  <- runDB conn $ do
      let predicate Promotions{orderReferenceId} =
            orderReferenceId ==. B.just_ (B.val_ orderId')
      findRows
        $ B.select
        $ B.filter_ predicate
        $ B.all_ (promotions eulerDBSchema)
    case res of
      Right proms -> pure proms
      Left err -> do
        logError "Find Promotions" $ toText $ P.show err
        throwException err500
  pure (ordId, promotions)

-- May it become pure after decryptPromotionRules be refactored
decryptActivePromotion :: (Text, [Promotions]) -> Flow (Maybe Promotion')
decryptActivePromotion (_,[]) = pure Nothing
decryptActivePromotion (ordId, promotions) = do
  let mPromotion = find (\promotion -> (getField @"status" promotion == "ACTIVE" )) promotions
  traverse (decryptPromotionRules ordId) mPromotion

getPromotion :: OrderReference -> Flow (Maybe Promotion')
getPromotion orderRef = do
  proms <- loadPromotions orderRef
  decryptActivePromotion  proms

-- ----------------------------------------------------------------------------
-- function: decryptPromotionRules
-- TODO port
-- ----------------------------------------------------------------------------

{-PS
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
-}

decryptPromotionRules :: Text -> Promotions -> Flow Promotion'
decryptPromotionRules ordId promotions = pure defaultPromotion'


-- ----------------------------------------------------------------------------
-- function: addTxnDetailsToResponse
-- done
-- ----------------------------------------------------------------------------

{-PS
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
-}

addTxnDetailsToResponse :: TxnDetail -> OrderReference -> OrderStatusResponse -> Flow OrderStatusResponse
addTxnDetailsToResponse txn ordRef orderStatus = do
  let gateway   = fromMaybe "" (getField @"gateway" txn)
      gatewayId = maybe 0 gatewayIdFromGateway $ stringToGateway gateway
  gatewayRefId <- getGatewayReferenceId txn ordRef
  logInfo "gatewayRefId " gatewayRefId
  pure (orderStatus
    { status = show $ getField @"status" txn
    , status_id = txnStatusToInt $ getField @"status" txn
    , txn_id = Just $ getField @"txnId" txn
    , txn_uuid = getField @"txnUuid" txn
    , gateway_id = Just gatewayId
    , gateway_reference_id = Just gatewayRefId
    , bank_error_code = whenNothing (getField @"bankErrorCode" txn) (Just "")
    , bank_error_message = whenNothing (getField @"bankErrorMessage" txn) (Just "")
    , gateway_payload = addGatewayPayload txn
    , txn_detail = Just $ mapTxnDetail txn
    } :: OrderStatusResponse)
  where addGatewayPayload txn =
         if (isBlankMaybe $ getField @"gatewayPayload" txn)
           then getField @"gatewayPayload" txn
           else Nothing



-- ----------------------------------------------------------------------------
-- function: mapTxnDetail
-- added from Types.Communication.OLTP.OrderStatus
-- done
-- ----------------------------------------------------------------------------

{-
mapTxnDetail :: TxnDetail -> TxnDetail'
mapTxnDetail (TxnDetail txn) =
    TxnDetail' {
          txn_id : txn.txnId,
          order_id : txn.orderId,
          txn_uuid : txn.txnUuid,
          gateway_id : just (maybe 0 gatewayIdFromGateway $ stringToGateway (unNull txn.gateway "")),
          status : show txn.status,
          gateway : txn.gateway,
          express_checkout : txn.expressCheckout,
          redirect : txn.redirect,
          net_amount : just (if (isPresent txn.netAmount) then toForeign (unNull txn.netAmount 0.0) else nullValue unit),
          surcharge_amount : just (if (isPresent txn.surchargeAmount) then toForeign (unNull txn.surchargeAmount 0.0) else nullValue unit),
          tax_amount : just (if (isPresent txn.taxAmount) then toForeign (unNull txn.taxAmount 0.0) else nullValue unit),
          txn_amount : just (if (isPresent txn.txnAmount) then toForeign (unNull txn.txnAmount 0.0) else nullValue unit),
          currency : txn.currency,
          error_message : just (unNull txn.bankErrorMessage ""),
          error_code : just (if (isPresent txn.bankErrorCode) then toForeign (unNull txn.bankErrorCode "") else nullValue unit),
          created : txn.dateCreated,
          txn_object_type : if (unNull txn.txnObjectType "") /= "ORDER_PAYMENT" then txn.txnObjectType else nothing,
          source_object : if (unNull txn.txnObjectType "") /= "ORDER_PAYMENT" then txn.sourceObject else nothing,
          source_object_id : if (unNull txn.txnObjectType "") /= "ORDER_PAYMENT" then txn.sourceObjectId else nothing
        }
-}

mapTxnDetail :: TxnDetail -> TxnDetail'
mapTxnDetail txn = TxnDetail'
  { txn_id = getField @"txnId" txn
  , order_id = getField @"orderId" txn
  , txn_uuid = getField @"txnUuid" txn
  , gateway_id = Just $ maybe 0 gatewayIdFromGateway $ stringToGateway $ fromMaybe mempty $ getField @"gateway" txn
  , status = show $ getField @"status" txn
  , gateway = getField @"gateway" txn
  , express_checkout = getField @"expressCheckout" txn
  , redirect = getField @"redirect" txn
  , net_amount = Just $ if isJust (getField @"netAmount" txn)
      then show $ fromMaybe 0 (getField @"netAmount" txn) -- Forign becomes Text in our TxnDetail'
      else mempty
  , surcharge_amount = Just $ if isJust (getField @"surchargeAmount" txn)
      then show $ fromMaybe 0 (getField @"surchargeAmount" txn)
      else mempty
  , tax_amount = Just $ if isJust (getField @"taxAmount" txn)
      then show $ fromMaybe 0 (getField @"taxAmount" txn)
      else mempty
  , txn_amount = Just $ if isJust (getField @"txnAmount" txn)
      then show $ fromMaybe 0 (getField @"txnAmount" txn)
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

-- ----------------------------------------------------------------------------
-- function: getGatewayReferenceId
-- done
-- ----------------------------------------------------------------------------

{-PS
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
-}

getGatewayReferenceId :: TxnDetail -> OrderReference -> Flow Text
getGatewayReferenceId txn ordRef = do

  let ordRefId = fromMaybe "" (getField @"id" ordRef)
  ordMeta <- withDB eulerDB $ do
        let predicate OrderMetadataV2 {orderReferenceId} =
              orderReferenceId ==. B.val_ ordRefId
        findRow
          $ B.select
          $ B.limit_ 1
          $ B.filter_ predicate
          $ B.all_ (EDB.order_metadata_v2 eulerDBSchema)


  case ordMeta of
    Just ordM ->
      case (blankToNothing (getField @"metadata" ordM)) of
        Just md -> do
          let md' = (decode $ BSL.fromStrict $ T.encodeUtf8 md) :: Maybe (Map Text Text)
          case md' of
            Nothing -> checkGatewayRefIdForVodafone ordRef txn
            Just metadata -> do
              gRefId <- pure $ Map.lookup ((fromMaybe "" $ getField @"gateway" txn) <> ":gateway_reference_id") metadata
              jusId  <- pure $ Map.lookup "JUSPAY:gateway_reference_id" metadata
              case (gRefId <|> jusId) of
                Just v  -> pure v
                Nothing -> checkGatewayRefIdForVodafone ordRef txn
        Nothing -> checkGatewayRefIdForVodafone ordRef txn
    Nothing -> checkGatewayRefIdForVodafone ordRef txn


loadOrderMetadataV2 :: Text -> Flow (Maybe OrderMetadataV2)
loadOrderMetadataV2 ordRefId = withDB eulerDB $ do
  let predicate OrderMetadataV2 {orderReferenceId} =
        orderReferenceId ==. B.val_ ordRefId
  findRow
    $ B.select
    $ B.limit_ 1
    $ B.filter_ predicate
    $ B.all_ (EDB.order_metadata_v2 eulerDBSchema)


-- partly refactored getGatewayReferenceId
getGatewayReferenceId2 :: Text -> OrderReference -> Flow Text
getGatewayReferenceId2 gateway ordRef = do
  let checkGateway = checkGatewayRefIdForVodafone2 ordRef gateway

  case getField @"id" ordRef of
    Nothing -> checkGateway
    Just refId -> do
      ordMeta <- loadOrderMetadataV2 refId

      case ordMeta of
        Just (ordM :: OrderMetadataV2) ->
          case blankToNothing (getField @"metadata" ordM) of
            Nothing -> checkGateway
            Just md -> do
              let md' = decode $ BSL.fromStrict $ T.encodeUtf8 md :: Maybe (Map Text Text)
              case md' of
                Nothing -> checkGateway
                Just metadata -> do
                  gRefId <- pure $ Map.lookup (gateway <> ":gateway_reference_id") metadata
                  jusId  <- pure $ Map.lookup "JUSPAY:gateway_reference_id" metadata
                  case (gRefId <|> jusId) of
                    Just v  -> pure v
                    Nothing -> checkGateway


-- ----------------------------------------------------------------------------
-- function: checkGatewayRefIdForVodafone
-- done
-- ----------------------------------------------------------------------------

checkGatewayRefIdForVodafone :: OrderReference -> TxnDetail -> Flow Text
checkGatewayRefIdForVodafone ordRef txn = do
  merchantId' <- whenNothing (getField @"merchantId" ordRef) (throwException err500) -- ordRef .^. _merchantId

  meybeFeature <- withDB eulerDB $ do
    let predicate Feature {name, merchantId} =
          name ==. B.val_ "USE_UDF2_FOR_GATEWAY_REFERENCE_ID"
          &&. merchantId ==. B.just_ (B.val_ merchantId')
    findRow
      $ B.select
      $ B.limit_ 1
      $ B.filter_ predicate
      $ B.all_ (EDB.feature eulerDBSchema)


  case meybeFeature of
    Just feature ->
        if ((fromMaybe "" $ getField @"gateway" txn) == "HSBC_UPI")
            && (getField @"enabled" feature)
            && (isJust $ getField @"udf2" ordRef)
        then pure $ fromMaybe "" $ getField @"udf2" ordRef
        else pure mempty -- nullValue unit
    Nothing -> pure mempty -- nullValue unit


-- Partly refactored
checkGatewayRefIdForVodafone2 :: OrderReference -> Text -> Flow Text
checkGatewayRefIdForVodafone2 ordRef gateway = do
  merchantId' <- whenNothing (getField @"merchantId" ordRef) (throwException err500) -- ordRef .^. _merchantId

  meybeFeature <- withDB eulerDB $ do
    let predicate Feature {name, merchantId} =
          name ==. B.val_ "USE_UDF2_FOR_GATEWAY_REFERENCE_ID"
          &&. merchantId ==. B.just_ (B.val_ merchantId')
    findRow
      $ B.select
      $ B.limit_ 1
      $ B.filter_ predicate
      $ B.all_ (EDB.feature eulerDBSchema)


  case meybeFeature of
    Just feature ->
        if (gateway == "HSBC_UPI")
            && (getField @"enabled" feature)
            && (isJust $ getField @"udf2" ordRef)
        then pure $ fromMaybe "" $ getField @"udf2" ordRef
        else pure mempty -- nullValue unit
    Nothing -> pure mempty -- nullValue unit

-- ----------------------------------------------------------------------------
-- function: addRiskCheckInfoToResponse
-- done
-- ----------------------------------------------------------------------------

{-PS
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
-}

-- Became loadTxnRiskCheck, loadRiskManagementAccount, makeRisk', makeRisk, changeRisk
addRiskCheckInfoToResponse :: TxnDetail -> OrderStatusResponse -> Flow OrderStatusResponse
addRiskCheckInfoToResponse txn orderStatus = do
  let txnId = fromMaybe T.empty $ getField @"id" txn

-- loadTxnRiskCheck
  txnRiskCheck <- withDB eulerDB $ do
    let predicate TxnRiskCheck {txnDetailId} = txnDetailId ==. B.val_ txnId
    findRow
      $ B.select
      $ B.limit_ 1
      $ B.filter_ predicate
      $ B.all_ (EDB.txn_risk_check eulerDBSchema)

-- loadRiskManagementAccount
  case txnRiskCheck of
    Just trc -> do
      let riskMAId = getField @"riskManagementAccountId" trc

      riskMngAcc <- withDB eulerDB $ do
        let predicate RiskManagementAccount {id} = id ==. B.val_ riskMAId
        findRow
          $ B.select
          $ B.limit_ 1
          $ B.filter_ predicate
          $ B.all_ (EDB.risk_management_account eulerDBSchema)

-- makeRisk'
      let risk = Risk'
            { provider = getField @"provider" <$> riskMngAcc
            , status = getField @"status" trc
            , message = getField @"message" trc
            , flagged = whenNothing (getField @"flagged" trc) (Just False)
            , recommended_action = whenNothing (getField @"recommendedAction" trc) (Just T.empty)
            , ebs_risk_level = Nothing
            , ebs_payment_status = Nothing
            , ebs_risk_percentage = Nothing
            , ebs_bin_country = Nothing
            }

-- makeRisk
      if (fromMaybe T.empty (getField @"provider" risk)) == "ebs"
        then do
            -- TODO not sure is this reliable enough? how port it?
            -- completeResponseJson <- xml2Json (trc ^. _completeResponse)
            -- outputObjectResponseJson <- xml2Json (trc ^. _completeResponse)
            -- responseObj <- pure $ fromMaybe emptyObj (lookupJson "RMSIDResult" completeResponseJson)
            -- outputObj   <- pure $ fromMaybe emptyObj (maybe Nothing (lookupJson "Output") (lookupJson "RMSIDResult" outputObjectResponseJson))
          let r' = undefined :: Risk'
            -- let r' = wrap (unwrap risk) {
            --   ebs_risk_level = NullOrUndefined $ maybe Nothing (Just <<< getString) (lookupJson "RiskLevel" responseObj) ,
            --   ebs_payment_status = (trc ^. _riskStatus),
            --   ebs_risk_percentage = NullOrUndefined $ maybe Nothing fromString (lookupJson "RiskPercentage" responseObj) ,
            --   ebs_bin_country = NullOrUndefined $ maybe Nothing (Just <<< getString) (lookupJson "Bincountry" (outputObj))
            -- }
          r  <- addRiskObjDefaultValueAsNull r'
          pure $ setField @"risk" (Just r) orderStatus
        else do
          r <- addRiskObjDefaultValueAsNull risk
          pure $ setField @"risk" (Just r) orderStatus
    Nothing -> pure orderStatus
  where getString a = "" -- TODO: if (isNotString a) then "" else a



-- refactoring


loadTxnRiskCheck :: Text -> Flow (Maybe TxnRiskCheck)
loadTxnRiskCheck txnId =
  -- let txnId = fromMaybe T.empty $ getField @"id" txn
  withDB eulerDB $ do
    let predicate TxnRiskCheck {txnDetailId} = txnDetailId ==. B.val_ txnId
    findRow
      $ B.select
      $ B.limit_ 1
      $ B.filter_ predicate
      $ B.all_ (EDB.txn_risk_check eulerDBSchema)


loadRiskManagementAccount :: Int -> Flow (Maybe RiskManagementAccount)
loadRiskManagementAccount riskMAId =
  withDB eulerDB $ do
    let predicate RiskManagementAccount {id} = id ==. B.val_ riskMAId
    findRow
      $ B.select
      $ B.limit_ 1
      $ B.filter_ predicate
      $ B.all_ (EDB.risk_management_account eulerDBSchema)


makeRisk' :: Maybe Text -> TxnRiskCheck -> Risk'
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

-- TODO: check if it can become pure
makeRisk :: Risk' -> Flow Risk
makeRisk risk' = if (fromMaybe T.empty (getField @"provider" risk')) == "ebs"
  then do
      -- TODO not sure is this reliable enough? how port it?
      -- completeResponseJson <- xml2Json (trc ^. _completeResponse)
      -- outputObjectResponseJson <- xml2Json (trc ^. _completeResponse)
      -- responseObj <- pure $ fromMaybe emptyObj (lookupJson "RMSIDResult" completeResponseJson)
      -- outputObj   <- pure $ fromMaybe emptyObj (maybe Nothing (lookupJson "Output") (lookupJson "RMSIDResult" outputObjectResponseJson))
    let r' = undefined :: Risk'
      -- TODO
      -- let r' = wrap (unwrap risk) {
      --   ebs_risk_level = NullOrUndefined $ maybe Nothing (Just <<< getString) (lookupJson "RiskLevel" responseObj) ,
      --   ebs_payment_status = (trc ^. _riskStatus),
      --   ebs_risk_percentage = NullOrUndefined $ maybe Nothing fromString (lookupJson "RiskPercentage" responseObj) ,
      --   ebs_bin_country = NullOrUndefined $ maybe Nothing (Just <<< getString) (lookupJson "Bincountry" (outputObj))
      -- }
    addRiskObjDefaultValueAsNull r'
  else
    addRiskObjDefaultValueAsNull risk'
    where getString a = "" -- TODO: if (isNotString a) then "" else a


-- Partly refactored
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




-- ----------------------------------------------------------------------------
-- function: addRiskObjDefaultValueAsNull
-- done
-- TODO check foreign fields seem to be the same in out design
-- ----------------------------------------------------------------------------

{-PS
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
-}

addRiskObjDefaultValueAsNull :: Risk' -> Flow Risk
addRiskObjDefaultValueAsNull risk' = do
  let risk = Risk
        { provider = getField @"provider" risk' -- : if (isJust $ unNullOrUndefined (risk' ^. _provider)) then just (toForeign (unNull (risk' ^. _provider) "")) else just (nullValue unit)
        , status = getField @"status" risk' -- : if (isJust $ unNullOrUndefined (risk' ^. _status)) then just (toForeign (unNull (risk' ^. _status) "")) else just (nullValue unit)
        , message = getField @"message" risk' -- : if (isJust $ unNullOrUndefined (risk' ^. _message)) then just (toForeign (unNull (risk' ^. _message) "")) else just (nullValue unit)
        , flagged = undefined :: Maybe Text -- TODO: getField @"flagged" risk' -- : if (isJust $ unNullOrUndefined (risk' ^. _flagged)) then just (toForeign (unNull (risk' ^. _flagged) false)) else just (nullValue unit)
        , recommended_action = getField @"recommended_action" risk' -- : if (isJust $ unNullOrUndefined (risk' ^. _recommended_action)) then just (toForeign (unNull (risk' ^. _recommended_action) "")) else just (nullValue unit)
        , ebs_risk_level = Nothing -- NullOrUndefined Nothing
        , ebs_payment_status = Nothing -- NullOrUndefined Nothing
        , ebs_risk_percentage = Nothing -- NullOrUndefined Nothing
        , ebs_bin_country = Nothing -- NullOrUndefined Nothing
        }

  case (fromMaybe mempty (getField @"provider" risk')) of
    "ebs" -> pure (risk
        { ebs_risk_level = getField @"ebs_risk_level" risk' -- if (isJust $ unNullOrUndefined (risk' ^. _ebs_risk_level)) then just (toForeign (unNull (risk' ^. _ebs_risk_level) "")) else just (nullValue unit)
        , ebs_payment_status = getField @"ebs_payment_status" risk' -- if (isJust $ unNullOrUndefined (risk' ^. _ebs_payment_status)) then just (toForeign (unNull (risk' ^. _ebs_payment_status) "")) else just (nullValue unit)
        , ebs_risk_percentage = undefined :: Maybe Text -- TODO: getField @"ebs_risk_percentage" risk' -- if (isJust $ unNullOrUndefined (risk' ^. _ebs_risk_percentage)) then just (toForeign (unNull (risk' ^. _ebs_risk_percentage) 0)) else just (nullValue unit)
        , ebs_bin_country = getField @"ebs_bin_country" risk' -- if (isJust $ unNullOrUndefined (risk' ^. _ebs_bin_country)) then just (toForeign (unNull (risk' ^. _ebs_bin_country) "")) else just (nullValue unit)
        } :: Risk)
    _ -> return risk


-- ----------------------------------------------------------------------------
-- function: addPaymentMethodInfo
-- done
-- ----------------------------------------------------------------------------

{-PS
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
-}

addPaymentMethodInfo :: Bool -> TxnDetail -> OrderStatusResponse -> Flow OrderStatusResponse
addPaymentMethodInfo sendCardIsin txn ordStatus = do
  let txnId = fromMaybe T.empty $ getField @"id" txn

  txnCard <- withDB eulerDB $ do
    let predicate TxnCardInfo {txnDetailId} = txnDetailId ==. B.just_ (B.val_ txnId)
    findRow
      $ B.select
      $ B.limit_ 1
      $ B.filter_ predicate
      $ B.all_ (EDB.txn_card_info eulerDBSchema)

  -- let enableSendingCardIsin = fromMaybe False (getField @"enableSendingCardIsin" mAccnt)
  case txnCard of
    Just card -> do
      cardBrandMaybe <- getCardBrandFromIsin (fromMaybe "" $ getField @"cardIsin" card)
      orderStatus  <- updatePaymentMethodAndType txn card ordStatus
      let orderStatus' =
            addCardInfo txn card sendCardIsin cardBrandMaybe
              $ addAuthType card
              $ addEmi txn orderStatus
      addSecondFactorResponseAndTxnFlowInfo txn card orderStatus'
    Nothing -> pure ordStatus



-- refactoring

loadTxnCardInfo :: Text -> Flow (Maybe TxnCardInfo)
loadTxnCardInfo txnId =
  withDB eulerDB $ do
    let predicate TxnCardInfo {txnDetailId} = txnDetailId ==. B.just_ (B.val_ txnId)
    findRow
      $ B.select
      $ B.limit_ 1
      $ B.filter_ predicate
      $ B.all_ (EDB.txn_card_info eulerDBSchema)



-- ----------------------------------------------------------------------------
-- function: addSecondFactorResponseAndTxnFlowInfo
-- TODO update
-- ----------------------------------------------------------------------------

{-PS
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
-}

addSecondFactorResponseAndTxnFlowInfo :: TxnDetail -> TxnCardInfo -> OrderStatusResponse -> Flow OrderStatusResponse
addSecondFactorResponseAndTxnFlowInfo txn card orderStatus = do
  if (fromMaybe T.empty $ getField @"authType" card) == "VIES" then do
    txnDetId <- whenNothing (getField @"id" txn) (throwException err500)
    maybeSf <- findMaybeSecondFactorByTxnDetailId txnDetId
    case maybeSf of
      Just sf -> do
        (authReqParams :: ViesGatewayAuthReqParams) <- undefined :: Flow ViesGatewayAuthReqParams
        -- TODO:
        --   parseAndDecodeJson (fromMaybe "{}" $ getField @"gatewayAuthReqParams" sf) "INTERNAL_SERVER_ERROR" "INTERNAL_SERVER_ERROR"
        -- let orderStatus' = orderStatus{ txn_flow_info = just $ mkTxnFlowInfo authReqParams }
        -- maybeSfr <- findMaybeSFRBySfId (sf .^. _id)
        -- case maybeSfr of
        --   Just sfr -> do
        --     let newSf = mkMerchantSecondFactorResponse sfr
        --     pure $ orderStatus' # _second_factor_response .~ (just newSf)
        --   _ -> do
        --     pure $ orderStatus' -- SFR not available.
        pure orderStatus
      Nothing ->  pure orderStatus -- NO Sf present
    else pure orderStatus --NON VIES Txn

-- -----------------------------------------------------------------------------
-- Function: findMaybeSFRBySfId
-- done
-- added from EC.SecondFactorResponse
-- -----------------------------------------------------------------------------

{-
findMaybeSFRBySfId ::forall st rt e. Newtype st (TState e) => String -> BackendFlow st _ (Maybe SecondFactorResponse)
findMaybeSFRBySfId second_factor_id =
  DB.findOne ecDB (where_ := WHERE ["second_factor_id" /\ String second_factor_id] :: WHERE SecondFactorResponse)
-}

findMaybeSFRBySfId :: Text -> Flow (Maybe SecondFactorResponse)
findMaybeSFRBySfId second_factor_id = withDB eulerDB $ do
  let predicate SecondFactorResponse{secondFactorId} =
        secondFactorId ==. B.just_ (B.val_ second_factor_id)
  findRow
    $ B.select
    $ B.limit_ 1
    $ B.filter_ predicate
    $ B.all_ (EDB.second_factor_response eulerDBSchema)

-- -----------------------------------------------------------------------------
-- Function: findMaybeSecondFactorByTxnDetailId
-- done
-- added from EC.SecondFactor
-- -----------------------------------------------------------------------------

{-
findMaybeSecondFactorByTxnDetailId ::forall st rt e. Newtype st (TState e) => String -> BackendFlow st _ (Maybe SecondFactor)
findMaybeSecondFactorByTxnDetailId txnDetail_id =
  DB.findOne ecDB (where_ := WHERE ["txn_detail_id" /\ String txnDetail_id] :: WHERE SecondFactor)
-}

findMaybeSecondFactorByTxnDetailId :: Text -> Flow (Maybe SecondFactor)
findMaybeSecondFactorByTxnDetailId txnDetail_id = withDB eulerDB $ do
  let predicate SecondFactor{txnDetailId} =
        txnDetailId ==. B.just_ (B.val_ txnDetail_id)
  findRow
    $ B.select
    $ B.limit_ 1
    $ B.filter_ predicate
    $ B.all_ (EDB.second_factor eulerDBSchema)


-- ----------------------------------------------------------------------------
-- function: mkTxnFlowInfo
-- done
-- ----------------------------------------------------------------------------

{-PS
mkTxnFlowInfo :: ViesGatewayAuthReqParams -> TxnFlowInfo
mkTxnFlowInfo params =  TxnFlowInfo
  {  flow_type : maybe null encode $ unNullOrUndefined $ params ^. _flow
  ,  status : maybe null toForeign $ unNullOrUndefined $ params ^. _flowStatus
  ,  error_code : maybe null toForeign $ unNullOrUndefined $ params ^. _errorCode
  ,  error_message : maybe null toForeign $ unNullOrUndefined $ params ^. _errorMessage
  }
-}

mkTxnFlowInfo :: ViesGatewayAuthReqParams -> TxnFlowInfo
mkTxnFlowInfo params = TxnFlowInfo
  {  flow_type = maybe T.empty show $ getField @"flow" params
  ,  status = fromMaybe T.empty $ getField @"flowStatus" params
  ,  error_code = fromMaybe T.empty $ getField @"errorCode" params
  ,  error_message = fromMaybe T.empty $ getField @"errorMessage" params
  }


-- ----------------------------------------------------------------------------
-- function: mkMerchantSecondFactorResponse
-- done
-- ----------------------------------------------------------------------------

{-PS
mkMerchantSecondFactorResponse :: SecondFactorResponse -> MerchantSecondFactorResponse
mkMerchantSecondFactorResponse sfr = MerchantSecondFactorResponse
  {  cavv : maybe null toForeign $ unNullOrUndefined $ sfr ^. _cavv
  ,  eci : sfr ^. _eci
  ,  xid : sfr ^. _xid
  ,  pares_status : sfr ^. _status
  }
-}

mkMerchantSecondFactorResponse :: SecondFactorResponse -> MerchantSecondFactorResponse
mkMerchantSecondFactorResponse sfr = MerchantSecondFactorResponse
  { cavv = fromMaybe T.empty $ getField @"cavv" sfr
  , eci = getField @"eci" sfr
  , xid = getField @"xid" sfr
  , pares_status = getField @"status" sfr
  }


-- ----------------------------------------------------------------------------
-- function: addAuthType
-- done
-- ----------------------------------------------------------------------------

{-PS
addAuthType :: TxnCardInfo -> OrderStatusResponse -> OrderStatusResponse
addAuthType card ordStatus = wrap $ (unwrap ordStatus) { auth_type = just $ unNull (card ^. _authType) "" }
-}

addAuthType :: TxnCardInfo -> OrderStatusResponse -> OrderStatusResponse
addAuthType card ordStatus =
  ordStatus{auth_type = whenNothing (getField @"authType" card) (Just "")}


-- ----------------------------------------------------------------------------
-- function: addEmi
-- done
-- ----------------------------------------------------------------------------

{-PS
addEmi :: TxnDetail -> OrderStatusResponse -> OrderStatusResponse
addEmi txn ordStatus =
  if(isTrue (txn ^. _isEmi))
  then wrap $ (unwrap ordStatus) { emi_tenure = txn ^. _emiTenure, emi_bank = txn ^. _emiBank }
  else ordStatus
-}

addEmi :: TxnDetail -> OrderStatusResponse -> OrderStatusResponse
addEmi txn ordStatus =
  if isTrueMaybe (getField @"isEmi" txn)
  then ordStatus{emi_tenure = getField @"emiTenure" txn, emi_bank = getField @"emiBank" txn}
  else ordStatus



-- ----------------------------------------------------------------------------
-- function: addCardInfo
-- done
-- ----------------------------------------------------------------------------

{-PS
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
-}

addCardInfo :: TxnDetail -> TxnCardInfo -> Bool -> Maybe Text -> OrderStatusResponse -> OrderStatusResponse
addCardInfo txnDetail txnCardInfo shouldSendCardIsin cardBrandMaybe ordStatus =
  if isBlankMaybe (getField @"cardIsin" txnCardInfo) then
    let payment_method' = if isJust cardBrandMaybe then cardBrandMaybe else Just "UNKNOWN"
        cardDetails = Just $ getCardDetails txnCardInfo txnDetail shouldSendCardIsin
    in ordStatus
        { payment_method = payment_method'
        , payment_method_type = Just "CARD"
        , card = cardDetails
        }
  else ordStatus


-- ----------------------------------------------------------------------------
-- function: getCardDetails
-- done
-- ----------------------------------------------------------------------------

{-PS
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
-}

getCardDetails :: TxnCardInfo -> TxnDetail -> Bool -> Card
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

-- ----------------------------------------------------------------------------
-- function: getPayerVpa
-- TODO update
-- ----------------------------------------------------------------------------

{-PS
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
-}

getPayerVpa :: TxnDetail -> TxnCardInfo -> Flow (Maybe Text)
getPayerVpa txn txnCardInfo = do
  if (fromMaybe T.empty (getField @"paymentMethod" txnCardInfo) == "GOOGLEPAY") then do
      respId <- whenNothing (getField @"successResponseId" txn)  (throwException err500)

      mPaymentGatewayResp <- withDB eulerDB $ do
        let predicate PaymentGatewayResponse {id} =
              id ==. B.just_ (B.val_ $ show respId)
        findRow
          $ B.select
          $ B.limit_ 1
          $ B.filter_ predicate
          $ B.all_ (EDB.payment_gateway_response eulerDBSchema)

      case mPaymentGatewayResp of
        Just paymentGateway -> do
          pure Nothing
          -- TODO:
          -- pgResponse  <- O.getResponseXml (fromMaybe T.empty $ getField @"responseXml" paymentGateway)
          -- payervpa <- lookupRespXml' pgResponse "payerVpa" ""
          -- case payervpa of
          --   "" -> pure Nothing
          --   _ -> pure $ Just payervpa
        Nothing -> pure Nothing
    else pure Nothing

-- ----------------------------------------------------------------------------
-- function: updatePaymentMethodAndType
-- TODO update
-- ----------------------------------------------------------------------------

{-PS
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
-}

updatePaymentMethodAndType :: TxnDetail -> TxnCardInfo -> OrderStatusResponse -> Flow OrderStatusResponse
updatePaymentMethodAndType txn card ordStatus = do
  case (getField @"cardType" card) of
    Just "NB" ->
      pure (ordStatus
        { payment_method = whenNothing (getField @"cardIssuerBankName"card) (Just T.empty)
        , payment_method_type = Just "NB"
        } :: OrderStatusResponse)
    Just "WALLET" -> do
      payerVpa <- getPayerVpa txn card
      pure (ordStatus
        { payment_method = whenNothing (getField @"cardIssuerBankName"card) (Just T.empty)
        , payment_method_type = Just "WALLET"
        , payer_vpa = payerVpa
        } :: OrderStatusResponse)

    Just "UPI" -> do
      let ord  = setField @"payment_method" (Just "UPI") ordStatus
          ordS = setField @"payment_method_type" (Just "UPI") ord
          paymentSource = if (fromMaybe "null" $ getField @"paymentSource" card) == "null"
            then Just "" -- just $ nullValue unit
            else whenNothing (getField @"paymentSource" card) (Just "null")
          sourceObj = fromMaybe "" $ getField @"sourceObject" txn
      if sourceObj == "UPI_COLLECT" || sourceObj == "upi_collect" then pure (setField @"payer_vpa" paymentSource ordS)
        else addPayerVpaToResponse txn ordS paymentSource

    Just "PAYLATER" ->
      pure (ordStatus
        { payment_method = Just "JUSPAY"
        , payment_method_type =Just "PAYLATER"
        } :: OrderStatusResponse)
    Just "CARD" ->
      pure (ordStatus
        { payment_method = whenNothing (getField @"cardIssuerBankName"card) (Just T.empty)
        , payment_method_type = Just "CARD"
        } :: OrderStatusResponse)
    Just "REWARD" ->
      pure (ordStatus
        { payment_method = whenNothing (getField @"cardIssuerBankName"card) (Just T.empty)
        , payment_method_type = Just "REWARD"
        } :: OrderStatusResponse)
    Just "ATM_CARD" ->
      pure (ordStatus
        { payment_method = whenNothing (getField @"cardIssuerBankName"card) (Just T.empty)
        , payment_method_type = Just "CARD"
        } :: OrderStatusResponse)
    Just _ -> checkPaymentMethodType card ordStatus
    Nothing -> checkPaymentMethodType card ordStatus

  where
    checkPaymentMethodType card' ordStatus' = case (getField @"paymentMethodType" card') of
      Just Mandate.CASH -> pure (ordStatus'
        { payment_method = whenNothing (getField @"paymentMethod" card') (Just T.empty)
        , payment_method_type = Just "CASH"
        } :: OrderStatusResponse)
      Just CONSUMER_FINANCE ->
          pure (ordStatus'
            { payment_method = whenNothing (getField @"paymentMethod" card') (Just T.empty)
            , payment_method_type = Just "CONSUMER_FINANCE"
            } :: OrderStatusResponse)
      _ -> pure ordStatus'


-- ----------------------------------------------------------------------------
-- function: addRefundDetails
-- done
-- refactored
-- ----------------------------------------------------------------------------

{-PS
addRefundDetails ::forall st rt e. Newtype st (TState e) => TxnDetail -> OrderStatusResponse -> BackendFlow st _ OrderStatusResponse
addRefundDetails (TxnDetail txn) ordStatus = do
  refunds <- DB.findAll ecDB ( order := [["dateCreated" , "ASC"]]
     <> where_ := WHERE ["txn_detail_id" /\ String  (nullOrUndefinedToAny (txn.id) "")] :: WHERE Refund)
  case (length refunds) of
    -- No Refunds
    0 -> pure ordStatus
    -- Has Refunds
    _ -> pure $ ordStatus # _refunds .~ (just $ mapRefund <$> refunds)
-}

-- Use refactored refundDetails

addRefundDetails :: TxnDetail -> OrderStatusResponse -> Flow OrderStatusResponse
addRefundDetails txn ordStatus = undefined :: Flow OrderStatusResponse

-- TODO: Use refactored refundDetails

--   --refunds <- DB.findAll ecDB ( order := [["dateCreated" , "ASC"]]
--   --   <> where_ := WHERE ["txn_detail_id" /\ String  (nullOrUndefinedToAny (txn.id) "")] :: WHERE Refund)
--   case (getField @"id" txn) of
--     Just txnId -> do
--       refunds <- withDB eulerDB $ do
--         let predicate Refund {txnDetailId} = txnDetailId ==. B.just_ (B.val_ txnId)
--         findRows
--           $ B.select
--           $ B.filter_ predicate
--           $ B.all_ (EDB.refund eulerDBSchema)

--       case refunds of
--         [] -> pure ordStatus
--         _  -> pure $ setField @"refunds" ( Just $ Euler.Product.OLTP.Order.OrderStatus.mapRefund <$> refunds) ordStatus
--     _ -> pure ordStatus

-- ----------------------------------------------------------------------------
-- Refactored

-- addRefundDetails2 txnId ordStatus = do
--   rs <- refundDetails txnId
--   setField @"refunds" (maybeList rs) ordStatus

refundDetails :: TxnDetailId -> Flow [Refund']
refundDetails txnId = do
  l <- findRefunds txnId
  pure $ map AO.mapRefund l


-- ----------------------------------------------------------------------------
-- function: mapRefund
-- done
-- refactored
-- from src/Types/Communication/OLTP/OrderStatus.purs
-- ----------------------------------------------------------------------------

{-PS
mapRefund :: Refund -> Refund'
mapRefund refund =
  let refundObj = unwrap refund
  in Refund'
       {  id : just (if (isPresent refundObj.referenceId) then toForeign (unNull refundObj.referenceId "") else nullValue unit)
       ,  amount : refundObj.amount
       ,  unique_request_id : just $ unNull refundObj.uniqueRequestId ""
       ,  ref : just (if (isPresent refundObj.epgTxnId) then toForeign (unNull refundObj.epgTxnId "") else nullValue unit)
       ,  created : (show refundObj.dateCreated)
       ,  status : refundObj.status --"" TODO // transform this
       ,  error_message : just $ unNull refundObj.errorMessage ""
       ,  sent_to_gateway : getStatus refundObj
       ,  arn : refundObj.refundArn
       ,  initiated_by : refundObj.initiatedBy
       ,  internal_reference_id : getRefId refundObj
       ,  refund_source : just $ (if (isPresent refundObj.refundSource) then toForeign (unNull refundObj.refundSource "") else nullValue unit)
       ,  refund_type : refundObj.refundType
       }

   where getStatus refundObj = just $
           ((refundObj.status) == (Refund.SUCCESS))
           || refundObj.processed
           || (isTrue refundObj.sentToGateway)
         getRefId refundObj =
           if refundObj.gateway == "HDFC" && (isTrue refundObj.sentToGateway)
           then refundObj.internalReferenceId
           else NullOrUndefined Nothing
-}

-- Use refactored mapRefund at Euler.API.Order

-- mapRefund :: Refund -> Refund'
-- mapRefund refund = Refund'
--   {  id = Just $ fromMaybe mempty (getField @"referenceId" refund)
--   ,  amount = getField @"amount" refund
--   ,  unique_request_id = Just $ fromMaybe mempty (getField @"uniqueRequestId" refund)
--   ,  ref = Just $ fromMaybe mempty (getField @"epgTxnId" refund)
--   ,  created = show $ getField @"dateCreated" refund -- TODO date format
--   ,  status = getField @"status" refund --"" ORIG TODO // transform this
--   ,  error_message = Just $ fromMaybe mempty (getField @"errorMessage" refund)
--   ,  sent_to_gateway = getStatus refund
--   ,  arn = getField @"refundArn" refund
--   ,  initiated_by = getField @"initiatedBy" refund
--   ,  internal_reference_id = getRefId refund
--   ,  refund_source = Just $ fromMaybe mempty (getField @"refundSource" refund)
--   ,  refund_type = Just $ fromMaybe mempty (getField @"refundType" refund)
--   }

--   where
--     getStatus refundObj = Just $ (status == SUCCESS || processed || sentToGateway)
--       where
--         status = getField @"status" refund
--         processed = getField @"processed" refund
--         sentToGateway = fromMaybe False (getField @"sentToGateway" refund)


--     getRefId refundObj
--       | (gateway == "HDFC" && sentToGateway) = internalReferenceId
--       | otherwise = Nothing
--       where
--         gateway = getField @"gateway" refund
--         sentToGateway = fromMaybe False (getField @"sentToGateway" refund)
--         internalReferenceId = getField @"internalReferenceId" refund

-- ----------------------------------------------------------------------------
-- function: addChargeBacks
-- done
-- refactored
-- ----------------------------------------------------------------------------

{-PS
addChargeBacks ::forall st rt e. Newtype st (TState e) => TxnDetail -> OrderStatusResponse -> BackendFlow st _ OrderStatusResponse
addChargeBacks (TxnDetail txnDetail) orderStatus = do
  txn <- pure $ mapTxnDetail (TxnDetail txnDetail)
  chargeBacks <- DB.findAll ecDB (where_ := WHERE ["txn_detail_id" /\ String (nullOrUndefinedToAny (txnDetail.id) "")] :: WHERE Chargeback)
  case length chargeBacks of
    0 -> pure orderStatus
    _ -> pure $ orderStatus # _chargebacks .~ (just $ mapChargeback txn <$> chargeBacks)
-}

-- Use refactored
-- addChargeBacks2 txnId ordStatus = do
--   rs <- chargebackDetails txnId
--   setField @"chargebacks" (maybeList rs) ordStatus

addChargeBacks :: TxnDetail -> OrderStatusResponse -> Flow OrderStatusResponse
addChargeBacks txnDetail orderStatus = undefined :: Flow OrderStatusResponse

  -- case (getField @"id" txnDetail) of
  --   Nothing -> pure orderStatus
  --   Just detailId  -> do
  --     --chargeBacks <- DB.findAll ecDB (where_ := WHERE ["txn_detail_id" /\ String (nullOrUndefinedToAny (txnDetail.id) "")] :: WHERE Chargeback)
  --     chargeBacks <- withDB eulerDB $ do
  --       let predicate Chargeback {txnDetailId}
  --             = txnDetailId ==. B.just_ (B.val_ detailId)
  --       findRows
  --         $ B.select
  --         $ B.filter_ predicate
  --         $ B.all_ (chargeback eulerDBSchema)

  --     case chargeBacks of
  --       [] -> pure orderStatus
  --       _  -> pure $ setField @"chargebacks" (Just $ mapChargeback txn <$> chargeBacks) orderStatus
  --           where
  --             txn = mapTxnDetail txnDetail

-- Refactored

-- addChargeBacks2 txnId ordStatus = do
--   rs <- chargebackDetails txnId
--   setField @"chargebacks" (maybeList rs) ordStatus

chargebackDetails :: TxnDetailId -> TxnDetail' -> Flow [Chargeback']
chargebackDetails txnId txn = do
  chargebacks <- findChargebacks txnId
  pure $ map (AO.mapChargeback txn) chargebacks

-- ----------------------------------------------------------------------------
-- function: mapChargeback
-- done
-- refactored
-- from src/Types/Storage/EC/Chargeback.purs
-- ----------------------------------------------------------------------------

{-PS
mapChargeback :: TxnDetail' -> Chargeback -> Chargeback'
mapChargeback txn chargeback =
   let chargebackObj = unwrap chargeback
   in Chargeback'
          {  id : just chargebackObj.id
          ,  amount : just chargebackObj.amount
          ,  object_reference_id : just chargebackObj.objectReferenceId
          ,  txn : just txn
          ,  date_resolved : chargebackObj.dateResolved
          ,  date_created : just chargebackObj.dateCreated
          ,  last_updated : just chargebackObj.lastUpdated
          ,  object : just "chargeback"
          ,  dispute_status : just chargebackObj.disputeStatus
          }
-}

-- Use refactored mapRefund at Euler.API.Order

-- mapChargeback :: TxnDetail' -> Chargeback -> Chargeback'
-- mapChargeback txn chargeback =
--   Chargeback'
--   {  id = Just $ getField @"id" chargeback
--   ,  amount = Just $ getField @"amount" chargeback
--   ,  object_reference_id = Just $ getField @"objectReferenceId" chargeback
--   ,  txn = Just txn
--   ,  date_resolved = getField @"dateResolved" chargeback
--   ,  date_created = Just $ getField @"dateCreated" chargeback
--   ,  last_updated = Just $ getField @"lastUpdated" chargeback
--   ,  object = Just "chargeback"
--   ,  dispute_status = Just $ getField @"disputeStatus" chargeback
--   }

-- ----------------------------------------------------------------------------
-- function: lookupPgRespXml
-- TODO port
-- ----------------------------------------------------------------------------

{-PS
lookupPgRespXml :: String -> String -> String -> BackendFlow _ _ String
lookupPgRespXml respxml key defaultValue = do
  respxmlVal <- O.getResponseXml respxml
  let keyVal = find (\val -> (key == (fromMaybe "" (val.string !! 0)))) respxmlVal
  pure $ case keyVal of
    Just val -> if isNotString $ fromMaybe defaultValue (val.string !! 1) then defaultValue else fromMaybe defaultValue (val.string !! 1)
    Nothing -> defaultValue
-}


-- ----------------------------------------------------------------------------
-- function: lookupRespXml
-- TODO port
-- ----------------------------------------------------------------------------

{-PS
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
-}


-- ----------------------------------------------------------------------------
-- function: lookupRespXml'
-- TODO port
-- ----------------------------------------------------------------------------

{-PS
lookupRespXml' :: forall st r e rt. Newtype st (TState e) =>(Array _) -> String -> String -> BackendFlow st rt String
lookupRespXml' xml str1 defaultValue = do
  str1 <- pure $ find (\val -> (str1 == (fromMaybe "" (val.string !! 0)))) xml
  case str1 of
    Just val -> if isNotString $ fromMaybe defaultValue (val.string !! 1) then pure $ defaultValue else pure $ fromMaybe defaultValue (val.string !! 1)
    Nothing -> pure $ defaultValue
-}


-- ----------------------------------------------------------------------------
-- function: lookupRespXmlVal
-- TODO port
-- ----------------------------------------------------------------------------

{-PS
lookupRespXmlVal :: String-> String -> String -> BackendFlow _ _ String
lookupRespXmlVal respXml str1 defaultValue = do
  xml <- O.getResponseXml respXml :: (BackendFlow _ _ (Array {string :: Array _}))
  let upVal = find (\val -> (str1 == (fromMaybe "" ((val.string) !! 0)))) xml
  pure $ case upVal of
    Just val  -> if isNotString $ fromMaybe defaultValue (val.string !! 1) then defaultValue else fromMaybe defaultValue (val.string !! 1)
    Nothing -> defaultValue
-}


-- ----------------------------------------------------------------------------
-- function: tempLookup
-- TODO port
-- ----------------------------------------------------------------------------

{-PS
tempLookup :: forall st r e rt. Newtype st (TState e) => String -> String -> String -> BackendFlow st rt String
tempLookup xml str1 defaultValue = do
  x <- O.getResponseXml xml
  str1 <- pure $ find (\val -> str1 == val.string) x
  case str1 of
    Just val -> do
      obj <- pure $ getRecordValues val
      pure $ fromMaybe defaultValue (obj !! 1)
    Nothing -> pure defaultValue
-}


-- ----------------------------------------------------------------------------
-- function: hierarchyObjectLookup
-- TODO port
-- ----------------------------------------------------------------------------

{-PS
hierarchyObjectLookup :: forall st r e rt. Newtype st (TState e) => String -> String -> String -> BackendFlow st rt String
hierarchyObjectLookup xml key1 key2 = do
  xmlVal <- O.getResponseXml xml
  val    <- pure $ find (\val -> (key1 == val.string)) xmlVal
  case val of
    Just v -> do
      let vA = (v."org.codehaus.groovy.grails.web.json.JSONObject".myHashMap.entry) -- ::  Array { string :: Array String | t594}
      lookupRespXml' vA key2 ""
    Nothing -> pure ""
-}

hierarchyObjectLookup :: Text -> Text -> Text -> Flow Text
hierarchyObjectLookup xml key1 key2 = undefined

-- ----------------------------------------------------------------------------
-- function: hierarchyObjectLookup
-- TODO port
-- from src/Types/Storage/EC/PaymentGatewayResponse.purs
-- ----------------------------------------------------------------------------

{-PS
getResponseXml :: forall a b. String -> BackendFlow _ _ (Array b)
getResponseXml xmlVal = do
	json <- xml2Json xmlVal
	json' <- xml2Json xmlVal
	pure $ case lookupJson "linked-hash-map" json of
						Just linkedHash -> entryLookup linkedHash
						Nothing -> case lookupJson "org.codehaus.groovy.grails.web.json.JSONObject" json' of
													Just orgCode -> case lookupJson "myHashMap" orgCode of
																						Just hashMap -> entryLookup hashMap
																						Nothing -> jsonValues emptyObj
													Nothing -> case lookupJson "map" json of
																				Just linkedHash -> entryLookup linkedHash
																				Nothing -> jsonValues emptyObj

  where entryLookup entry = case (lookupJson "entry" entry) of
								Just entry -> if (isArray entry) then (jsonValues entry) else (jsonValues emptyObj)
								Nothing -> jsonValues emptyObj
-}

-- ----------------------------------------------------------------------------
-- function: addGatewayResponse
-- done
-- ----------------------------------------------------------------------------

{-PS
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
-}

addGatewayResponse :: TxnDetail -> Bool -> OrderStatusResponse -> Flow OrderStatusResponse
addGatewayResponse txn shouldSendFullGatewayResponse orderStatus = do

  -- TODO OS: We need to have this clarified with Sushobhith
  -- they introduces PGRV1 in early 2020

  -- paymentGatewayResp <- sequence $ findMaybePGRById <$> unNullOrUndefined (txn ^. _successResponseId)

  -- findMaybePGRById :: forall st rt e. Newtype st (TState e) => String -> BackendFlow st _ (Maybe PaymentGatewayResponseV1)
  -- findMaybePGRById pgrRefId = DB.findOne ecDB $ where_ := WHERE ["id" /\ String pgrRefId]

  respId <- whenNothing (getField @"successResponseId" txn)  (throwException err500)

  mPaymentGatewayResp <- withDB eulerDB $ do
    let predicate PaymentGatewayResponse {id} =
          id ==. B.just_ (B.val_ $ show respId)
    findRow
      $ B.select
      $ B.limit_ 1
      $ B.filter_ predicate
      $ B.all_ (EDB.payment_gateway_response eulerDBSchema)

  case mPaymentGatewayResp of
    Just pgr -> do
      -- Зависимость
      ordStatus <- getPaymentGatewayResponse txn pgr orderStatus

      -- unNullOrUndefined (ordStatus ^._payment_gateway_response'))
      -- TODO do we need unNullOrUndefined here?
      let mPgr' = getField @"payment_gateway_response'" ordStatus
      case mPgr' of
        Just pgr' -> do
          gatewayResponse <- getGatewayResponseInJson pgr shouldSendFullGatewayResponse
          let r = makeMerchantPaymentGatewayResponse gatewayResponse pgr'

          pure (orderStatus
            { payment_gateway_response' = Nothing
            , payment_gateway_response = Just r
            } :: OrderStatusResponse)

        Nothing -> pure orderStatus
    Nothing -> pure orderStatus


loadPaymentGatewayResponse :: TxnDetail -> Bool -> Flow (Maybe PaymentGatewayResponse)
loadPaymentGatewayResponse txn sendFullGatewayResponse = do

  -- TODO OS: We need to have this clarified with Sushobhith
  -- they introduces PGRV1 in early 2020

  -- paymentGatewayResp <- sequence $ findMaybePGRById <$> unNullOrUndefined (txn ^. _successResponseId)

  -- findMaybePGRById :: forall st rt e. Newtype st (TState e) => String -> BackendFlow st _ (Maybe PaymentGatewayResponseV1)
  -- findMaybePGRById pgrRefId = DB.findOne ecDB $ where_ := WHERE ["id" /\ String pgrRefId]

  respId <- whenNothing (getField @"successResponseId" txn)  (throwException err500)

  withDB eulerDB $ do
    let predicate PaymentGatewayResponse {id} =
          id ==. B.just_ (B.val_ $ show respId)
    findRow
      $ B.select
      $ B.limit_ 1
      $ B.filter_ predicate
      $ B.all_ (EDB.payment_gateway_response eulerDBSchema)


makeMerchantPaymentGatewayResponse :: Maybe Text -> MerchantPaymentGatewayResponse' -> MerchantPaymentGatewayResponse
makeMerchantPaymentGatewayResponse gatewayResponse pgr' = MerchantPaymentGatewayResponse
  { resp_code = Just $ checkNull $ getField @"resp_code" pgr' -- : just $ checkNull pgr'.resp_code nullVal
  , rrn = Just $ checkNull $ getField @"resp_code" pgr' -- : just $ checkNull pgr'.rrn nullVal
  , created = Just $ checkNull $ getField @"resp_code" pgr' -- : just $ checkNull pgr'.created nullVal
  , epg_txn_id = Just $ checkNull $ getField @"resp_code" pgr' -- : just $ checkNull pgr'.epg_txn_id nullVal
  , resp_message = Just $ checkNull $ getField @"resp_code" pgr' -- : just $ checkNull pgr'.resp_message nullVal
  , auth_id_code = Just $ checkNull $ getField @"resp_code" pgr' -- : just $ checkNull pgr'.auth_id_code nullVal
  , txn_id = Just $ checkNull $ getField @"resp_code" pgr' -- : just $ checkNull pgr'.txn_id nullVal
  , offer = getField @"offer" pgr' -- : pgr'.offer
  , offer_type = getField @"offer_type" pgr' -- : pgr'.offer_type
  , offer_availed = getField @"offer_availed" pgr' -- : pgr'.offer_availed
  , discount_amount = getField @"discount_amount" pgr' -- : pgr'.discount_amount
  , offer_failure_reason = getField @"offer_failure_reason" pgr' -- : pgr'.offer_failure_reason
  , gateway_response = gatewayResponse -- TODO Text/ByteString?
  }
    -- TODO move to common utils or sth to that effect
  where
    checkNull :: Maybe Text -> Text
    checkNull Nothing = mempty
    checkNull (Just resp)
      | resp == "null" = mempty  --  (unNull resp "") == "null" = nullVal
      | otherwise      = resp -- TODO original: toForeign $ (unNull resp "")

-- ----------------------------------------------------------------------------
-- function: getGatewayResponseInJson
-- TODO port
-- ----------------------------------------------------------------------------

{-PS
getGatewayResponseInJson ::forall st rt e. Newtype st (TState e) =>
                        PaymentGatewayResponse -> Boolean -> BackendFlow st _ (NullOrUndefined Foreign)
getGatewayResponseInJson paymentGatewayResponse shouldSendFullGatewayResponse =
  if shouldSendFullGatewayResponse then do
    jsonPgr <- createJsonFromPGRXmlResponse <$> (O.getResponseXml (paymentGatewayResponse ^.. _responseXml $ ""))
    pure $ just $ jsonPgr
    else pure nothing
-}

getGatewayResponseInJson ::
--forall st rt e. Newtype st (TState e) =>
  PaymentGatewayResponse
  -> Bool
  -> Flow (Maybe Text) -- TODO Text/ByteString?
getGatewayResponseInJson paymentGatewayResponse shouldSendFullGatewayResponse =
  -- if shouldSendFullGatewayResponse then do
  --   jsonPgr <- createJsonFromPGRXmlResponse <$> (O.getResponseXml (paymentGatewayResponse ^.. _responseXml $ ""))
  --   pure $ just $ jsonPgr
  --   else pure nothing
  pure Nothing

-- ----------------------------------------------------------------------------
-- function: casematch
-- TODO port
-- ----------------------------------------------------------------------------

{-PS
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
-}

-- ----------------------------------------------------------------------------
-- function: getPaymentGatewayResponse
-- TODO port
-- ----------------------------------------------------------------------------

{-PS
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
-}

getPaymentGatewayResponse
  :: TxnDetail
  -> PaymentGatewayResponse
  -> OrderStatusResponse
  -> Flow OrderStatusResponse
getPaymentGatewayResponse txn pgr orderStatusResp =
  return orderStatusResp


-- ----------------------------------------------------------------------------
-- function: versionSpecificTransforms
-- done
-- ----------------------------------------------------------------------------

{-PS
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
-}

versionSpecificTransforms :: Text -> OrderStatusResponse -> Flow OrderStatusResponse
versionSpecificTransforms version orderStatus@OrderStatusResponse{gateway_id} =
  pure $ transformOrderStatus (mkOrderStatusService version gatewayId) orderStatus
  where
    gatewayId = fromMaybe 0 gateway_id
--  let pgResponse = getField @"payment_gateway_response" orderStatus
--      refunds = fromMaybe [] $ getField @"refunds" orderStatus
--      gatewayId = fromMaybe 0 $ getField @"gateway_id" orderStatus
--      version = Map.lookup "version" $ unRP headers
--      apiVersion = fromMaybe "" version
--      refunds' = if (apiVersion < "2015-08-18" && apiVersion /= "") || apiVersion == ""
--        then filter (\refund -> getField @"status" refund /= Refund.FAILURE) refunds
--        else refunds
--  refundStatuses <- traverse (getRefundStatus apiVersion) refunds'
--
--  let pgResps = case pgResponse of
--        Just pgResp ->
--            let pgResponse' = if apiVersion < "2017-05-25" || apiVersion == ""
--                  then (pgResp
--                    { offer = Nothing -- :: Maybe Text)
--                    , offer_availed = Nothing -- :: Maybe Text)
--                    , offer_type = Nothing -- :: Maybe Text)
--                    , offer_failure_reason = Nothing -- :: Maybe Text)
--                    , discount_amount = Nothing -- :: Maybe Text)
--                    } :: MerchantPaymentGatewayResponse)
--                  else pgResp
--
--            in Just $ if gatewayId == gatewayIdFromGateway PAYU && ((apiVersion < "2017-10-26" && apiVersion /= "") || apiVersion == "")
--              then
--                  let pgResp = setField @"auth_id_code" (getField @"rrn" pgResponse') pgResponse'
--                  in setField @"rrn" (getField @"epg_txn_id" pgResp) pgResp
--              else pgResponse'
--        Nothing -> Nothing
--
--      orderStatus' = if length refundStatuses > 0
--        then setField @"refunds" (Just refundStatuses) orderStatus
--        else orderStatus
--
--      ordStatus = setField @"payment_gateway_response" pgResps orderStatus'
--
--      ordStatusResp = if isJust (getField @"chargebacks" ordStatus) && ((apiVersion < "2017-07-26" && apiVersion /= "") || apiVersion == "")
--        then setField @"chargebacks" Nothing ordStatus
--        else if apiVersion == ""
--          then setField @"chargebacks" Nothing ordStatus
--          else ordStatus
--
--      ordResp = if isJust (getField @"txn_detail" ordStatusResp) && ((apiVersion < "2018-07-16" && apiVersion /= "") || apiVersion == "")
--        then setField @"txn_detail" Nothing ordStatusResp
--        else if (apiVersion == "")
--          then setField @"txn_detail" Nothing ordStatusResp
--          else ordStatusResp
--  if isJust (getField @"gateway_reference_id" ordResp) && ((apiVersion < "2018-10-25" && apiVersion /= "") || apiVersion == "")
--    then pure $ setField @"gateway_reference_id" Nothing ordResp
--    else if (apiVersion == "")
--      then pure $ setField @"gateway_reference_id" Nothing ordResp
--      else pure ordResp

-- ----------------------------------------------------------------------------
-- function: getRefundStatus
-- done
-- ----------------------------------------------------------------------------

{-PS
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
-}

-- getRefundStatus :: Text -> Refund' -> Flow Refund'
-- getRefundStatus apiVersion refund = do undefined :: Flow Refund' -- TODO: refactor it, coz Refund' fields not Maybe
  -- let status = getField @"status" refund
  -- let refunds1 = if (apiVersion < "2018-09-20" && apiVersion /= "") || (apiVersion == "")
  --       then setField @"initiated_by" Nothing refund
  --       else refund
  -- let refunds2 = if (apiVersion < "2019-03-12" && apiVersion /= "") || (apiVersion == "")
  --       then refunds1{refund_type = Nothing, refund_source = Nothing}
  --       else refunds1
  -- if apiVersion >= "2015-01-09" && apiVersion /= ""
  --   then do
  --     if apiVersion < "2017-03-16" && apiVersion /= "" && status == MANUAL_REVIEW
  --       then pure (setField @"status" Refund.PENDING refunds2)
  --       else pure refunds2
  --   else pure $ setField @"status" Refund.SUCCESS refunds2

-- ----------------------------------------------------------------------------
-- function: proxyOrderStatus & co
-- TODO port
-- ----------------------------------------------------------------------------

{-PS
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
-}


-- ----------------------------------------------------------------------------
-- function: proxyOrderCreate
-- TODO port
-- ----------------------------------------------------------------------------

{-PS
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
-}


-- ----------------------------------------------------------------------------
-- Separate API! Should be in a separate file!
-- function: getTxnStatusResponse
-- done
-- ----------------------------------------------------------------------------

{-PS
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
-}

-- Original getTxnStatusResponse has `SecondFactor` that not used
getTxnStatusResponse :: TxnDetail -> Bool -> SecondFactor -> Flow TxnStatusResponse
getTxnStatusResponse txnDetail sendCardIsin sf = do
  ordStatusResponse <- addPaymentMethodInfo sendCardIsin txnDetail defaultOrderStatusResponse
                        >>= addRefundDetails txnDetail
                        >>= addGatewayResponse txnDetail False

  txnSRId <- whenNothing (getField @"txnUuid" txnDetail) (throwException err500)
  txnSRgateway <- whenNothing (getField @"gateway" txnDetail) (throwException err500)
  txnSRCreated <- whenNothing (getField @"dateCreated" txnDetail) (throwException err500)

  pure TxnStatusResponse
    { id = txnSRId
    , order_id = getField @"orderId" txnDetail
    , txn_id = getField @"txnId" txnDetail
    , status = getField @"status" txnDetail
    , gateway = txnSRgateway
    , created = txnSRCreated
    , resp_code = fromMaybe T.empty $ getField @"bankErrorCode" txnDetail
    , resp_message = fromMaybe T.empty $ getField @"bankErrorMessage" txnDetail
    , payment_info = getPaymentInfo ordStatusResponse
    , payment_gateway_response = getField @"payment_gateway_response" ordStatusResponse
    , refunds = getField @"refunds" ordStatusResponse
    , payment = Nothing
    }

-- ----------------------------------------------------------------------------
-- function: getPaymentInfo
-- done
-- ----------------------------------------------------------------------------

{-PS
getPaymentInfo :: OrderStatusResponse -> PaymentInfo
getPaymentInfo ordStatusResponse = PaymentInfo {
    payment_method_type : ordStatusResponse ^. _payment_method_type
  , payment_method      : ordStatusResponse ^. _payment_method
  , card                : ordStatusResponse ^. _card
  , auth_type           : nothing
  , authentication      : nothing
}
-}

getPaymentInfo :: OrderStatusResponse -> PaymentInfo
getPaymentInfo ordStatusResponse = PaymentInfo {
    payment_method_type = getField @"payment_method_type" ordStatusResponse -- Maybe in OrderStatusResponse
  , payment_method      = getField @"payment_method" ordStatusResponse -- Maybe in OrderStatusResponse
  , card                = getField @"card" ordStatusResponse -- Maybe in OrderStatusResponse
  , auth_type           = Nothing
  , authentication      = Nothing
}

-- ----------------------------------------------------------------------------
-- function: getTokenExpiryData
-- done
-- ----------------------------------------------------------------------------

{-PS
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
-}

getTokenExpiryData :: Flow OrderTokenExpiryData
getTokenExpiryData = do
  orderToken <- T.append "tkn_" <$> getUUID32
  currentDateWithOffset <- getCurrentDateStringWithSecOffset orderTokenExpiry
  let defaultTokenData = OrderTokenExpiryData
        { expiryInSeconds = orderTokenExpiry
        , tokenMaxUsage = orderTokenMaxUsage
        , orderToken = Just orderToken
        , currentDateWithExpiry = Just currentDateWithOffset
        }

  mServiceConfiguration <- withDB eulerDB $ do
      let predicate ServiceConfiguration {name} =
            name ==. B.val_ "ORDER_TOKEN_EXPIRY_DATA"
      findRow
        $ B.select
        $ B.limit_ 1
        $ B.filter_ predicate
        $ B.all_ (EDB.service_configuration eulerDBSchema)

  case mServiceConfiguration of
    (Just serviceConfiguration) -> do
      let value = getField @"value" serviceConfiguration
          mDecodedVal = (decode $ BSL.fromStrict $ T.encodeUtf8 value) :: Maybe OrderTokenExpiryData
      case mDecodedVal of
        Nothing -> pure defaultOrderTokenExpiryData
        Just decodedVal -> pure defaultOrderTokenExpiryData
          { expiryInSeconds = getField @"expiryInSeconds" decodedVal
          , tokenMaxUsage = getField @"tokenMaxUsage" decodedVal
          }
    Nothing -> pure defaultOrderTokenExpiryData


-- ----------------------------------------------------------------------------
-- function: getAxisUpiTxnIdFromPgr
-- TODO port
-- ----------------------------------------------------------------------------

{-PS
getAxisUpiTxnIdFromPgr ::forall st rt e. Newtype st (TState e) =>
      TxnDetail -> (Array _)  -> BackendFlow st _ String
getAxisUpiTxnIdFromPgr txn pgResponse = do
  merchantRequestId <- lookupRespXml' pgResponse "merchantRequestId" ""
  if merchantRequestId == "" then do
    transactionRef <- lookupRespXml' pgResponse "transactionRef" ""
    if transactionRef /= "" then do
      pure transactionRef else pure (txn ^. _txnId)
    else pure merchantRequestId

-}

-- ----------------------------------------------------------------------------
-- function: getAxisUpiRequestIdFromPgr
-- TODO port
-- ----------------------------------------------------------------------------

{-PS
getAxisUpiRequestIdFromPgr ::forall st rt e. Newtype st (TState e) =>
      TxnDetail -> (Array _) -> BackendFlow st _ String
getAxisUpiRequestIdFromPgr txn pgResponse = do
  gatewayTransactionId <- lookupRespXml' pgResponse "gatewayTransactionId" ""
  if gatewayTransactionId == "" then lookupRespXml' pgResponse "upiRequestId" "null"
    else pure gatewayTransactionId

-}


-- ----------------------------------------------------------------------------
-- function: createJsonFromPGRXmlResponse
-- TODO port
-- ----------------------------------------------------------------------------

{-PS
createJsonFromPGRXmlResponse :: Array String -> Foreign
createJsonFromPGRXmlResponse arrayXml = toForeign $ foldl xmlToJsonPgrAccumulater empty arrayXml
-}


-- ----------------------------------------------------------------------------
-- function: xmlToJsonPgrAccumulater
-- TODO port
-- ----------------------------------------------------------------------------

{-PS
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
-}

-- ----------------------------------------------------------------------------
-- function:
-- TODO update
-- ----------------------------------------------------------------------------

{-PS
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

-}

addPayerVpaToResponse :: TxnDetail -> OrderStatusResponse -> Maybe Text -> Flow OrderStatusResponse
addPayerVpaToResponse txnDetail ordStatusResp paymentSource = do
  let ordStatus = setField @"payer_app_name" paymentSource ordStatusResp
  respId <- whenNothing (getField @"successResponseId" txnDetail)  (throwException err500)

  mPaymentGatewayResp <- withDB eulerDB $ do
    let predicate PaymentGatewayResponse {id} =
          id ==. B.just_ (B.val_ $ show respId)
    findRow
      $ B.select
      $ B.limit_ 1
      $ B.filter_ predicate
      $ B.all_ (EDB.payment_gateway_response eulerDBSchema)

  case mPaymentGatewayResp of
    Just paymentGateway -> do
      pure ordStatus
      -- TODO:
      -- pgResponse  <- O.getResponseXml $ fromMaybe T.empty $ getField @"responseXml" paymentGateway
      -- payervpa <- case (fromMaybe T.empty $ getField @"gateway" txnDetail) of
      --               "AXIS_UPI"    -> lookupRespXml' pgResponse "payerVpa" =<< lookupRespXml' pgResponse "customerVpa" ""
      --               "HDFC_UPI"    -> lookupRespXml' pgResponse "payerVpa" ""
      --               "INDUS_UPI"   -> lookupRespXml' pgResponse "payerVpa" ""
      --               "KOTAK_UPI"   -> lookupRespXml' pgResponse "payerVpa" ""
      --               "SBI_UPI"     -> lookupRespXml' pgResponse "payerVpa" ""
      --               "ICICI_UPI"   -> lookupRespXml' pgResponse "payerVpa" ""
      --               "HSBC_UPI"    -> lookupRespXml' pgResponse "payerVpa" ""
      --               "VIJAYA_UPI"  -> lookupRespXml' pgResponse "payerVpa" ""
      --               "YESBANK_UPI" -> lookupRespXml' pgResponse "payerVpa" ""
      --               "PAYTM_UPI"   -> lookupRespXml' pgResponse "payerVpa" ""
      --               "PAYU"        -> lookupRespXml' pgResponse "field3" ""
      --               "RAZORPAY"    -> lookupRespXml' pgResponse "vpa" ""
      --               "PAYTM_V2"    -> lookupRespXml' pgResponse "VPA" ""
      --               "GOCASHFREE"  -> lookupRespXml' pgResponse "payersVPA" ""
      --               _             -> pure ""
      -- case payervpa of
      --   "" -> pure ordStatus
      --   _ -> pure $ setField @"payer_vpa" (Just payervpa) ordStatus
    Nothing -> pure ordStatus

-- OLD STUFF

-- getOrderReferenceFromDB :: Text -> Text -> Flow OrderReference -- OrderReference
-- getOrderReferenceFromDB orderId merchantId = do
--   _    <- logInfo "Get order reference from DB" $ "fetching order status from DB for merchant_id " <> merchantId <> " orderId " <> orderId
--   -- DB.findOneWithErr ecDB (where_ := WHERE ["order_id" /\ String orderId, "merchant_id" /\ String merchantId]) (orderNotFound orderId)
--   pure defaultOrderReference

-- from src/Validation/Offers/Validation.purs
-- sanitizeAmount :: Number -> Number
-- sanitizeAmount x = (fromMaybe x) <<< fromString <<< (toFixed 2) <<< fromNumber $ x
sanitizeAmount x = x
sanitizeNullAmount = fmap sanitizeAmount
