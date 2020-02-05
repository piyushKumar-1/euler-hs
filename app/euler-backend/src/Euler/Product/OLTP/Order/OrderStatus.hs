{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeApplications          #-}

module Euler.Product.OLTP.Order.OrderStatus where


import           EulerHS.Prelude hiding (id)
import qualified EulerHS.Prelude as P (id)
import qualified Prelude as P (show)

import EulerHS.Types
import EulerHS.Language
import Euler.Lens

import Data.Aeson
import Data.Generics.Product.Fields
import Servant.Server
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map.Strict as Map

import Euler.API.Order
import Euler.API.RouteParameters
import Euler.Common.Types.DefaultDate
import Euler.Common.Types.Mandate
import Euler.Common.Utils
import Euler.Common.Types.Gateway
import Euler.Storage.Types.OrderMetadataV2
import Euler.Common.Types.TxnDetail
import Euler.Common.Types.Merchant
import Euler.Common.Types.Promotion
import Euler.Product.Domain.Order (Order)
import Euler.Product.OLTP.Services.AuthenticationService (extractApiKey, getMerchantId)


import Euler.Storage.Types.Customer
import Euler.Storage.Types.Feature
import Euler.Storage.Types.Mandate
import Euler.Storage.Types.MerchantAccount
import Euler.Storage.Types.MerchantIframePreferences
import Euler.Storage.Types.MerchantKey
import Euler.Storage.Types.OrderReference
import Euler.Storage.Types.PaymentGatewayResponse
import Euler.Storage.Types.ResellerAccount
import Euler.Storage.Types.Promotions
import Euler.Storage.Types.TxnCardInfo
import Euler.Storage.Types.TxnDetail

import Euler.Storage.Types.EulerDB as EDB

import qualified Euler.Common.Types.Order as C
import qualified Euler.Common.Metric      as Metric

import Euler.Storage.DBConfig
import qualified Database.Beam as B
import qualified Database.Beam.Backend.SQL as B
import Database.Beam ((==.), (&&.), (<-.), (/=.))

-- 403 error with error message
myerr n = err403 { errBody = "Err # " <> n }

-- PS Will be removing gateways one by one from here as part of direct upi integrations.
-- used in `casematch` function
eulerUpiGateways :: [Gateway]
eulerUpiGateways = [HDFC_UPI, INDUS_UPI, KOTAK_UPI, SBI_UPI, ICICI_UPI, HSBC_UPI, VIJAYA_UPI, YESBANK_UPI, PAYTM_UPI]

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
-- function: updateState
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

-- TODO inside authentication used "x-forwarded-for" header
-- TODO can we collect all headers in Map and save them in state? before we run the flow?
processOrderStatusGET :: Text -> APIKey -> Flow OrderStatusResponse 
processOrderStatusGET orderId apiKey = do
  -- if merchantAccount don't exists - throw access denied exception
  (merchantAccount, isAuthenticated) <- authenticateWithAPIKey apiKey
 -- _ <- updateState (merchantAccount .^. _merchantId) orderId
  --instead of updateState we can save parameters with options (if they not shared over all api handlers)
  _ <- setOption FlowStateOption $ FlowState (getField @"merchantId" merchantAccount) orderId
  -- set metrics/log info
  -- * field "merchantId" is mandatory, so we can define it as Text instead of Maybe Text in MerchantAccount data type
-- * if unauthenticated calls disabled by merchant - throw access forbidden
  _  <- unless isAuthenticated $ rejectIfUnauthenticatedCallDisabled merchantAccount
-- * use unless/when instead of skipIfB/execIfB
--rejectIfUnauthenticatedCallDisabled merchantAccount `skipIfB` isAuthenticated
  response <- getOrderStatusWithoutAuth defaultOrderStatusRequest orderId {-routeParams-} merchantAccount isAuthenticated Nothing Nothing
 -- _ <- log "Process Order Status Response" $ response
  pure response

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
  :: OrderStatusRequest 
  -> Text {-RouteParameters-} 
  -> MerchantAccount 
  -> Bool
  -> (Maybe OrderCreateRequest) 
  -> (Maybe OrderReference) 
  -> Flow OrderStatusResponse -- Foreign
getOrderStatusWithoutAuth req orderId merchantAccount isAuthenticated maybeOrderCreateReq maybeOrd = do
  merchId <- case (getField @"merchantId" merchantAccount) of
                Nothing -> throwException $ myerr "3"
                Just v -> pure v
  cachedResp <- getCachedOrdStatus isAuthenticated orderId merchId  --TODO check for txn based refund
  resp <- case cachedResp of
            Nothing -> do
              resp <- getOrdStatusResp req merchantAccount isAuthenticated orderId --route params can be replaced with orderId?
         -- *     _    <- addToCache req isAuthenticated merchantAccount routeParams resp
              pure resp
            Just resp -> pure resp
  ordResp'   <- pure resp -- versionSpecificTransforms routeParams resp

  -- * This part probably shoul be moved to the separate function, used only for orderCreate request
  ordResp    <- if isJust maybeOrderCreateReq && isJust maybeOrd  then do
                    let order = fromJust maybeOrd
                    let orderCreateReq = fromJust maybeOrderCreateReq
                    checkAndAddOrderToken req orderCreateReq "routeParams" ordResp' merchId order
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

addOrderTokenToOrderStatus :: Int -> OrderCreateRequest -> Text -> Flow (Maybe OrderTokenResp)
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
            _ -> throwException err403
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
    _ -> throwException $ myerr "1" -- ecForbidden
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
-- TODO port unsed?
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
      keyPrefix True = "euler_ostatus_"
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
    Right Nothing -> pure Nothing -- log "redis_cache_value_not_found" ("value not found for this key " <> key) *> pure Nothing
    Left err -> pure Nothing -- log "redis_fetch_error" ("Error while getting value from cache " <> key <> "_" <> show err) *> pure Nothing

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

getOrdStatusResp
  :: OrderStatusRequest
  -> MerchantAccount
  -> Bool
  -> Text -- RouteParameters
  -> Flow OrderStatusResponse
getOrdStatusResp req {- @(OrderStatusRequest ordReq) -} mAccnt isAuthenticated routeParam = do
    orderId'     <- pure routeParam -- getOrderId req routeParam
    merchantId'  <- getMerchantId mAccnt -- unNullOrErr500 (mAccnt ^. _merchantId)
 --   _           <- logInfo "Get order status from DB" $ "fetching order status from DB for merchant_id " <> merchantId <> " orderId " <> orderId
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
      -- pure defaultOrderReference -- DB.findOneWithErr ecDB (where_ := WHERE ["order_id" /\ String orderId, "merchant_id" /\ String merchantId]) (orderNotFound orderId)
 --   let maybeTxnUuid = (unNullOrUndefined ordReq.txnUuid) <|> (unNullOrUndefined ordReq.txn_uuid)
 --   maybeTxn    <- runMaybeT $ MaybeT (getTxnFromTxnUuid order maybeTxnUuid) <|> MaybeT (getLastTxn order)
    paymentlink <- getPaymentLink mAccnt order
    ordResp'    <- fillOrderDetails isAuthenticated paymentlink order defaultOrderStatusResponse
                    >>= addPromotionDetails order
    ordResp     <- addMandateDetails order ordResp'
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
    pure ordResp

-- ----------------------------------------------------------------------------
-- function: getTxnFromTxnUuid
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
getTxnFromTxnUuid order maybeTxnUuid = do
  case maybeTxnUuid of
    Just txnUuid' -> do
      orderId' <- whenNothing (getField @"orderId" order) (throwException err500) -- unNullOrErr500 $ order ^. _orderId
      merchantId' <- whenNothing (getField @"merchantId" order) (throwException err500)--unNullOrErr500 $ order ^. _merchantId

      txnDetail <- withDB eulerDB $ do
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
      return txnDetail
    Nothing -> pure Nothing


-- ----------------------------------------------------------------------------
-- function: getOrderId
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

fillOrderDetails :: Bool
  -> Paymentlinks
  -> OrderReference
  -> OrderStatusResponse
  -> Flow OrderStatusResponse
fillOrderDetails isAuthenticated paymentLinks ord status = do
  let --resp = status
      -- ordObj = ord
  id <- whenNothing (orderUuid ord) (throwException $ myerr "4")-- unNullOrErr500 ordObj.orderUuid
  let nullVal = Nothing -- nullValue unit -- create foreign (JS) null value ???
  customerId    <- case (customerId ord) of
                    Just customerId -> pure customerId
                    Nothing -> pure "" -- nullVal -- What is this? (Foreign null) ???
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


-- ----------------------------------------------------------------------------
-- function: formatAmount
-- TODO port or use sanitizeAmount/sanitizeNullAmount?
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

addMandateDetails :: OrderReference -> OrderStatusResponse -> Flow  OrderStatusResponse
addMandateDetails ordRef orderStatus =
  case (orderType ordRef) of
    Just orderType ->
      if orderType == C.MANDATE_REGISTER then do
        orderId <- pure $ getField @"id" ordRef  -- unNullOrErr500 $ ordRef ^. _id
        mandate :: Maybe Mandate <- do
          conn <- getConn eulerDB
          let predicate Mandate {authOrderId, merchantId} = authOrderId ==. B.val_ orderId
                &&. merchantId ==. (B.val_ $ fromMaybe "" $ ordRef ^. _merchantId)
          res <- runDB conn $ do
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

getPaymentLink :: MerchantAccount
 -> OrderReference
 -> Flow Paymentlinks
getPaymentLink mAcc orderRef = do
 -- maybeResellerAccount :: Maybe ResellerAccount <- DB.findOne ecDB (where_ := WHERE ["reseller_id" /\ String (unNull (account ^._resellerId) "")] :: WHERE ResellerAccount)
 -- (maybeResellerAccount :: Maybe ResellerAccount) <- pure $ Just defaultResellerAccount
  maybeResellerEndpoint <- do
    conn <- getConn eulerDB
    res  <- runDB conn $ do
      let predicate ResellerAccount {resellerId} = resellerId ==. B.val_ (fromMaybe "" $ mAcc ^. _resellerId)
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
  pure $ createPaymentLinks (fromMaybe "" $ getField @"orderUuid" orderRef) maybeResellerEndpoint


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
          finalReturnUrl <- if (orderRefReturnUrl == "") then pure $ fromMaybe merchantIframeReturnUrl (getField @"returnUrl" merchantAcc ) else pure orderRefReturnUrl
          pure $ finalReturnUrl
    Nothing -> pure $ ""

-- ----------------------------------------------------------------------------
-- function: getChargedTxn
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
    _ -> pure $ find (\txn -> (getField @"status" txn == CHARGED)) txnDetails


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

data Config = Config
  { protocol :: Text
  , host :: Text
  , internalECHost :: Text
  }
  deriving (Show, Read, Eq, Ord, Generic, ToJSON, FromJSON)
  
defaultConfig = Config
    { protocol = "https"
    , host = "defaulthost"
    , internalECHost = "defaultInternalECHost"
    }

createPaymentLinks :: Text -> Maybe Text -> Paymentlinks
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
-- function: addPromotionDetails
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

addPromotionDetails :: OrderReference -> OrderStatusResponse -> Flow OrderStatusResponse
addPromotionDetails orderRef orderStatus = do
  -- Order contains two id -like fields (better names?)
  let orderId  = getField @"id" orderRef -- unNull (orderRef ^. _id) 0 -- id Int
      ordId = fromMaybe "" (getField @"orderId" orderRef)-- unNull (orderRef ^. _orderId) "" --orderId Maybe Text
  promotions <- do
    conn <- getConn eulerDB
    res  <- runDB conn $ do
      let predicate Promotions {orderReferenceId} = orderReferenceId ==. B.val_ orderId
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
-- TODO update
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
  gatewayRefId <- (undefined :: Flow Text) -- TODO: getGatewayReferenceId txn ordRef
  logInfo "gatewayRefId " gatewayRefId
  pure $ orderStatus
    { status = show $ getField @"status" txn
    , status_id = txnStatusToInt $ getField @"status" txn
    , txn_id = Just $ getField @"txnId" txn
    , txn_uuid = getField @"txnUuid" txn
    , gateway_id = Just gatewayId
    , gateway_reference_id = Just gatewayRefId
    , bank_error_code = whenNothing (getField @"bankErrorCode" txn) (Just "")
    , bank_error_message = whenNothing (getField @"bankErrorMessage" txn) (Just "")
    , gateway_payload = addGatewayPayload txn
    , txn_detail = Just (undefined :: TxnDetail') -- TODO: mapTxnDetail txn
    }
  where addGatewayPayload txn =
         if (isBlankMaybe $ getField @"gatewayPayload" txn)
           then getField @"gatewayPayload" txn
           else Nothing
           

-- ----------------------------------------------------------------------------
-- function:
-- TODO update/port
-- ----------------------------------------------------------------------------

{-PS

-}


-- ----------------------------------------------------------------------------
-- function:
-- TODO update/port
-- ----------------------------------------------------------------------------

{-PS

-}

-- ----------------------------------------------------------------------------
-- ----------------------------------------------------------------------------
-- ----------------------------------------------------------------------------
-- ----------------------------------------------------------------------------
-- ----------------------------------------------------------------------------
-- ----------------------------------------------------------------------------
-- ----------------------------------------------------------------------------
-- ----------------------------------------------------------------------------
-- ----------------------------------------------------------------------------
 
{-



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



-}


-- ----------------------------------------------------------------------------
-- ----------------------------------------------------------------------------
-- ----------------------------------------------------------------------------
-- ----------------------------------------------------------------------------
-- ----------------------------------------------------------------------------
-- ----------------------------------------------------------------------------
-- ----------------------------------------------------------------------------
-- ----------------------------------------------------------------------------


--
-- OLD CODE
--


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


-- from src/Types/Storage/EC/Mandate.purs
mapMandate :: Mandate -> Mandate'
mapMandate Mandate {..} =
   Mandate' {  mandate_token = token
             , mandate_status = Just $ show $ status
             , mandate_id = mandateId
            }







-- from src/Types/Communication/OLTP/OrderStatus.purs
getOrderStatusRequest :: Text -> OrderStatusRequest
getOrderStatusRequest ordId = OrderStatusRequest {  txn_uuid    = Nothing
                                                  , merchant_id = Nothing
                                                  , order_id    = Just ordId
                                                  , txnUuid     = Nothing
                                                  , merchantId  = Nothing
                                                  , orderId     = Nothing
                                                 -- , "options.add_full_gateway_response" : NullOrUndefined Nothing
                                                  }









myerr400 n = err400 { errBody = "Err # " <> n }










------------------------------------------------------------------------------------------



------------------------------------------------------------------------------------------
-- Gateway
------------------------------------------------------------------------------------------

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

  --case join paymentGatewayResp of
  case mPaymentGatewayResp of
    Just pgr -> do
      ordStatus <- getPaymentGatewayResponse txn pgr orderStatus

      -- unNullOrUndefined (ordStatus ^._payment_gateway_response'))
      -- TODO do we need unNullOrUndefined here?
      let mPgr' = getField @"payment_gateway_response'" ordStatus
      case mPgr' of
        Just pgr' -> do
          gatewayResponse <- getGatewayResponseInJson pgr shouldSendFullGatewayResponse
          let r = MerchantPaymentGatewayResponse {
               resp_code = Just $ checkNull $ getField @"resp_code" pgr' -- : just $ checkNull pgr'.resp_code nullVal
            ,  rrn = Just $ checkNull $ getField @"resp_code" pgr' -- : just $ checkNull pgr'.rrn nullVal
            ,  created = Just $ checkNull $ getField @"resp_code" pgr' -- : just $ checkNull pgr'.created nullVal
            ,  epg_txn_id = Just $ checkNull $ getField @"resp_code" pgr' -- : just $ checkNull pgr'.epg_txn_id nullVal
            ,  resp_message = Just $ checkNull $ getField @"resp_code" pgr' -- : just $ checkNull pgr'.resp_message nullVal
            ,  auth_id_code = Just $ checkNull $ getField @"resp_code" pgr' -- : just $ checkNull pgr'.auth_id_code nullVal
            ,  txn_id = Just $ checkNull $ getField @"resp_code" pgr' -- : just $ checkNull pgr'.txn_id nullVal
            ,  offer = getField @"offer" pgr' -- : pgr'.offer
            ,  offer_type = getField @"offer_type" pgr' -- : pgr'.offer_type
            ,  offer_availed = getField @"offer_availed" pgr' -- : pgr'.offer_availed
            ,  discount_amount = getField @"discount_amount" pgr' -- : pgr'.discount_amount
            ,  offer_failure_reason = getField @"offer_failure_reason" pgr' -- : pgr'.offer_failure_reason
            ,  gateway_response = gatewayResponse -- TODO Text/ByteString?
          }

          -- ordStatus' = ordStatus # _payment_gateway_response' .~ (NullOrUndefined Nothing)
          --pure $ ordStatus' # _payment_gateway_response .~ (just pgr)
          return orderStatus

        Nothing -> return orderStatus
    Nothing -> return orderStatus

  -- TODO move to common utils or sth to that effect
  where
    checkNull :: Maybe Text -> Text
    checkNull (Just resp)
      | resp == "null" = nullVal  --  (unNull resp "") == "null" = nullVal
      | otherwise      = resp -- TODO original: toForeign $ (unNull resp "")
      where
        -- TODO what's that?  nullVal   <- pure $ nullValue unit
        nullVal = mempty
    checkNull Nothing = mempty

-- TODO port
getPaymentGatewayResponse ::
  -- ∀ st r.
  --     Newtype st
  --       { orderId           :: Maybe String
  --       , merchantId        :: Maybe String
  --       , isDBMeshEnabled   :: Maybe Boolean
  --       , isMemCacheEnabled :: Boolean
  --       | r }
  --     =>
  TxnDetail
  -> PaymentGatewayResponse
  -> OrderStatusResponse
  -> Flow OrderStatusResponse
getPaymentGatewayResponse txn pgr orderStatusResp =
  -- getResponseXMLTuple pgrXml
  --   <#>
  --     (casematch txn pgr upgr gateway >>> just >>> \v → orderStatusResp # _payment_gateway_response' .~ v)

  -- where
  --       gateway = unNull (txn ^. _gateway) ""
  --       pgrXml  = unNull (pgr ^. _responseXml) ""
  --       date    = pgr ^. _dateCreated # unNullOrUndefined >>> maybe nothing (_dateToString >>> just)
  --       upgr    = defaultPaymentGatewayResponse # _created .~ date
  return orderStatusResp

-- TODO port
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


getGatewayReferenceId :: TxnDetail -> OrderReference -> Flow Text
getGatewayReferenceId txn ordRef = do

  let ordRefId = fromMaybe 0 (getField @"id" ordRef)
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
            Nothing -> (undefined :: Flow Text)  -- TODO: checkGatewayRefIdForVodafone ordRef txn
            Just metadata -> do
              gRefId <- pure $ Map.lookup ((fromMaybe "" $ getField @"gateway" txn) <> ":gateway_reference_id") metadata
              jusId  <- pure $ Map.lookup "JUSPAY:gateway_reference_id" metadata
              case (gRefId <|> jusId) of
                Just v -> pure v
                Nothing -> (undefined :: Flow Text)  -- TODO: checkGatewayRefIdForVodafone ordRef txn
        Nothing -> (undefined :: Flow Text)  -- TODO: checkGatewayRefIdForVodafone ordRef txn
    Nothing -> (undefined :: Flow Text)  -- TODO: checkGatewayRefIdForVodafone ordRef txn

-- checkGatewayRefIdForVodafone :: forall st r. Newtype st { orderId :: Maybe String, merchantId :: Maybe String, isDBMeshEnabled :: Maybe Boolean, isMemCacheEnabled :: Boolean | r } => OrderReference -> TxnDetail -> BackendFlow st _ Foreign
-- checkGatewayRefIdForVodafone ordRef txn = do
--   meybeFeature <- DB.findOne ecDB (where_ := WHERE ["name" /\ String ("USE_UDF2_FOR_GATEWAY_REFERENCE_ID"), "merchant_id" /\ String (ordRef .^. _merchantId)] :: WHERE Feature)
--   case meybeFeature of
--     Just feature -> if ((unNull (txn ^._gateway) "") == "HSBC_UPI") && (feature ^. _enabled) && (isPresent (ordRef ^. _udf2)) then pure $ toForeign (unNull (ordRef ^. _udf2) "") else pure $ nullValue unit
--     Nothing -> pure $ nullValue unit

-----------------------------------------------------------------------------------------







------------------------------------------------------------------------------------------
-- Card
------------------------------------------------------------------------------------------
addCardInfo :: TxnDetail -> TxnCardInfo -> Bool -> Maybe Text -> OrderStatusResponse -> OrderStatusResponse
addCardInfo txnDetail txnCardInfo shouldSendCardIsin cardBrandMaybe ordStatus =
  if isBlankMaybe (getField @"cardIsin" txnCardInfo) then
    let payment_method' = if isJust cardBrandMaybe then cardBrandMaybe else Just "UNKNOWN"
        cardDetails = Just $ getCardDetails txnCardInfo txnDetail shouldSendCardIsin
    in ordStatus
        { payment_method = payment_method'
        , payment_method_type = (Just "CARD")
        , card = cardDetails
        }
  else ordStatus


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

--------------------------------------------------------------------------------------