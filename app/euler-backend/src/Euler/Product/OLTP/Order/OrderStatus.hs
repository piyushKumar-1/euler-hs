{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE DataKinds                 #-}
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

import Servant.Server
import Data.Generics.Product.Fields

import Euler.API.Order
import Euler.API.RouteParameters
import Euler.Common.Types.DefaultDate
import Euler.Common.Types.Mandate
import Euler.Common.Utils
import Euler.Common.Types.TxnDetail
import Euler.Common.Types.Merchant
import Euler.Common.Types.Promotion
import Euler.Product.Domain.Order (Order)
import Euler.Product.OLTP.Services.AuthenticationService (extractApiKey, getMerchantId)


import Euler.Storage.Types.Customer
import Euler.Storage.Types.TxnDetail
import Euler.Storage.Types.Feature
import Euler.Storage.Types.Mandate
import Euler.Storage.Types.MerchantAccount
import Euler.Storage.Types.MerchantIframePreferences
import Euler.Storage.Types.MerchantKey
import Euler.Storage.Types.OrderReference
import Euler.Storage.Types.PaymentGatewayResponse
import Euler.Storage.Types.Promotions
import Euler.Storage.Types.ResellerAccount
import Euler.Storage.Types.TxnCardInfo

import Euler.Storage.Types.EulerDB as EDB

import qualified Euler.Common.Types.Order as C
import qualified Euler.Common.Metric      as Metric

import Euler.Storage.DBConfig
import qualified Database.Beam as B
import qualified Database.Beam.Backend.SQL as B
import Database.Beam ((==.), (&&.), (<-.), (/=.))



-- No state
--updateState :: String -> Maybe String -> BackendFlow OrderLocalState Configs OrderLocalState
--updateState merchantId orderId = do
--  state       <- get
--  meshEnabled <- isDBMeshEnabled  merchantId orderId false
--  memCacheEnabled <- getCache C.ecRedis C.memCacheEnabled <#> (fromMaybe false)
--  _           <- log "isDBMeshEnabled" meshEnabled
--  let state'   = state # _merchantId .~ (Just merchantId)
--      updState' = state' # _orderId .~ orderId
--      updState'' = updState' # _isMemCacheEnabled .~ memCacheEnabled
--      updState  = updState'' # _isDBMeshEnabled .~ (Just meshEnabled)
--  _           <- put updState
--  put updState

-- Not used
-- createOrderStatusResponse :: forall st r. Newtype st { orderId :: Maybe String, merchantId :: Maybe String, isDBMeshEnabled :: Maybe Boolean, isMemCacheEnabled :: Boolean | r }
--                 => String -> String -> MerchantAccount -> BackendFlow st _ Unit
-- createOrderStatusResponse orderId merchantId merchantAccount =  do
--   _ <- Presto.log "createOrderStatusResponse" $ "Creating order status cache for " <> merchantId <> " and order_id " <> orderId
--   orderStatusReq <- pure $ getOrderStatusRequest orderId
--   _              <- getOrderStatusWithoutAuth orderStatusReq empty merchantAccount true Nothing Nothing
--   pure unit

-- -> #################
-- -> #################
-- -> #################
-- -> #################
-- -> #################

data FlowState = FlowState
  { merchantId :: Maybe Text
  , orderId    :: Text
  }
  deriving (Generic, Show, Eq, ToJSON, FromJSON )


data FlowStateOption = FlowStateOption
  deriving (Generic, Show, Eq, ToJSON, FromJSON )

instance OptionEntity FlowStateOption FlowState

type APIKey = Text

-- * inside authentication used "x-forwarded-for" header
-- * can we collect all headers in Map and save them in state? before we run the flow?
processOrderStatusGET :: Text -> APIKey -> Flow OrderStatusResponse -- processOrderStatus
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

myerr n = err403 { errBody = "Err # " <> n }

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

-- from src/Types/Alias.purs
type AuthToken = Text

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

--getMerchantAccountForAuthToken (C.ClientAuthTokenData otokenData@{resourceType: "CUSTOMER"}) = do
--  Customer customer <- DB.findOneWithErr ecDB (where_ := WHERE ["id" /\ String otokenData.resourceId]) ecAccessDenied
--  DB.findOneWithErr ecDB (where_ := WHERE ["id" /\ Int customer.merchantAccountId]) ecAccessDenied
--
--getMerchantAccountForAuthToken (C.ClientAuthTokenData otokenData@{resourceType: _}) =
--  liftErr ecAccessDenied


getOrderStatusWithoutAuth :: OrderStatusRequest -> Text {-RouteParameters-} -> MerchantAccount -> Bool -> (Maybe OrderCreateRequest) -> (Maybe OrderReference) -> Flow OrderStatusResponse -- Foreign
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

checkAndAddOrderToken :: OrderStatusRequest -> OrderCreateRequest -> {-RouteParameters-} Text -> OrderStatusResponse -> Text {-MerchantAccount-} -> OrderReference -> Flow OrderStatusResponse
checkAndAddOrderToken orderStatusRequest orderCreateReq routeParams resp merchantId order = do
  orderIdPrimary <- maybe (throwException err500) pure (getField @"id" order)
 -- merchantId <- unNullOrErr500 (merchantAccount ^. _merchantId)
  let version = Just "2018-07-01" -- lookup "version" routeParams -- version should be in headers?
  if version >= Just "2018-07-01" then do
      orderTokenData  <- addOrderTokenToOrderStatus orderIdPrimary orderCreateReq merchantId
      pure $ setField @"juspay" orderTokenData resp
    else pure resp

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

-- new ->

-- getOrderReferenceFromDB :: Text -> Text -> Flow OrderReference -- OrderReference
-- getOrderReferenceFromDB orderId merchantId = do
--   _    <- logInfo "Get order reference from DB" $ "fetching order status from DB for merchant_id " <> merchantId <> " orderId " <> orderId
--   -- DB.findOneWithErr ecDB (where_ := WHERE ["order_id" /\ String orderId, "merchant_id" /\ String merchantId]) (orderNotFound orderId)
--   pure defaultOrderReference

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

-- from src/Validation/Offers/Validation.purs
-- sanitizeAmount :: Number -> Number
-- sanitizeAmount x = (fromMaybe x) <<< fromString <<< (toFixed 2) <<< fromNumber $ x
sanitizeAmount x = x
sanitizeNullAmount = fmap sanitizeAmount

decryptPromotionRules ::   Text -> Promotions -> Flow Promotion'
decryptPromotionRules ordId promotions = pure defaultPromotion' --do
--   let promotion = unwrap promotions
--   keyForDecryption <- doAffRR' "ecTempCardCred" (ecTempCardCred)
--   resp <- case (decryptAESRaw "aes-256-ecb" (base64ToHex (promotion.rules)) keyForDecryption Nothing) of
--     Right result -> pure result
--     Left err -> throwErr $ show err
--   rules  <- pure $ getRulesFromString resp
--   rValue <- pure $ getMaskedAccNo (rules ^. _value)
--   pure $ Promotion'
--           { id : just $ show (promotion.id)
--           , order_id : just ordId
--           , rules : just $ singleton (rules # _value .~ rValue)
--           , created : just $ _dateToString (promotion.dateCreated)
--           , discount_amount : just $ promotion.discountAmount
--           , status :just $ promotion.status
--           }

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

-- from src/Types/Storage/EC/Mandate.purs
mapMandate :: Mandate -> Mandate'
mapMandate Mandate {..} =
   Mandate' {  mandate_token = token
             , mandate_status = Just $ show $ status
             , mandate_id = mandateId
            }

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



  -- <- new

-- ?
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

myerr400 n = err400 { errBody = "Err # " <> n }

getOrderId :: OrderStatusRequest -> RouteParameters -> Flow Text
getOrderId orderReq routeParam = do
  let ordId = lookupRP @OrderId routeParam
  let orderid = ordId <|> getField @"orderId" orderReq <|> getField @"order_id" orderReq
  maybe (throwException $ myerr400 "invalid_request") pure orderid

getLastTxn :: OrderReference -> Flow (Maybe TxnDetail)
getLastTxn orderRef = do
  orderId' <- whenNothing (getField @"orderId" orderRef) (throwException err500) -- unNullOrErr500 $ order ^. _orderId
  merchantId' <- whenNothing (getField @"merchantId" orderRef) (throwException err500) --unNullOrErr500 $ order ^. _merchantId

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


addCardInfo :: TxnDetail -> TxnCardInfo -> Bool -> Maybe Text -> OrderStatusResponse -> OrderStatusResponse
addCardInfo txnDetail txnCardInfo shouldSendCardIsin cardBrandMaybe ordStatus =
  if isBlankMaybe (getField @"cardIsin" txnCardInfo) then
    let ordStatus1 = setField @"payment_method" (if isJust cardBrandMaybe then cardBrandMaybe else Just "UNKNOWN") ordStatus
        ordStatus2 = setField @"payment_method_type" (Just "CARD") ordStatus1
    in  setField @"card" (Just $ (undefined :: Card)) ordStatus2 -- TODO getCardDetails txnCardInfo txnDetail shouldSendCardIsin
  else ordStatus



-- <- #################################
-- <- #################################
-- <- #################################
-- <- #################################
-- <- #################################

-- Will be removing gateways one by one from here as part of direct upi integrations.
-- eulerUpiGateways :: [Gateway]
-- eulerUpiGateways = [HDFC_UPI, INDUS_UPI, KOTAK_UPI, SBI_UPI, ICICI_UPI, HSBC_UPI, VIJAYA_UPI, YESBANK_UPI, PAYTM_UPI]
{-
-- *MAIN FUNCTION - HANDLE BOTH API REQUESTS (GET, POST)
-- *maybe better to separate to two methods for GET and POST
processOrderStatus :: OrderStatusRequest -> RouteParameters -> BackendFlow OrderLocalState _ Foreign
processOrderStatus req routeParams = do
  -- *trying to find orderId, that have been 100% represented in case of GET method in route
  -- *or in case of POST in body, so no need in "maybe"
  let orderId = maybe (unNullOrUndefined $ req ^. _order_id) pure $ routeParams ^. (at "order_id")
  -- set metrics/log info
  _ <- setTransactionTracersInLogs orderId Nothing
  -- * Authentification. By API key for GET (and POST ?), with client_auth_token ? for POST,
  -- * or try to take MerchantAcc with merchantId for unauthenticated calls
  {merchantAccount, isAuthenticated} <- authenticateReqAndGetMerchantAcc req routeParams
  -- * put mAcc and OrderID in state (probably used for kvdb sharding)
  _ <- updateState (merchantAccount .^. _merchantId) orderId
    -- set metrics/log info
  _ <- Monitoring.addMerchantIdToTrackingInfo $ merchantAccount .^. _merchantId
  -- * if unauthenticated calls disabled by merchant - throw access forbidden
  _  <- rejectIfUnauthenticatedCallDisabled merchantAccount `skipIfB` isAuthenticated

  response <- getOrderStatusWithoutAuth req routeParams merchantAccount isAuthenticated Nothing Nothing
  _ <- Presto.log "Process Order Status Response" $ response
  pure response

getOrderStatusWithoutAuth :: forall st r rt. Newtype st { orderId :: Maybe String, merchantId :: Maybe String, isDBMeshEnabled :: Maybe Boolean, isMemCacheEnabled :: Boolean | r } =>
  OrderStatusRequest -> RouteParameters -> MerchantAccount -> Boolean -> (Maybe OrderCreateReq) -> (Maybe OrderReference) -> BackendFlow st {sessionId :: String, trackers :: StrMap Metric | rt} Foreign
getOrderStatusWithoutAuth req routeParams merchantAccount isAuthenticated maybeOrderCreateReq maybeOrd = do
  cachedResp <- getCachedOrdStatus isAuthenticated req merchantAccount routeParams --TODO check for txn based refund
  resp <- case cachedResp of
            Nothing -> do
              resp <- getOrdStatusResp req merchantAccount isAuthenticated routeParams
              _    <- addToCache req isAuthenticated merchantAccount routeParams resp
              pure resp
            Just resp -> pure resp
  -- resp       <- getOrdStatusResp req merchantAccount isAuthenticated routeParams `skipIf` cachedResp
  -- cache      <- addToCache req isAuthenticated merchantAccount routeParams resp
  ordResp'   <- versionSpecificTransforms routeParams resp

  ordResp    <- if isJust maybeOrderCreateReq && isJust maybeOrd  then do
                    let order = unsafePartial $ fromJust $ maybeOrd
                    let orderCreateReq = unsafePartial $ fromJust $ maybeOrderCreateReq
                    checkAndAddOrderToken req orderCreateReq routeParams ordResp' merchantAccount order
                  else pure ordResp'

  checkEnableCaseForResponse req routeParams ordResp

checkAndAddOrderToken :: forall st r rt. Newtype st { orderId :: Maybe String, merchantId :: Maybe String, isDBMeshEnabled :: Maybe Boolean, isMemCacheEnabled :: Boolean | r } =>
   OrderStatusRequest -> OrderCreateReq -> RouteParameters -> OrderStatusResponse -> MerchantAccount -> OrderReference -> BackendFlow st _ OrderStatusResponse
checkAndAddOrderToken orderStatusRequest orderCreateReq routeParams resp merchantAccount order = do
  let orderIdPrimary = order .^. _id
  merchantId <- unNullOrErr500 (merchantAccount ^. _merchantId)
  let version = lookup "version" routeParams -- version should be in headers?
  if version >= Just "2018-07-01" then do
      orderTokenData  <- addOrderTokenToOrderStatus orderIdPrimary orderCreateReq merchantId
      pure $ resp # _juspay .~ (orderTokenData)
    else pure resp

addOrderTokenToOrderStatus :: forall st r rt. Newtype st { orderId :: Maybe String, merchantId :: Maybe String, isDBMeshEnabled :: Maybe Boolean, isMemCacheEnabled :: Boolean | r } =>
   Int -> OrderCreateReq -> String -> BackendFlow st _ (NullOrUndefined OrderTokenResp)
addOrderTokenToOrderStatus orderId (OrderCreateReq orderCreateReq) merchantId = do
  case (unNullOrUndefined orderCreateReq."options.get_client_auth_token") of
    Just true -> do
      {token,expiry} <- RedisService.tokenizeResource (toForeign orderId) "ORDER" merchantId
      incrementClientAuthTokenGeneratedCount
      pure $ just $ OrderTokenResp {
              client_auth_token : just $ token
            , client_auth_token_expiry : just $ expiry
            }
    _ -> pure $ nothing

processOrderStatusWithParams :: OrderStatusRequest -> RouteParameters -> BackendFlow OrderLocalState _ Foreign
processOrderStatusWithParams req routeParams =
  rejectIfMerchantIdMissing req `skipIf` (authHeaderPresent routeParams) *> processOrderStatus req routeParams
  where authHeaderPresent routeParams = ((StrMap.lookup "Authorization" routeParams)
                                        <|> (StrMap.lookup "client_auth_token" routeParams)) >>= (const $ Just unit)

checkEnableCaseForResponse :: forall st r. Newtype st { orderId :: Maybe String, merchantId :: Maybe String, isDBMeshEnabled :: Maybe Boolean, isMemCacheEnabled :: Boolean | r } => OrderStatusRequest -> RouteParameters -> OrderStatusResponse -> BackendFlow st _ Foreign
checkEnableCaseForResponse req params resp =
  if isJust (StrMap.lookup "orderId" params) || isPresent (req ^. _orderId) then pure $ snakeCaseToCamelCase (encode resp)
   else pure (encode resp)

authenticateReqAndGetMerchantAcc ::
     OrderStatusRequest
  -> RouteParameters
  -> BackendFlow OrderLocalState _ {merchantAccount :: MerchantAccount, isAuthenticated :: Boolean}
authenticateReqAndGetMerchantAcc ostatusReq@(OrderStatusRequest req) headers = do
    let optApiKey = getApiKeyFromHeader headers
        maybeAuthToken = StrMap.lookup "client_auth_token" headers -- headers? not in req?
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


updateAuthTokenUsage :: AuthToken -> ClientAuthTokenData -> BackendFlow OrderLocalState _ Unit
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
  -> BackendFlow OrderLocalState _ {merchantAccount :: MerchantAccount, isAuthenticated :: Boolean}
authenticateReqWithClientAuthToken (OrderStatusRequest req) authToken headers = do
  maybeAuthTokenData :: Maybe ClientAuthTokenData <- getCachedValEC authToken
  case maybeAuthTokenData of
    Just authTokenData -> do
      _ <- updateAuthTokenUsage authToken authTokenData
      merchantAccount <- getMerchantAccountForAuthToken authTokenData
      pure {merchantAccount, isAuthenticated: true}
    Nothing -> liftErr ecAccessDenied

getMerchantAccountForAuthToken :: forall st r. Newtype st { orderId :: Maybe String, merchantId :: Maybe String, isDBMeshEnabled :: Maybe Boolean, isMemCacheEnabled :: Boolean | r } => ClientAuthTokenData -> BackendFlow st _ MerchantAccount
getMerchantAccountForAuthToken (ClientAuthTokenData otokenData@{resourceType: "ORDER"}) = do
  OrderReference orderReference <- DB.findOneWithErr ecDB (where_ := WHERE ["id" /\ Int (parseInt otokenData.resourceId)]) ecAccessDenied
  merchantId <- unNullOrErr500 orderReference.merchantId
  DB.findOneWithErr ecDB (where_ := WHERE ["merchant_id" /\ String merchantId]) ecAccessDenied

getMerchantAccountForAuthToken (ClientAuthTokenData otokenData@{resourceType: "CUSTOMER"}) = do
  Customer customer <- DB.findOneWithErr ecDB (where_ := WHERE ["id" /\ String otokenData.resourceId]) ecAccessDenied
  DB.findOneWithErr ecDB (where_ := WHERE ["id" /\ Int customer.merchantAccountId]) ecAccessDenied

getMerchantAccountForAuthToken (ClientAuthTokenData otokenData@{resourceType: _}) =
  liftErr ecAccessDenied

rejectIfUnauthenticatedCallDisabled :: MerchantAccount -> BackendFlow OrderLocalState _ Unit
rejectIfUnauthenticatedCallDisabled mAccnt =
  if isTrue (mAccnt ^. _enableUnauthenticatedOrderStatusApi)
  then continue unit -- continue = pure
  else liftErr ecForbidden

rejectIfMerchantIdMissing :: OrderStatusRequest -> BackendFlow OrderLocalState _ Unit
rejectIfMerchantIdMissing (OrderStatusRequest req) =
  let maybeMerchantId = unNullOrUndefined req.merchantId
                        <|> (unNullOrUndefined req.merchant_id)
  in
  if isJust maybeMerchantId
  then continue unit
  else liftErr merchantIdMissing

getCachedOrdStatus :: forall st rt r.
  Newtype st { orderId :: Maybe String, merchantId :: Maybe String, isDBMeshEnabled :: Maybe Boolean, isMemCacheEnabled :: Boolean | r }
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
            _ <- Presto.log "Fetch cache from order status" $ "Order status cache feature is enabled"
            orderId <- getOrderId req routeParam
            merchantId <- unNullOrErr500 (mAccnt ^. _merchantId)
            val <- getCachedResp ((keyPrefix isAuthenticated) <> merchantId <> "_" <> orderId)
            case val of
              Just value -> do
                _ <- Monitoring.incrementOrderStatusCacheHitCount merchantId
                _ <- log "order status api response from cache" ("merchant_id " <> merchantId <> " orderId " <> orderId)
                _ <- Presto.log "Fetch cache from order status" $ "Order status response found in cache for merchant_id " <> merchantId <> " orderId " <> orderId
                pure val
              Nothing -> do
                _ <- Monitoring.incrementOrderStatusCacheMissCount merchantId
                _ <- Presto.log "Fetch cache from order status" $ "Could not find order status response in cache for merchant_id " <> merchantId <> " orderId " <> orderId
                pure val
          false -> do
            _ <- Presto.log "Fetch cache from order status" $ "Order status cache feature is not enabled"
            pure Nothing
      keyPrefix true = "euler_ostatus_"
      keyPrefix false = "euler_ostatus_unauth_"


addToCache :: forall st rt r.
  Newtype st { orderId :: Maybe String, merchantId :: Maybe String, isDBMeshEnabled :: Maybe Boolean, isMemCacheEnabled :: Boolean | r }
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
        --let val' = S.replaceAll (S.Pattern "null") (S.Replacement (show "null")) v
        --    val = S.replaceAll (S.Pattern "\"\"null\"\"") (S.Replacement (show "null")) val'
        let resp = fromMaybe (toForeign "") (parseAndReplaceWithStringNull Just Nothing v)
        _ <- Presto.log ("Cache value for this order status cache key " <> key) v
        case (runExcept (decode (camelCaseToSnakeCase resp))) of
          Right typedVal -> pure (replaceObjValWithForeignNull typedVal Just Nothing)
          Left err -> log "decode_error" ("Error while decoding cached value for " <> key <> "_" <> show err) *> pure Nothing
    Right Nothing -> log "redis_cache_value_not_found" ("value not found for this key " <> key) *> pure Nothing
    Left err -> log "redis_fetch_error" ("Error while getting value from cache " <> key <> "_" <> show err) *> pure Nothing

getOrdStatusResp :: forall st rt r.
  Newtype st { orderId :: Maybe String, merchantId :: Maybe String, isDBMeshEnabled :: Maybe Boolean, isMemCacheEnabled :: Boolean | r }
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
    case maybeTxn of
      Just txn -> do
        addTxnDetailsToResponse txn order ordResp
        >>= addRiskCheckInfoToResponse txn
        >>= addPaymentMethodInfo mAccnt txn
        >>= addRefundDetails txn
        >>= addChargeBacks txn
        >>= addGatewayResponse txn
      Nothing ->  pure ordResp

fillOrderDetails :: forall st rt r.
  Newtype st { orderId :: Maybe String, merchantId :: Maybe String, isDBMeshEnabled :: Maybe Boolean, isMemCacheEnabled :: Boolean | r }
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
  pure $ wrap resp
       { id = id
       , merchant_id = ordObj.merchantId
       , order_id = ordObj.orderId
       , customer_id = just customerId
       , product_id = unNull ordObj.productId ""
       , status = show $ ordObj.status
       , status_id = orderStatusToInt ordObj.status
       , amount = sanitizeNullAmount ordObj.amount
       , currency = ordObj.currency
       , refunded = ordObj.refundedEntirely
       , payment_links = paymentLinks
       , amount_refunded = sanitizeNullAmount ordObj.amountRefunded
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

addMandateDetails :: forall st r rt. Newtype st { orderId :: Maybe String, merchantId :: Maybe String, isDBMeshEnabled :: Maybe Boolean, isMemCacheEnabled :: Boolean | r } => OrderReference -> OrderStatusResponse -> BackendFlow st {sessionId :: String, trackers :: StrMap Metric | rt} OrderStatusResponse
addMandateDetails ordRef orderStatus =
  case (unNullOrUndefined $ ordRef ^._orderType) of
    Just orderType ->
      if orderType == MANDATE_REGISTER then do
        orderId <- unNullOrErr500 $ ordRef ^. _id
        mandate :: Maybe Mandate <- DB.findOne ecDB (where_ := WHERE ["auth_order_id" /\ Int orderId, "merchant_id" /\ String (unNull (ordRef ^._merchantId) "")])
        case mandate of
          Just mandateVal -> pure $ orderStatus # _mandate .~ (just $ mapMandate $ mandateVal)
          Nothing -> pure $ orderStatus
        else pure $ orderStatus
    Nothing -> pure $ orderStatus

getPaymentLink :: forall st rt r.
  Newtype st { orderId :: Maybe String, merchantId :: Maybe String, isDBMeshEnabled :: Maybe Boolean, isMemCacheEnabled :: Boolean | r }
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

getReturnUrl :: forall st r. Newtype st { orderId :: Maybe String, merchantId :: Maybe String, isDBMeshEnabled :: Maybe Boolean, isMemCacheEnabled :: Boolean | r } => OrderReference -> Boolean -> BackendFlow st _ String
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
      -- if (finalReturnUrl /= "") then do
      --     maybeTxn <- runMaybeT $ MaybeT (getChargedTxn orderRef) <|> MaybeT (getLastTxn orderRef)
      --     -- if (includeParams == true) then do
      --     params   <- getParamsForReturnUrl maybeTxn mirrorGatewayResponse orderRef
      --     isParams <- null params
      --     --params'  <- getParamsHash params `skipIfB` isParams
      --     pure $ finalReturnUrl
      --  else pure $ finalReturnUrl
    Nothing -> pure $ ""


getChargedTxn :: forall st r. Newtype st { orderId :: Maybe String, merchantId :: Maybe String, isDBMeshEnabled :: Maybe Boolean, isMemCacheEnabled :: Boolean | r } => OrderReference -> BackendFlow st _ (Maybe TxnDetail)
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

addPromotionDetails :: forall st r rt. Newtype st { orderId :: Maybe String, merchantId :: Maybe String, isDBMeshEnabled :: Maybe Boolean, isMemCacheEnabled :: Boolean | r } => OrderReference -> OrderStatusResponse -> BackendFlow st { sessionId :: String | rt} OrderStatusResponse
addPromotionDetails orderRef orderStatus = do
  let orderId  = unNull (orderRef ^. _id) 0
      ordId = unNull (orderRef ^. _orderId) ""
  promotions <- DB.findAll ecDB (where_ := WHERE ["order_reference_id" /\ Int orderId] :: WHERE Promotions)
  case (length promotions) of
    0 -> pure $ orderStatus
    _ -> do
      promotion' <- pure $ find (\promotion -> (promotion ^. _status == "ACTIVE")) promotions
      case promotion' of
        Just promotionVal -> do
          promotion  <- decryptPromotionRules ordId promotionVal
          let amount  = (orderStatus ^.. _amount $ 0.0) + (promotion ^.. _discount_amount $ 0.0)
              ordS    = orderStatus # _amount .~ (just $ sanitizeAmount amount)
          pure $ ordS # _promotion .~ (just promotion)
        Nothing -> pure orderStatus
      -- promotion' <- filterA (\promotion -> pure (promotion ^. _status == "ACTIVE")) promotions
      -- promotion  <- traverse (decryptPromotionRules ordId) promotion'
      -- let ordS    = orderStatus # _promotion .~ (just promotion)
      --     amount  = (ordS ^.. _amount $ 0.0) + (promotion ^.. _discount_amount $ 0.0)
      -- pure $ orderStatus # _amount .~ (just $ sanitizeAmount amount)

decryptPromotionRules :: forall st r rt. Newtype st { orderId :: Maybe String, merchantId :: Maybe String, isDBMeshEnabled :: Maybe Boolean, isMemCacheEnabled :: Boolean | r } => String -> Promotions -> BackendFlow st rt Promotion'
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

addTxnDetailsToResponse :: forall r st.
  Newtype st { orderId :: Maybe String, merchantId :: Maybe String, isDBMeshEnabled :: Maybe Boolean, isMemCacheEnabled :: Boolean | r }
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

getGatewayReferenceId :: forall st r. Newtype st { orderId :: Maybe String, merchantId :: Maybe String, isDBMeshEnabled :: Maybe Boolean, isMemCacheEnabled :: Boolean | r } => TxnDetail -> OrderReference -> BackendFlow st _ Foreign
getGatewayReferenceId txn ordRef = do
  ordMeta <- DB.findOne ecDB (where_ := WHERE ["order_reference_id" /\ Int (unNull (ordRef ^. _id) 0)] :: WHERE OrderMetadataV2)
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

checkGatewayRefIdForVodafone :: forall st r. Newtype st { orderId :: Maybe String, merchantId :: Maybe String, isDBMeshEnabled :: Maybe Boolean, isMemCacheEnabled :: Boolean | r } => OrderReference -> TxnDetail -> BackendFlow st _ Foreign
checkGatewayRefIdForVodafone ordRef txn = do
  meybeFeature <- DB.findOne ecDB (where_ := WHERE ["name" /\ String ("USE_UDF2_FOR_GATEWAY_REFERENCE_ID"), "merchant_id" /\ String (ordRef .^. _merchantId)] :: WHERE Feature)
  case meybeFeature of
    Just feature -> if ((unNull (txn ^._gateway) "") == "HSBC_UPI") && (feature ^. _enabled) && (isPresent (ordRef ^. _udf2)) then pure $ toForeign (unNull (ordRef ^. _udf2) "") else pure $ nullValue unit
    Nothing -> pure $ nullValue unit

addRiskCheckInfoToResponse :: forall st rt r.
  Newtype st { orderId :: Maybe String, merchantId :: Maybe String, isDBMeshEnabled :: Maybe Boolean, isMemCacheEnabled :: Boolean | r }
  => TxnDetail
  -> OrderStatusResponse
  -> BackendFlow st { sessionId :: String | rt} OrderStatusResponse
addRiskCheckInfoToResponse txn orderStatus = do
  txnRiskCheck <- DB.findOne ecDB (where_ := WHERE ["txn_detail_id" /\ Int (unNull (txn ^. _id) 0)] :: WHERE TxnRiskCheck)
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

addRiskObjDefaultValueAsNull :: forall st r. Newtype st { orderId :: Maybe String, merchantId :: Maybe String, isDBMeshEnabled :: Maybe Boolean, isMemCacheEnabled :: Boolean | r } => Risk' -> BackendFlow st _ Risk
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

addPaymentMethodInfo :: forall st r.
  Newtype st { orderId :: Maybe String, merchantId :: Maybe String, isDBMeshEnabled :: Maybe Boolean, isMemCacheEnabled :: Boolean | r }
  => MerchantAccount
  -> TxnDetail
  -> OrderStatusResponse
  -> BackendFlow st _ OrderStatusResponse
addPaymentMethodInfo mAccnt txn ordStatus = do
  optCard <- DB.findOne ecDB (where_ := WHERE ["txn_detail_id" /\ Int (unNull (txn ^._id) 0)])
  let enableSendingCardIsin = unNull (mAccnt ^. _enableSendingCardIsin) false
  case optCard of
    Just card -> do
      --  cardInfo <- DB.findOne ecDB (where_ := WHERE ["card_isin" /\ String (unNull (card ^. _cardIsin) "")] :: WHERE CardInfo)
      --  let cardSwitchProvider = maybe "" ( _.cardSwitchProvider <<< unwrap) cardInfo
       cardBrandMaybe <- getCardBrandFromIsin (unNull (card ^. _cardIsin) "")
       orderStatus <- updatePaymentMethodAndType txn card ordStatus
       pure $ addCardInfo txn card enableSendingCardIsin cardBrandMaybe
          <<< addAuthType card
          <<< addEmi txn $ orderStatus
    Nothing -> pure ordStatus

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

-- getCardBrandValue :: String -> String -> String
-- getCardBrandValue cardIsin cardSwitchProvider =
  --  if (getCardBrand cardIsin == "") then cardSwitchProvider else getCardBrand cardIsin


  -- let cardIsin' = S.replaceAll (S.Pattern " ") (S.Replacement "") (S.trim cardIsin)
  --     patternDiscover = unsafePartial $ fromRight $ regex "^6011\\d*|65\\d*|64[4-9]\\d*|622\\d*" noFlags
  --     patternDiners = unsafePartial $ fromRight $ regex "^36\\d*|38\\d*|30[0-5]\\d*" noFlags
  --     patternMaster = unsafePartial $ fromRight $ regex "^(51|52|53|54|55)\\d*" noFlags
  --     patternMaestro = unsafePartial $ fromRight $ regex "^(5018|5081|5044|504681|504993|5020|502260|5038|603845|603123|6304|6759|676[1-3]|6220|504834|504817|504645|504775)\\d*" noFlags
  --     rupayRanges = [ Tuple 508227 508227,
  --                     Tuple 508500 508999,
  --                     Tuple 603741 603741,
  --                     Tuple 606985 607984,
  --                     Tuple 608001 608500,
  --                     Tuple 652150 653149 ]
  --     rupay = fromMaybe (-1) (fromString (take 6 cardIsin'))
  --     rupayTrue = filter (\(Tuple x y) -> rupay >= x && rupay <= y) rupayRanges
  --     brand = if test patternMaster (take 2 cardIsin') then "MASTERCARD"
  --               else if (take 2 cardIsin' == "4") then "VISA"
  --               else if (take 2 cardIsin' == "34") || (take 2 cardIsin' == "37") then  "AMEX"
  --               else if test patternMaestro cardIsin' then "MAESTRO"
  --               else if length rupayTrue > 0 then "RUPAY"
  --               else if test patternDiners cardIsin' then  "DINERS"
  --               else if test patternDiscover cardIsin' then "DISCOVER"
  --               else if  take 2 cardIsin' == "35" then "JCB"
  --               else if take 6 cardIsin' == "637513" then "SODEXO"
  --               else if S.length cardIsin' == 6 then cardSwitchProvider else ""
  -- in brand

getPayerVpa :: forall st r. Newtype st { orderId :: Maybe String, merchantId :: Maybe String, isDBMeshEnabled :: Maybe Boolean, isMemCacheEnabled :: Boolean | r } =>
               TxnDetail -> TxnCardInfo -> BackendFlow st _ (NullOrUndefined Foreign)
getPayerVpa txn txnCardInfo = do
  if((txnCardInfo ^.. _paymentMethod $ "") == "GOOGLEPAY") then do
      pgr <- DB.findOne ecDB (where_ := WHERE ["id" /\ Int (unNull (txn ^. _successResponseId) (0))])
      case pgr of
        Just (PaymentGatewayResponse pg) -> do
          pgResponse  <- getResponseXml (unNull pg.responseXml "")
          payervpa <- lookupRespXml' pgResponse "payerVpa" ""
          if (payervpa == "") then pure nothing else pure $ just $ toForeign payervpa
        Nothing -> pure nothing
    else pure nothing


updatePaymentMethodAndType :: forall st r. Newtype st { orderId :: Maybe String, merchantId :: Maybe String, isDBMeshEnabled :: Maybe Boolean, isMemCacheEnabled :: Boolean | r } =>
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
        gateway <- pure $ find (\val -> (txn .^. _gateway == show val)) (eulerUpiGateways <> [AXIS_UPI])
        if (isJust gateway) then do
          let ord' = (ordS # _payer_app_name .~ paymentSource)
          pgr <- DB.findOne ecDB (where_ := WHERE ["id" /\ Int (unNull (txn ^. _successResponseId) (0))])
          case pgr of
            Just (PaymentGatewayResponse pg) -> do
              pgResponse  <- getResponseXml (unNull pg.responseXml "")
              payervpa <- lookupRespXml' pgResponse "payerVpa" ""
              if (payervpa == "") then pure ord' else pure (ord' # _payer_vpa .~ just (toForeign payervpa))
            Nothing -> pure ord'
          else pure ordS
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

addRefundDetails :: forall st r. Newtype st { orderId :: Maybe String, merchantId :: Maybe String, isDBMeshEnabled :: Maybe Boolean, isMemCacheEnabled :: Boolean | r } => TxnDetail -> OrderStatusResponse -> BackendFlow st _ OrderStatusResponse
addRefundDetails (TxnDetail txn) ordStatus = do
  refunds <- DB.findAll ecDB ( order := [["dateCreated" , "ASC"]]
     <> where_ := WHERE ["txn_detail_id" /\ Int  (nullOrUndefinedToAny (txn.id) 0)] :: WHERE Refund)
  case (length refunds) of
    -- No Refunds
    0 -> pure ordStatus
    -- Has Refunds
    _ -> pure $ ordStatus # _refunds .~ (just $ mapRefund <$> refunds)


addChargeBacks :: forall st r. Newtype st { orderId :: Maybe String, merchantId :: Maybe String, isDBMeshEnabled :: Maybe Boolean, isMemCacheEnabled :: Boolean | r } => TxnDetail -> OrderStatusResponse -> BackendFlow st _ OrderStatusResponse
addChargeBacks (TxnDetail txnDetail) orderStatus = do
  txn <- pure $ mapTxnDetail (TxnDetail txnDetail)
  chargeBacks <- DB.findAll ecDB (where_ := WHERE ["txn_detail_id" /\ Int (nullOrUndefinedToAny (txnDetail.id) 0)] :: WHERE Chargeback)
  case length chargeBacks of
    0 -> pure orderStatus
    _ -> pure $ orderStatus # _chargebacks .~ (just $ mapChargeback txn <$> chargeBacks)

lookupPgRespXml :: String -> String -> String -> BackendFlow _ _ String
lookupPgRespXml respxml key defaultValue = do
  respxmlVal <- getResponseXml respxml
  let keyVal = find (\val -> (key == (fromMaybe "" (val.string !! 0)))) respxmlVal
  pure $ case keyVal of
    Just val -> if isNotString $ fromMaybe defaultValue (val.string !! 1) then defaultValue else fromMaybe defaultValue (val.string !! 1)
    Nothing -> defaultValue

lookupRespXml :: forall st r rt. Newtype st { orderId :: Maybe String, merchantId :: Maybe String, isDBMeshEnabled :: Maybe Boolean, isMemCacheEnabled :: Boolean | r } => (Array _)  -> String -> String -> BackendFlow st rt String
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

lookupRespXml' :: forall st r rt. Newtype st { orderId :: Maybe String, merchantId :: Maybe String, isDBMeshEnabled :: Maybe Boolean, isMemCacheEnabled :: Boolean | r } =>(Array _) -> String -> String -> BackendFlow st rt String
lookupRespXml' xml str1 defaultValue = do
  str1 <- pure $ find (\val -> (str1 == (fromMaybe "" (val.string !! 0)))) xml
  case str1 of
    Just val -> if isNotString $ fromMaybe defaultValue (val.string !! 1) then pure $ defaultValue else pure $ fromMaybe defaultValue (val.string !! 1)
    Nothing -> pure $ defaultValue

lookupRespXmlVal :: String-> String -> String -> BackendFlow _ _ String
lookupRespXmlVal respXml str1 defaultValue = do
  xml <- getResponseXml respXml :: (BackendFlow _ _ (Array {string :: Array _}))
  let upVal = find (\val -> (str1 == (fromMaybe "" ((val.string) !! 0)))) xml
  pure $ case upVal of
    Just val  -> if isNotString $ fromMaybe defaultValue (val.string !! 1) then defaultValue else fromMaybe defaultValue (val.string !! 1)
    Nothing -> defaultValue

tempLookup :: forall st r rt. Newtype st { orderId :: Maybe String, merchantId :: Maybe String, isDBMeshEnabled :: Maybe Boolean, isMemCacheEnabled :: Boolean | r } => String -> String -> String -> BackendFlow st rt String
tempLookup xml str1 defaultValue = do
  x <- getResponseXml xml
  str1 <- pure $ find (\val -> str1 == val.string) x
  case str1 of
    Just val -> do
      obj <- pure $ getRecordValues val
      pure $ fromMaybe defaultValue (obj !! 1)
    Nothing -> pure defaultValue

hierarchyObjectLookup :: forall st r rt. Newtype st { orderId :: Maybe String, merchantId :: Maybe String, isDBMeshEnabled :: Maybe Boolean, isMemCacheEnabled :: Boolean | r } => String -> String -> String -> BackendFlow st rt String
hierarchyObjectLookup xml key1 key2 = do
  xmlVal <- getResponseXml xml
  val    <- pure $ find (\val -> (key1 == val.string)) xmlVal
  case val of
    Just v -> do
      let vA = (v."org.codehaus.groovy.grails.web.json.JSONObject".myHashMap.entry) -- ::  Array { string :: Array String | t594}
      lookupRespXml' vA key2 ""
    Nothing -> pure ""

addGatewayResponse :: forall st r. Newtype st { orderId :: Maybe String, merchantId :: Maybe String, isDBMeshEnabled :: Maybe Boolean, isMemCacheEnabled :: Boolean | r } => TxnDetail -> OrderStatusResponse -> BackendFlow st _ OrderStatusResponse
addGatewayResponse txn orderStatus = do
  paymentGatewayResp <- DB.findOne ecDB (where_ := WHERE ["id" /\ Int (unNull (txn ^. _successResponseId) (0))])
  case paymentGatewayResp of
    Just pgr -> do
      ordStatus <- getPaymentGatewayResponse txn pgr orderStatus
      nullVal   <- pure $ nullValue unit
      case (unNullOrUndefined (ordStatus ^._payment_gateway_response')) of
        Just (MerchantPaymentGatewayResponse' pgr') -> do
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
                      }
                ordStatus' = ordStatus # _payment_gateway_response' .~ (NullOrUndefined Nothing)
            pure $ ordStatus' # _payment_gateway_response .~ (just pgr)
        Nothing -> pure $ orderStatus
    Nothing -> pure $ orderStatus
  where checkNull resp nullVal      = if (unNull resp "") == "null" then nullVal else toForeign $ (unNull resp "")

getPaymentGatewayResponse :: forall st r. Newtype st { orderId :: Maybe String, merchantId :: Maybe String, isDBMeshEnabled :: Maybe Boolean, isMemCacheEnabled :: Boolean | r } => TxnDetail -> PaymentGatewayResponse -> OrderStatusResponse -> BackendFlow st _ OrderStatusResponse
getPaymentGatewayResponse txn paymentGatewayResp orderStatus = do
  let mpgr'       = defaultPaymentGatewayResponse
      ordStatus   = unwrap orderStatus
      pgr         = unwrap paymentGatewayResp
      respMessage = unNullOrUndefined pgr.respMessage
      gateway     = unNull (txn ^._gateway) ""
      pgrXml      = unNull pgr.responseXml ""
  pgResponse  <- getResponseXml pgrXml
  createdDate <- case (unNullOrUndefined pgr.dateCreated) of
                    Just date -> pure $ just $ _dateToString date
                    Nothing -> pure $ NullOrUndefined Nothing
  let mpgr = mpgr' # _created .~ createdDate
  if gateway == show CCAVENUE_V2 then do
    bankNo    <- lookupRespXml pgResponse "bank_ref_no" "order_bank_ref_no"
    trackId   <- lookupRespXml pgResponse "tracking_id" "reference_no"
    ordStatus <- lookupRespXml' pgResponse "order_status" "null"
    ordBankNo <- tempLookup pgrXml "order_bank_ref_no" "null"
    refNo     <- tempLookup pgrXml "reference_no" "null"
    rrn       <- if (bankNo == "null") then pure ordBankNo else pure bankNo
    epgTxnId  <- if (trackId == "null") then pure refNo else pure trackId
    let mpgr1 = mpgr # _txn_id .~ (just $ txn ^._txnId)
        mpgr2 = mpgr1 # _rrn .~ (just $ rrn)
        mpgr3 = mpgr2 # _epg_txn_id .~ (just $ epgTxnId)
        mpgr4 = mpgr3 # _auth_id_code .~ (just $ "NA")
        mpgr5 = mpgr4 # _resp_code .~ pgr.respCode
        mpgr6 = mpgr5 # _resp_message .~ (just $ (fromMaybe ordStatus respMessage))
    pure $ orderStatus # _payment_gateway_response' .~ (just $ mpgr6)
   else if gateway == show PAYTM_V2 then do
    epgTxnId <- lookupRespXml' pgResponse "TXNID" ""
    rrn      <- lookupRespXml' pgResponse "BANKTXNID" ""
    let mpgr1 = mpgr  # _txn_id .~ (just $ txn ^._txnId)
        mpgr2 = mpgr1 # _rrn .~ (just $ rrn)
        mpgr3 = mpgr2 # _epg_txn_id .~ (just $ epgTxnId)
        mpgr4 = mpgr3 # _auth_id_code .~ (just $ "")
        mpgr5 = mpgr4 # _resp_code .~ pgr.respCode
        mpgr6 = mpgr5 # _resp_message .~ pgr.respMessage
    pure $ orderStatus # _payment_gateway_response' .~ (just $ mpgr6)
   else if gateway == show PAYPAL then do
    epgTxnId <- lookupRespXml' pgResponse "paymentId" ""
    let mpgr1 = mpgr  # _txn_id .~ (just $ txn ^._txnId)
        mpgr2 = mpgr1 # _rrn .~ (just $ "")
        mpgr3 = mpgr2 # _epg_txn_id .~ (just $ epgTxnId)
        mpgr4 = mpgr3 # _auth_id_code .~ (just $ "NA")
        mpgr5 = mpgr4 # _resp_code .~ pgr.respCode
        mpgr6 = mpgr5 # _resp_message .~ pgr.respMessage
    pure $ orderStatus # _payment_gateway_response' .~ (just $ mpgr6)
   else if gateway == show OLAPOSTPAID then do
    epgTxnId <- lookupRespXml' pgResponse "transactionId" "null"
    let mpgr1 = mpgr  # _txn_id .~ (just $ txn ^._txnId)
        mpgr2 = mpgr1 # _rrn .~ (just $ "")
        mpgr3 = mpgr2 # _epg_txn_id .~ (just $ epgTxnId)
        mpgr4 = mpgr3 # _auth_id_code .~ (just $ "NA")
        mpgr5 = mpgr4 # _resp_code .~ pgr.respCode
        mpgr6 = mpgr5 # _resp_message .~ pgr.respMessage
    pure $ orderStatus # _payment_gateway_response' .~ (just $ mpgr6)
   else if gateway == show SIMPL then do
    epgTxnId <- lookupRespXml' pgResponse "id" "null"
    let mpgr1 = mpgr  # _txn_id .~ (just $ txn ^._txnId)
        mpgr2 = mpgr1 # _rrn .~ (just $ "")
        mpgr3 = mpgr2 # _epg_txn_id .~ (just $ epgTxnId)
        mpgr4 = mpgr3 # _auth_id_code .~ (just $ "NA")
        mpgr5 = mpgr4 # _resp_code .~ pgr.respCode
        mpgr6 = mpgr5 # _resp_message .~ pgr.respMessage
    pure $ orderStatus # _payment_gateway_response' .~ (just $ mpgr6)
   else if gateway == show PAYTM_V2 then do
    epgTxnId <- lookupRespXml' pgResponse "TXNID" ""
    rrn      <- lookupRespXml' pgResponse "BANKTXNID" ""
    let mpgr1 = mpgr  # _txn_id .~ (just $ txn ^._txnId)
        mpgr2 = mpgr1 # _rrn .~ (just $ rrn)
        mpgr3 = mpgr2 # _epg_txn_id .~ (just $ epgTxnId)
        mpgr4 = mpgr3 # _auth_id_code .~ (just $ "")
        mpgr5 = mpgr4 # _resp_code .~ pgr.respCode
        mpgr6 = mpgr5 # _resp_message .~ pgr.respMessage
    pure $ orderStatus # _payment_gateway_response' .~ (just $ mpgr6)
   else if gateway == show GOCASHFREE then do
    epgTxnId <- lookupRespXml' pgResponse "referenceId" ""
    let mpgr1 = mpgr  # _txn_id .~ (just $ txn ^._txnId)
        mpgr2 = mpgr1 # _rrn .~ (just $ "")
        mpgr3 = mpgr2 # _epg_txn_id .~ (just $ epgTxnId)
        mpgr4 = mpgr3 # _auth_id_code .~ (just $ "NA")
        mpgr5 = mpgr4 # _resp_code .~ pgr.respCode
        mpgr6 = mpgr5 # _resp_message .~ pgr.respMessage
    pure $ orderStatus # _payment_gateway_response' .~ (just $ mpgr6)
   else if gateway == show BLAZEPAY then do
    rrn      <- lookupRespXml pgResponse "issuerRefNo" "RRN"
    epgTxnId <- lookupRespXml pgResponse "pgTxnNo" "pgTxnId"
    respMsg  <- lookupRespXml' pgResponse "respMsg" "null"
    authId   <- lookupRespXml' pgResponse "authIdCode" "null"
    let mpgr1 = mpgr  # _txn_id .~ (just $ txn ^._txnId)
        mpgr2 = mpgr1 # _rrn .~ (just $ rrn)
        mpgr3 = mpgr2 # _epg_txn_id .~ (just $ epgTxnId)
        mpgr4 = mpgr3 # _auth_id_code .~ (just $ authId)
        mpgr5 = mpgr4 # _resp_code .~ pgr.respCode
        mpgr6 = mpgr5 # _resp_message .~ (just $ (fromMaybe respMsg respMessage))
    pure $ orderStatus # _payment_gateway_response' .~ (just $ mpgr6)
   else if gateway == show CITRUS then do
    rrn      <- lookupRespXml' pgResponse "issuerRefNo" "null"
    epgTxnId <- lookupRespXml' pgResponse "pgTxnNo" "null"
    authId   <- lookupRespXml' pgResponse "authIdCode" "null"
    let mpgr1 = mpgr  # _txn_id .~ (just $ txn ^._txnId)
        mpgr2 = mpgr1 # _rrn .~ (just $ rrn)
        mpgr3 = mpgr2 # _epg_txn_id .~ (just $ epgTxnId)
        mpgr4 = mpgr3 # _auth_id_code .~ (just $ authId)
        mpgr5 = mpgr4 # _resp_code .~ pgr.respCode
        mpgr6 = mpgr5 # _resp_message .~ pgr.respMessage
    pure $ orderStatus # _payment_gateway_response' .~ (just $ mpgr6)
   else if gateway == show ICICINB then do
    epgTxnId <- lookupRespXml' pgResponse "BID" "null"
    respCode <- lookupRespXml' pgResponse "PAID" "null"
    let mpgr1 = mpgr  # _txn_id .~ (just $ txn ^._txnId)
        mpgr2 = mpgr1 # _rrn .~ (just $ "NA")
        mpgr3 = mpgr2 # _epg_txn_id .~ (just $ epgTxnId)
        mpgr4 = mpgr3 # _auth_id_code .~ (just $ "NA")
        mpgr5 = mpgr4 # _resp_code .~ (just $ respCode)
        mpgr6 = mpgr5 # _resp_message .~ (just $ "NA")
    pure $ orderStatus # _payment_gateway_response' .~ (just $ mpgr6)
   else if gateway == show RAZORPAY then do
    epgTxnId <- lookupRespXml pgResponse "razorpay_payment_id" "id"
    let mpgr1 = mpgr  # _txn_id .~ (just $ txn ^._txnId)
        mpgr2 = mpgr1 # _rrn .~ (just $ "")
        mpgr3 = mpgr2 # _epg_txn_id .~ (just $ epgTxnId)
        mpgr4 = mpgr3 # _auth_id_code .~ (just $ "NA")
        mpgr5 = mpgr4 # _resp_code .~ (just $ unNull pgr.respCode "null")
        mpgr6 = mpgr5 # _resp_message .~ (just $ unNull pgr.respMessage "null")
    pure $ orderStatus # _payment_gateway_response' .~ (just $ mpgr6)
   else if gateway == show MOBIKWIK then do
    epgTxnId      <- lookupRespXml pgResponse "refid" "refId"
    statuscode    <- lookupRespXml' pgResponse "statuscode" "null"
    statusmessage <- lookupRespXml' pgResponse "statusmessage" "null"
    let mpgr1 = mpgr  # _txn_id .~ (just $ txn ^._txnId)
        mpgr2 = mpgr1 # _rrn .~ (just $ "")
        mpgr3 = mpgr2 # _epg_txn_id .~ (just $ epgTxnId)
        mpgr4 = mpgr3 # _auth_id_code .~ (just $ "NA")
        mpgr5 = mpgr4 # _resp_code .~ (just $ statuscode)
        mpgr6 = mpgr5 # _resp_message .~ (just $ statusmessage)
    pure $ orderStatus # _payment_gateway_response' .~ (just $ mpgr6)
   else if gateway == show OLAMONEY then do
    couponCode    <- lookupRespXml' pgResponse "couponCode" "null"
    isCashbackSuc <- lookupRespXml' pgResponse "isCashbackSuccessful" "null"
    epgTxnId      <- lookupRespXml' pgResponse "transactionId" "NA"
    let mpgr1 = mpgr  # _txn_id .~ (just $ txn ^._txnId)
        mpgr2 = mpgr1 # _rrn .~ (just $ "")
        mpgr3 = mpgr2 # _epg_txn_id .~ (just $ epgTxnId)
        mpgr4 = mpgr3 # _auth_id_code .~ (just $ "NA")
        mpgr5 = mpgr4 # _resp_code .~ pgr.respCode
        mpgr6 = mpgr5 # _resp_message .~ pgr.respMessage
    if couponCode /= "null" then do
      let mpgr7  = mpgr6 # _offer .~ (just $ couponCode)
          mpgr8  = mpgr7 # _offer_type .~ (just $ "NA")
          mpgr9  = mpgr8 # _offer_availed .~ (just $ toForeign $ strToBool isCashbackSuc)
          mpgr10 = mpgr9 # _discount_amount .~ (just $ nullValue unit)
      pure $ orderStatus # _payment_gateway_response' .~ (just $ mpgr10)
      else pure $ orderStatus # _payment_gateway_response' .~ (just $ mpgr6)
   else if gateway == show HDFC_EBS_VAS then do
    respCode     <- lookupRespXml' pgResponse "ResponseCode" "null"
    respMessage' <- lookupRespXml pgResponse "ResponseMessage" "Error"
    respMessage  <- if respMessage' == "null" then pure $ "" else pure $ respMessage'
    let mpgr1 = mpgr  # _txn_id .~ (just $ txn ^._txnId)
        mpgr2 = mpgr1 # _rrn .~ (just $ "")
        mpgr3 = mpgr2 # _epg_txn_id .~ (just $ "NA")
        mpgr4 = mpgr3 # _auth_id_code .~ (just $ "NA")
        mpgr5 = mpgr4 # _resp_code .~ (just $ respCode)
        mpgr6 = mpgr5 # _resp_message .~ (just $ respMessage)
    pure $ orderStatus # _payment_gateway_response' .~ (just $ mpgr6)
   else if gateway == show PAYLATER then do
    let mpgr1 = mpgr  # _txn_id .~ (just $ txn ^._txnId)
        mpgr2 = mpgr1 # _rrn .~ (just $ "")
        mpgr3 = mpgr2 # _epg_txn_id .~ (just $ "NA")
        mpgr4 = mpgr3 # _auth_id_code .~ (just $ "NA")
        mpgr5 = mpgr4 # _resp_code .~ (just $ "NA")
        mpgr6 = mpgr5 # _resp_message .~ (just $ "NA")
    pure $ orderStatus # _payment_gateway_response' .~ (just $ mpgr6)
   else if gateway == show TPSL then do
    rrn         <- lookupRespXml' pgResponse "clnt_txn_ref" "null"
    epgTxnId    <- lookupRespXml' pgResponse "tpsl_txn_id" "null"
    respCode    <- lookupRespXml' pgResponse "txn_status" "null"
    respMessage <- lookupRespXml' pgResponse "txn_msg" "null"
    let mpgr1 = mpgr  # _txn_id .~ (just $ txn ^._txnId)
        mpgr2 = mpgr1 # _rrn .~ (just $ rrn)
        mpgr3 = mpgr2 # _epg_txn_id .~ (just $ epgTxnId)
        mpgr4 = mpgr3 # _auth_id_code .~ (just $ "")
        mpgr5 = mpgr4 # _resp_code .~ (just $ respCode)
        mpgr6 = mpgr5 # _resp_message .~ (just $ respMessage)
    pure $ orderStatus # _payment_gateway_response' .~ (just $ mpgr6)
   else if (gateway == show AXIS || gateway == show MIGS) then do
    rrn        <- lookupRespXml' pgResponse "vpc_ReceiptNo" "null"
    epgTxnId   <- lookupRespXml' pgResponse "vpc_TransactionNo" "null"
    authIdCode <- lookupRespXml' pgResponse "vpc_AuthorizeId" "null"
    txnId      <- lookupRespXml' pgResponse "vpc_MerchTxnRef" "null"
    respCode   <- lookupRespXml' pgResponse "vpc_TxnResponseCode" "null"
    respMessage<- lookupRespXml' pgResponse "vpc_Message" "null"
    let mpgr1 = mpgr  # _txn_id .~ (just $ txnId)
        mpgr2 = mpgr1 # _rrn .~ (just $ rrn)
        mpgr3 = mpgr2 # _epg_txn_id .~ (just $ epgTxnId)
        mpgr4 = mpgr3 # _auth_id_code .~ (just $ authIdCode)
        mpgr5 = mpgr4 # _resp_code .~ (just $ respCode)
        mpgr6 = mpgr5 # _resp_message .~ (just $ respMessage)
    pure $ orderStatus # _payment_gateway_response' .~ (just $ mpgr6)
   else if gateway == show HDFC then do
    rrn        <- lookupRespXml' pgResponse "ref" "null"
    epgTxnId   <- lookupRespXml' pgResponse "paymentid" "null"
    authIdCode <- lookupRespXml' pgResponse "auth" "null"
    txnId      <- lookupRespXml' pgResponse "trackid" "null"
    respCode   <- lookupRespXml' pgResponse "result" "null"
    respMessage<- lookupRespXml' pgResponse "result" "null"
    let mpgr1 = mpgr  # _txn_id .~ (just $ txnId)
        mpgr2 = mpgr1 # _rrn .~ (just $ rrn)
        mpgr3 = mpgr2 # _epg_txn_id .~ (just $ epgTxnId)
        mpgr4 = mpgr3 # _auth_id_code .~ (just $ authIdCode)
        mpgr5 = mpgr4 # _resp_code .~ (just $ respCode)
        mpgr6 = mpgr5 # _resp_message .~ (just $ respMessage)
    pure $ orderStatus # _payment_gateway_response' .~ (just $ mpgr6)
   else if gateway == show ICICI then do
    rrn        <- lookupRespXml' pgResponse "RRN" "null"
    epgTxnId   <- lookupRespXml' pgResponse "epgTxnId" "null"
    authIdCode <- lookupRespXml' pgResponse "authIdCode" "null"
    txnId      <- lookupRespXml' pgResponse "txnId" "null"
    respCode   <- lookupRespXml' pgResponse "respCode" "null"
    respMessage<- lookupRespXml' pgResponse "respMessage" "null"
    let mpgr1 = mpgr  # _txn_id .~ (just $ txnId)
        mpgr2 = mpgr1 # _rrn .~ (just $ rrn)
        mpgr3 = mpgr2 # _epg_txn_id .~ (just $ epgTxnId)
        mpgr4 = mpgr3 # _auth_id_code .~ (just $ authIdCode)
        mpgr5 = mpgr4 # _resp_code .~ (just $ respCode)
        mpgr6 = mpgr5 # _resp_message .~ (just $ respMessage)
    pure $ orderStatus # _payment_gateway_response' .~ (just $ mpgr6)
   else if gateway == show DUMMY then do
    rrn        <- lookupRespXml' pgResponse "RRN" "null"
    epgTxnId   <- lookupRespXml' pgResponse "epgTxnId" "null"
    authIdCode <- lookupRespXml' pgResponse "authIdCode" "null"
    txnId      <- lookupRespXml' pgResponse "txnId" "null"
    respCode   <- lookupRespXml' pgResponse "respCode" "null"
    respMessage<- lookupRespXml' pgResponse "respMessage" "null"
    let mpgr1 = mpgr  # _txn_id .~ (just $ txnId)
        mpgr2 = mpgr1 # _rrn .~ (just $ rrn)
        mpgr3 = mpgr2 # _epg_txn_id .~ (just $ epgTxnId)
        mpgr4 = mpgr3 # _auth_id_code .~ (just $ authIdCode)
        mpgr5 = mpgr4 # _resp_code .~ (just $ respCode)
        mpgr6 = mpgr5 # _resp_message .~ (just $ respMessage)
    pure $ orderStatus # _payment_gateway_response' .~ (just $ mpgr6)
   else if gateway == show AMEX then do
    rrn        <- lookupRespXml' pgResponse "vpc_ReceiptNo" "null"
    epgTxnId   <- lookupRespXml' pgResponse "vpc_TransactionNo" "null"
    authIdCode <- lookupRespXml' pgResponse "vpc_AuthorizeId" "null"
    respCode   <- lookupRespXml' pgResponse "vpc_TxnResponseCode" "null"
    respMessage<- lookupRespXml' pgResponse "vpc_Message" "null"
    let mpgr1 = mpgr  # _txn_id .~ (just $ txn ^. _txnId)
        mpgr2 = mpgr1 # _rrn .~ (just $ rrn)
        mpgr3 = mpgr2 # _epg_txn_id .~ (just $ epgTxnId)
        mpgr4 = mpgr3 # _auth_id_code .~ (just $ authIdCode)
        mpgr5 = mpgr4 # _resp_code .~ (just $ respCode)
        mpgr6 = mpgr5 # _resp_message .~ (just $ respMessage)
    pure $ orderStatus # _payment_gateway_response' .~ (just $ mpgr6)
   else if gateway == show EBS then do
    rrn        <- lookupRespXml' pgResponse "vpc_TransactionId" "null"
    epgTxnId   <- lookupRespXml' pgResponse "vpc_PaymentId" "null"
    let mpgr1 = mpgr  # _txn_id .~ (pgr.txnId)
        mpgr2 = mpgr1 # _rrn .~ (just $ rrn)
        mpgr3 = mpgr2 # _epg_txn_id .~ (just $ epgTxnId)
        mpgr4 = mpgr3 # _auth_id_code .~ (just $ "")
        mpgr5 = mpgr4 # _resp_code .~ pgr.respCode
        mpgr6 = mpgr5 # _resp_message .~ pgr.respMessage
    pure $ orderStatus # _payment_gateway_response' .~ (just $ mpgr6)
   else if gateway == show EBS_V3 then do
    rrn        <- lookupRespXml  pgResponse "PaymentID" "paymentId"
    epgTxnId   <- lookupRespXml  pgResponse "TransactionID" "transactionId"
    let mpgr1 = mpgr  # _txn_id .~ (pgr.txnId)
        mpgr2 = mpgr1 # _rrn .~ (just $ rrn)
        mpgr3 = mpgr2 # _epg_txn_id .~ (just $ epgTxnId)
        mpgr4 = mpgr3 # _auth_id_code .~ (just $ "")
        mpgr5 = mpgr4 # _resp_code .~ pgr.respCode
        mpgr6 = mpgr5 # _resp_message .~ pgr.respMessage
    pure $ orderStatus # _payment_gateway_response' .~ (just $ mpgr6)
   else if gateway == show PAYTM then do
    rrn        <- lookupRespXml  pgResponse "TXNID" "TxnId"
    epgTxnId   <- lookupRespXml  pgResponse "TXNID" "TxnId"
    promoCampId<- lookupRespXml' pgResponse "PROMO_CAMP_ID" "null"
    promoStatus <- lookupRespXml' pgResponse "PROMO_STATUS" "null"
    let mpgr1 = mpgr  # _txn_id .~ (pgr.txnId)
        mpgr2 = mpgr1 # _rrn .~ (just $ rrn)
        mpgr3 = mpgr2 # _epg_txn_id .~ (just $ epgTxnId)
        mpgr4 = mpgr3 # _auth_id_code .~ (just $ "")
        mpgr5 = mpgr4 # _resp_code .~ pgr.respCode
        mpgr6 = mpgr5 # _resp_message .~ pgr.respMessage
    if promoCampId /= "null" then do
      let mpgr7  = mpgr6 # _offer .~ (just $ promoCampId)
          mpgr8  = mpgr7 # _offer_type .~ (just $ "NA")
          mpgr9  = mpgr8 # _offer_availed .~ (just $ toForeign (promoStatus == "PROMO_SUCCESS"))
          mpgr10 = mpgr9 # _discount_amount .~ (just $ nullValue unit)
      pure $ orderStatus # _payment_gateway_response' .~ (just $ mpgr10)
     else pure $ orderStatus # _payment_gateway_response' .~ (just $ mpgr6)
   else if gateway == show PAYU then do
    rrn         <- lookupRespXml' pgResponse "bank_ref_num" "null"
    authIdCode  <- lookupRespXml' pgResponse "field2" "NA"
    epgTxnId    <- lookupRespXml' pgResponse "mihpayid" "null"
    offer       <- lookupRespXml' pgResponse "offer" "NA"
    offerVal    <- lookupRespXml  pgResponse "offer_availed" "offer"
    offerType   <- lookupRespXml' pgResponse "offer_type" "null"
    offerFailure<- lookupRespXml' pgResponse "offer_failure_reason" "null"
    discount    <- lookupRespXml' pgResponse "discount" "null"
    offerAvailed<- pure $ find (\val -> ("offer_availed" == (fromMaybe "" (val.string !! 0)))) pgResponse
    offerAvailed' <- case offerAvailed of
       Just offer -> pure $ true
       Nothing -> pure $ false
    disAmount <- if (discount == "null") then pure $ nullValue unit else do
                    case (Number.fromString discount) of
                      Just am -> pure (toForeign am)
                      Nothing -> pure $ nullValue unit
    let mpgr1 = mpgr  # _txn_id .~ (pgr.txnId)
        mpgr2 = mpgr1 # _rrn .~ (just $ rrn)
        mpgr3 = mpgr2 # _epg_txn_id .~ (just $ epgTxnId)
        mpgr4 = mpgr3 # _auth_id_code .~ (just $ authIdCode)
        mpgr5 = mpgr4 # _resp_code .~ (just $ unNull pgr.respCode "null")
        mpgr6 = mpgr5 # _resp_message .~ (just $ unNull pgr.respMessage "null")
    if offer /= "NA" then do
      -- * Payu allows merchants to send multiple offers and avails a valid offer among them
      -- * offer_availed contains the successfully availed offer.
      let mpgr7  = mpgr6 # _offer .~ (just $ offerVal)
          mpgr8  = mpgr7 # _offer_type .~ (just $ offerType)
          mpgr9  = mpgr8 # _offer_availed .~ (just $ toForeign offerAvailed')
          mpgr10 = mpgr9 # _discount_amount .~ (just $ disAmount)
      if offerFailure /= "null" then do
        let mpgr11 = mpgr10 # _offer_failure_reason .~ (just $ offerFailure)
        pure $ orderStatus # _payment_gateway_response' .~ (just $ mpgr11)
       else pure $ orderStatus # _payment_gateway_response' .~ (just $ mpgr10)
     else pure $ orderStatus # _payment_gateway_response' .~ (just $ mpgr6)
   else if gateway == show FREECHARGE then do
    epgTxnId     <- lookupRespXml' pgResponse "txnId" "null"
    respCode     <- lookupRespXml  pgResponse "status" "errorCode"
    respMessage  <- lookupRespXml  pgResponse "errorMessage" "status"
    campaignCode <- lookupRespXml' pgResponse "campaignCode" "NA"
    let mpgr1 = mpgr  # _txn_id .~ (just $ txn ^._txnId)
        mpgr2 = mpgr1 # _rrn .~ (just $ "")
        mpgr3 = mpgr2 # _epg_txn_id .~ (just $ epgTxnId)
        mpgr4 = mpgr3 # _auth_id_code .~ (just $ "NA")
        mpgr5 = mpgr4 # _resp_code .~ (just $ respCode)
        mpgr6 = mpgr5 # _resp_message .~ (just $ respMessage)
    if campaignCode /= "NA" then do
       -- Freecharge doesn't return any information about offers in their response
      let mpgr7  = mpgr6 # _offer .~ (just $ campaignCode)
          mpgr8  = mpgr7 # _offer_type .~ (just $ "NA")
          mpgr9  = mpgr8 # _offer_availed .~ (just $ toForeign "NA")
          mpgr10 = mpgr9 # _discount_amount .~ (just $ nullValue unit)
      pure $ orderStatus # _payment_gateway_response' .~ (just $ mpgr10)
     else pure $ orderStatus # _payment_gateway_response' .~ (just $ mpgr6)
   else if gateway == show ZAAKPAY then do
    epgTxnId   <- lookupRespXml pgResponse "pgTransId" "txnId"
    respCode   <- lookupRespXml' pgResponse "responseCode" "null"
    respMessage<- lookupRespXml' pgResponse "responseDescription" "null"
    let mpgr1 = mpgr  # _txn_id .~ (just $ txn ^._txnId)
        mpgr2 = mpgr1 # _rrn .~ (just $ "")
        mpgr3 = mpgr2 # _epg_txn_id .~ (just $ epgTxnId)
        mpgr4 = mpgr3 # _auth_id_code .~ (just $ "NA")
        mpgr5 = mpgr4 # _resp_code .~ (just $ respCode)
        mpgr6 = mpgr5 # _resp_message .~ (just $ respMessage)
    pure $ orderStatus # _payment_gateway_response' .~ (just $ mpgr6)
   else if gateway == show STRIPE then do
    rrn        <- lookupRespXml' pgResponse "id" "null"
    epgTxnId   <- lookupRespXml' pgResponse "id" "null"
    respMessage<- lookupRespXml' pgResponse "failureMessage" "null"
    failureCode<- lookupRespXml' pgResponse "failureCode" "null"
    status     <- lookupRespXml' pgResponse "status" "null"
    respCode   <- if status == "failed" then pure $ failureCode
                   else pure $ status
    let mpgr1 = mpgr  # _txn_id .~ (just $ txn ^._txnId)
        mpgr2 = mpgr1 # _rrn .~ (just $ rrn)
        mpgr3 = mpgr2 # _epg_txn_id .~ (just $ epgTxnId)
        mpgr4 = mpgr3 # _auth_id_code .~ (just $ "NA")
        mpgr5 = mpgr4 # _resp_code .~ (just $ respCode)
        mpgr6 = mpgr5 # _resp_message .~ (just $ respMessage)
    pure $ orderStatus # _payment_gateway_response' .~ (just $ mpgr6)
   else if gateway == show CYBERSOURCE then do
    epgTxnId    <- lookupRespXml pgResponse "requestID" "RequestID"
    authIdCode' <- lookupRespXml pgResponse "ccAuthReply_authorizationCode" "AuthorizationCode"
    authIdCode  <- if (authIdCode' == "null") || (authIdCode' == "") then pure $ "NA" else pure authIdCode'
    let mpgr1 = mpgr  # _txn_id .~ (just $ txn ^._txnId)
        mpgr2 = mpgr1 # _rrn .~ (just $ "NA")
        mpgr3 = mpgr2 # _epg_txn_id .~ (just $ epgTxnId)
        mpgr4 = mpgr3 # _auth_id_code .~ (just $ authIdCode)
        mpgr5 = mpgr4 # _resp_code .~ pgr.respCode
        mpgr6 = mpgr5 # _resp_message .~ pgr.respMessage
    pure $ orderStatus # _payment_gateway_response' .~ (just $ mpgr6)
   else if gateway == show IPG then do
    epgTxnId         <- lookupRespXml' pgResponse "ipgTransactionId" "null"
    respCode'        <- lookupRespXml  pgResponse "fail_rc" "approval_code"
    respMessage'     <- lookupRespXml  pgResponse "fail_reason" "status"
    approvalCode     <- lookupRespXml' pgResponse "ApprovalCode" "null"
    transactionState <- lookupRespXml' pgResponse "TransactionState" "null"
    respCode         <- if (respCode' == "null") || (respCode' == "") then pure $ approvalCode else pure $ respCode'
    respMessage      <- if (respMessage' == "null") || (respMessage' == "") then pure $ transactionState else pure $ respMessage'
    let mpgr1 = mpgr  # _txn_id .~ (just $ txn ^._txnId)
        mpgr2 = mpgr1 # _rrn .~ (just $ "NA")
        mpgr3 = mpgr2 # _epg_txn_id .~ (just $ epgTxnId)
        mpgr4 = mpgr3 # _auth_id_code .~ (just $ "NA")
        mpgr5 = mpgr4 # _resp_code .~ (just $ respCode)
        mpgr6 = mpgr5 # _resp_message .~ (just $ respMessage)
    pure $ orderStatus # _payment_gateway_response' .~ (just $ mpgr6)
   else if gateway == show AXISNB then do
    epgTxnId <- lookupRespXml' pgResponse "BID" ""
    let mpgr1 = mpgr  # _txn_id .~ (just $ txn ^._txnId)
        mpgr2 = mpgr1 # _rrn .~ (just $ "NA")
        mpgr3 = mpgr2 # _epg_txn_id .~ (just $ epgTxnId)
        mpgr4 = mpgr3 # _auth_id_code .~ (just $ "NA")
        mpgr5 = mpgr4 # _resp_code .~ pgr.axisRespCode
        mpgr6 = mpgr5 # _resp_message .~ pgr.axisRespMessage
    pure $ orderStatus # _payment_gateway_response' .~ (just $ mpgr6)
   else if gateway == show ZESTMONEY then do
    let mpgr1 = mpgr  # _txn_id .~ (just $ txn ^._txnId)
        mpgr2 = mpgr1 # _rrn .~ (just $ "NA")
        mpgr3 = mpgr2 # _epg_txn_id .~ (just $ "NA")
        mpgr4 = mpgr3 # _auth_id_code .~ (just $ "NA")
        mpgr5 = mpgr4 # _resp_code .~ pgr.respCode
        mpgr6 = mpgr5 # _resp_message .~ pgr.respMessage
    pure $ orderStatus # _payment_gateway_response' .~ (just $ mpgr6)
   else if gateway == show MPESA then do
    epgTxnId   <- lookupRespXml pgResponse "mcompgtransid" "mcompgtransid"
    let mpgr1 = mpgr  # _txn_id .~ (just $ txn ^._txnId)
        mpgr2 = mpgr1 # _rrn .~ (just $ "NA")
        mpgr3 = mpgr2 # _epg_txn_id .~ (just $ epgTxnId)
        mpgr4 = mpgr3 # _auth_id_code .~ (just $ "NA")
        mpgr5 = mpgr4 # _resp_code .~ pgr.respCode
        mpgr6 = mpgr5 # _resp_message .~ pgr.respMessage
    pure $ orderStatus # _payment_gateway_response' .~ (just $ mpgr6)
   else if gateway == show SBIBUDDY then do
    epgTxnId <- lookupRespXml' pgResponse "transactionId" "NA"
    let mpgr1 = mpgr  # _txn_id .~ (just $ txn ^._txnId)
        mpgr2 = mpgr1 # _rrn .~ (just $ "NA")
        mpgr3 = mpgr2 # _epg_txn_id .~ (just $ epgTxnId)
        mpgr4 = mpgr3 # _auth_id_code .~ (just $ "NA")
        mpgr5 = mpgr4 # _resp_code .~ pgr.respCode
        mpgr6 = mpgr5 # _resp_message .~ pgr.respMessage
    pure $ orderStatus # _payment_gateway_response' .~ (just $ mpgr6)
   else if gateway == show BILLDESK then do
    rrn        <- lookupRespXml' pgResponse "TxnReferenceNo" "null"
    epgTxnId   <- lookupRespXml' pgResponse "TxnReferenceNo" "null"
    authIdCode <- lookupRespXml' pgResponse "BankReferenceNo" "NA"
    let mpgr1 = mpgr  # _txn_id .~ (just $ txn ^._txnId)
        mpgr2 = mpgr1 # _rrn .~ (just $ rrn)
        mpgr3 = mpgr2 # _epg_txn_id .~ (just $ epgTxnId)
        mpgr4 = mpgr3 # _auth_id_code .~ (just $ authIdCode)
        mpgr5 = mpgr4 # _resp_code .~ pgr.respCode
        mpgr6 = mpgr5 # _resp_message .~ pgr.respMessage
    pure $ orderStatus # _payment_gateway_response' .~ (just $ mpgr6)
   else if gateway == show KOTAK then do
    rrn        <- lookupRespXml' pgResponse "RetRefNo" "NA"
    epgTxnId   <- lookupRespXml' pgResponse "RetRefNo" "NA"
    authIdCode <- lookupRespXml' pgResponse "AuthCode" "NA"
    let mpgr1 = mpgr  # _txn_id .~ (just $ txn ^._txnId)
        mpgr2 = mpgr1 # _rrn .~ (just $ rrn)
        mpgr3 = mpgr2 # _epg_txn_id .~ (just $ epgTxnId)
        mpgr4 = mpgr3 # _auth_id_code .~ (just $ authIdCode)
        mpgr5 = mpgr4 # _resp_code .~ pgr.respCode
        mpgr6 = mpgr5 # _resp_message .~ pgr.respMessage
    pure $ orderStatus # _payment_gateway_response' .~ (just $ mpgr6)
   else if gateway == show JIOMONEY then do
    epgTxnId <- lookupRespXml' pgResponse "jm_tran_ref_no" "NA"
    let mpgr1 = mpgr  # _txn_id .~ (just $ txn ^._txnId)
        mpgr2 = mpgr1 # _rrn .~ (just $ "NA")
        mpgr3 = mpgr2 # _epg_txn_id .~ (just $ epgTxnId)
        mpgr4 = mpgr3 # _auth_id_code .~ (just $ "NA")
        mpgr5 = mpgr4 # _resp_code .~ pgr.respCode
        mpgr6 = mpgr5 # _resp_message .~ pgr.respMessage
    pure $ orderStatus # _payment_gateway_response' .~ (just $ mpgr6)
   else if gateway == show AMAZONPAY then do
    epgTxnId <- lookupRespXml pgResponse "amazonOrderId" "transactionId"
    let mpgr1 = mpgr  # _txn_id .~ (just $ txn ^._txnId)
        mpgr2 = mpgr1 # _rrn .~ (just $ "NA")
        mpgr3 = mpgr2 # _epg_txn_id .~ (just $ if epgTxnId == "null" then "NA" else epgTxnId)
        mpgr4 = mpgr3 # _auth_id_code .~ (just $ "NA")
        mpgr5 = mpgr4 # _resp_code .~ pgr.respCode
        mpgr6 = mpgr5 # _resp_message .~ pgr.respMessage
    pure $ orderStatus # _payment_gateway_response' .~ (just $ mpgr6)
   else if gateway == show AIRTELMONEY then do
    epgTxnId <- lookupRespXml pgResponse "TRAN_ID" "FDC_TXN_ID"
    let mpgr1 = mpgr  # _txn_id .~ (just $ onlyAlphaNumeric $ txn ^._txnId)
        mpgr2 = mpgr1 # _rrn .~ (just $ "NA")
        mpgr3 = mpgr2 # _epg_txn_id .~ (just $ epgTxnId)
        mpgr4 = mpgr3 # _auth_id_code .~ (just $ "NA")
        mpgr5 = mpgr4 # _resp_code .~ pgr.respCode
        mpgr6 = mpgr5 # _resp_message .~ pgr.respMessage
    pure $ orderStatus # _payment_gateway_response' .~ (just $ mpgr6)
   else if gateway == show PHONEPE then do
    epgVal   <- hierarchyObjectLookup pgrXml "data" "providerReferenceId"
    epgTxnId <- lookupRespXml' pgResponse "providerReferenceId" epgVal
    let mpgr1 = mpgr  # _txn_id .~ (just $ txn ^._txnId)
        mpgr2 = mpgr1 # _rrn .~ (just $ "NA")
        mpgr3 = mpgr2 # _epg_txn_id .~ (just $ if (epgTxnId == "") then "null" else epgTxnId)
        mpgr4 = mpgr3 # _auth_id_code .~ (just $ "NA")
        mpgr5 = mpgr4 # _resp_code .~ pgr.respCode
        mpgr6 = mpgr5 # _resp_message .~ pgr.respMessage
    pure $ orderStatus # _payment_gateway_response' .~ (just $ mpgr6)
   else if gateway == show EPAYLATER then do
    epgVal   <- hierarchyObjectLookup pgrXml "payload" "eplOrderId"
    epgTxnId <- lookupRespXml pgResponse "eplOrderId" "id"
    let mpgr1 = mpgr  # _txn_id .~ (just $ txn ^._txnId)
        mpgr2 = mpgr1 # _rrn .~ (just $ "NA")
        mpgr3 = mpgr2 # _epg_txn_id .~ (just $ if epgVal == "" then epgTxnId else epgVal)
        mpgr4 = mpgr3 # _auth_id_code .~ (just $ "NA")
        mpgr5 = mpgr4 # _resp_code .~ pgr.respCode
        mpgr6 = mpgr5 # _resp_message .~ pgr.respMessage
    pure $ orderStatus # _payment_gateway_response' .~ (just $ mpgr6)
   else if gateway == show SODEXO then do
    epgTxnId <- lookupRespXml' pgResponse "transactionId" "null"
    rrn      <- lookupRespXml' pgResponse "retrievalReferenceNumber" "null"
    let mpgr1 = mpgr  # _txn_id .~ (just $ txn ^._txnId)
        mpgr2 = mpgr1 # _rrn .~ (just $ rrn)
        mpgr3 = mpgr2 # _epg_txn_id .~ (just $ epgTxnId)
        mpgr4 = mpgr3 # _auth_id_code .~ (just $ "NA")
        mpgr5 = mpgr4 # _resp_code .~ pgr.respCode
        mpgr6 = mpgr5 # _resp_message .~ pgr.respMessage
    pure $ orderStatus # _payment_gateway_response' .~ (just $ mpgr6)
   else if gateway == show FSS_ATM_PIN || gateway == show FSS_ATM_PIN_V2 then do
    rrn           <- lookupRespXml' pgResponse "ref" "null"
    epgTxnId      <- lookupRespXml' pgResponse "tranid" "null"
    authIdCode    <- lookupRespXml' pgResponse "auth" "null"
    rrn'          <- tempLookup pgrXml "ref" "null"
    epgTxnId'     <- tempLookup pgrXml "tranid" "null"
    authIdCode'   <- tempLookup pgrXml "auth" "null"
    rrnVal        <- if rrn == "null" then pure rrn' else pure rrn
    epgTxnIdVal   <- if epgTxnId == "null" then pure epgTxnId' else pure epgTxnId
    authIdCodeVal <- if authIdCode == "null" then pure authIdCode' else pure authIdCode
    let mpgr1 = mpgr  # _txn_id .~ (just $ txn ^._txnId)
        mpgr2 = mpgr1 # _rrn .~ (just $ if (isNotString rrnVal) then "null" else rrnVal)
        mpgr3 = mpgr2 # _epg_txn_id .~ (just $ if (isNotString epgTxnIdVal) then "null" else epgTxnIdVal)
        mpgr4 = mpgr3 # _auth_id_code .~ (just $ if (isNotString authIdCodeVal) then "null" else authIdCodeVal)
        mpgr5 = mpgr4 # _resp_code .~ pgr.respCode
        mpgr6 = mpgr5 # _resp_message .~ pgr.respMessage
    pure $ orderStatus # _payment_gateway_response' .~ (just $ mpgr6)
   else if gateway == show LINEPAY then do
    v <- hierarchyObjectLookup pgrXml "info" "transactionId"
    let mpgr1 = mpgr  # _txn_id .~ (just $ txn ^._txnId)
        mpgr2 = mpgr1 # _rrn .~ (just $ v)
        mpgr3 = mpgr2 # _epg_txn_id .~ (just $ v)
        mpgr4 = mpgr3 # _auth_id_code .~ (just $ "NA")
        mpgr5 = mpgr4 # _resp_code .~ pgr.respCode
        mpgr6 = mpgr5 # _resp_message .~ pgr.respMessage
    pure $ orderStatus # _payment_gateway_response' .~ (just $ mpgr6)
   else if gateway == show FSSPAY then do
    epgTxnId <- lookupRespXml' pgResponse "tranid" ""
    authId <- lookupRespXml' pgResponse "auth" ""
    let mpgr1 = mpgr  # _txn_id .~ (just $ txn ^._txnId)
        mpgr2 = mpgr1 # _rrn .~ (just $ "")
        mpgr3 = mpgr2 # _epg_txn_id .~ (just $ epgTxnId)
        mpgr4 = mpgr3 # _auth_id_code .~ (just $ authId)
        mpgr5 = mpgr4 # _resp_code .~ pgr.respCode
        mpgr6 = mpgr5 # _resp_message .~ pgr.respMessage
    pure $ orderStatus # _payment_gateway_response' .~ (just $ mpgr6)
   else if gateway == show CITI then do
      citiToMall   <- lookupRespXml' pgResponse "CitiToMall" "null"
      result       <- pure $ S.split (S.Pattern "|") citiToMall
      decisionCode <- pure $ fromMaybe "" (result !! 6)
      authCode     <- pure $ fromMaybe "" (result !! 7)
      let mpgr1 = mpgr  # _txn_id .~ (just $ txn ^._txnId)
          mpgr2 = mpgr1 # _rrn .~ (just $ "")
          mpgr3 = mpgr2 # _epg_txn_id .~ (just $ "")
          mpgr4 = mpgr3 # _auth_id_code .~ (just $ authCode)
          mpgr5 = mpgr4 # _resp_code .~ (just $ decisionCode)
          mpgr6 = mpgr5 # _resp_message .~ (just $ decisionCodeToMessageMap decisionCode)
      pure $ orderStatus # _payment_gateway_response' .~ (just $ mpgr6)
   else if gateway == show OLAPOSTPAID then do
      epgTxnId <- lookupRespXml' pgResponse "transactionId" "null"
      let mpgr1 = mpgr  # _txn_id .~ (just $ txn ^._txnId)
          mpgr2 = mpgr1 # _rrn .~ (just $ "")
          mpgr3 = mpgr2 # _epg_txn_id .~ (just $ epgTxnId)
          mpgr4 = mpgr3 # _auth_id_code .~ (just $ "NA")
          mpgr5 = mpgr4 # _resp_code .~ pgr.respCode
          mpgr6 = mpgr5 # _resp_message .~ pgr.respMessage
      pure $ orderStatus # _payment_gateway_response' .~ (just $ mpgr6)
   else if gateway == show CASH then do
      let mpgr1 = mpgr  # _txn_id .~ (just $ txn ^._txnId)
          mpgr2 = mpgr1 # _rrn .~ (just $ "NA")
          mpgr3 = mpgr2 # _epg_txn_id .~ (just $ "NA")
          mpgr4 = mpgr3 # _auth_id_code .~ (just $ "NA")
          mpgr5 = mpgr4 # _resp_code .~ pgr.respCode
          mpgr6 = mpgr5 # _resp_message .~ pgr.respMessage
      pure $ orderStatus # _payment_gateway_response' .~ (just $ mpgr6)
   else if gateway == show AXIS_UPI then do
      txnId    <- getAxisUpiTxnIdFromPgr txn pgResponse
      rrn      <- lookupRespXml' pgResponse "custRef" "null"
      epgTxnId <- getAxisUpiRequestIdFromPgr txn pgResponse
      let mpgr1 = mpgr  # _txn_id .~ (just $ txnId)
          mpgr2 = mpgr1 # _rrn .~ (just $ rrn)
          mpgr3 = mpgr2 # _epg_txn_id .~ (just $ epgTxnId)
          mpgr4 = mpgr3 # _auth_id_code .~ (just $ "NA")
          mpgr5 = mpgr4 # _resp_code .~ pgr.respCode
          mpgr6 = mpgr5 # _resp_message .~ pgr.respMessage
      pure $ orderStatus # _payment_gateway_response' .~ (just $ mpgr6)
   else do
      gateway <- pure $ find (\val -> (gateway == show val)) (eulerUpiGateways <> [GOOGLEPAY])
      case gateway of
        Just gateway -> do
          txnId    <- lookupRespXml' pgResponse "transactionRef" (txn ^._txnId)
          rrn      <- lookupRespXml' pgResponse "custRef" "null"
          epgTxnId <- lookupRespXml' pgResponse "upiRequestId" "null"
          let mpgr1 = mpgr  # _txn_id .~ (just $ txnId)
              mpgr2 = mpgr1 # _rrn .~ (just $ rrn)
              mpgr3 = mpgr2 # _epg_txn_id .~ (just $ epgTxnId)
              mpgr4 = mpgr3 # _auth_id_code .~ (just $ "NA")
              mpgr5 = mpgr4 # _resp_code .~ pgr.respCode
              mpgr6 = mpgr5 # _resp_message .~ pgr.respMessage
          pure $ orderStatus # _payment_gateway_response' .~ (just $ mpgr6)
        Nothing -> do
          rrn        <- lookupRespXml pgResponse "rrn" "RRN"
          authIdCode <- lookupRespXml pgResponse "authIdCode" "AuthIdCode"
          epgTxnId   <- lookupRespXml pgResponse "epgTxnId" "EpgTxnId"
          let mpgr1 = mpgr  # _txn_id .~ (pgr.txnId)
              mpgr2 = mpgr1 # _rrn .~ (just $ rrn)
              mpgr3 = mpgr2 # _epg_txn_id .~ (just $ epgTxnId)
              mpgr4 = mpgr3 # _auth_id_code .~ (just $ authIdCode)
              mpgr5 = mpgr4 # _resp_code .~ pgr.respCode
              mpgr6 = mpgr5 # _resp_message .~ pgr.respMessage
          pure $ orderStatus # _payment_gateway_response' .~ (just $ mpgr6)

versionSpecificTransforms :: forall st r. Newtype st { orderId :: Maybe String, merchantId :: Maybe String, isDBMeshEnabled :: Maybe Boolean, isMemCacheEnabled :: Boolean | r } => RouteParameters -> OrderStatusResponse -> BackendFlow st _ OrderStatusResponse
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

getRefundStatus :: forall st r. Newtype st { orderId :: Maybe String, merchantId :: Maybe String, isDBMeshEnabled :: Maybe Boolean, isMemCacheEnabled :: Boolean | r } => String  -> Refund' -> BackendFlow st _ Refund'
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
postOrderStatus :: BackendFlow OrderLocalState Configs Foreign
postOrderStatus = proxyOrderStatus POST

postOrderCreate :: BackendFlow OrderLocalState Configs Foreign
postOrderCreate = proxyOrderCreate POST

getOrderStatus :: BackendFlow OrderLocalState Configs Foreign
getOrderStatus = proxyOrderStatus GET

proxyOrderStatus :: Method -> BackendFlow OrderLocalState Configs Foreign
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


proxyOrderCreate :: Method -> BackendFlow OrderLocalState Configs Foreign
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

getTxnStatusResponse :: forall st r. Newtype st { orderId :: Maybe String, merchantId :: Maybe String, isDBMeshEnabled :: Maybe Boolean, isMemCacheEnabled :: Boolean | r } => TxnDetail -> MerchantAccount -> BackendFlow st _ TxnStatusResponse
getTxnStatusResponse txnDetail@(TxnDetail txn) merchantAccount = do
    ordStatusResponse <- addPaymentMethodInfo merchantAccount txnDetail def
                          >>= addRefundDetails txnDetail
                          >>= addGatewayResponse txnDetail
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
getTokenExpiryData :: BackendFlow OrderLocalState _ OrderTokenExpiryData
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


-- lookupRespXml' :: forall st r rt. Newtype st { orderId :: Maybe String, merchantId :: Maybe String, isDBMeshEnabled :: Maybe Boolean, isMemCacheEnabled :: Boolean | r } =>(Array _) -> String -> String -> BackendFlow st rt String

getAxisUpiTxnIdFromPgr :: forall st r. Newtype st { orderId :: Maybe String, merchantId :: Maybe String, isDBMeshEnabled :: Maybe Boolean, isMemCacheEnabled :: Boolean | r } =>
      TxnDetail -> (Array _)  -> BackendFlow st _ String
getAxisUpiTxnIdFromPgr txn pgResponse = do
  merchantRequestId <- lookupRespXml' pgResponse "merchantRequestId" ""
  if merchantRequestId == "" then do
    transactionRef <- lookupRespXml' pgResponse "transactionRef" ""
    if transactionRef == "" then pure transactionRef else pure (txn ^. _txnId)
    else pure merchantRequestId

getAxisUpiRequestIdFromPgr :: forall st r. Newtype st { orderId :: Maybe String, merchantId :: Maybe String, isDBMeshEnabled :: Maybe Boolean, isMemCacheEnabled :: Boolean | r } =>
      TxnDetail -> (Array _) -> BackendFlow st _ String
getAxisUpiRequestIdFromPgr txn pgResponse = do

-}
