module Euler.Product.OLTP.Order.Create where

import EulerHS.Prelude hiding (id, show, get)
import qualified EulerHS.Prelude as EHP
import EulerHS.Language
import Euler.Lens
import WebService.Language

import           Data.Generics.Product.Fields
import           Data.Generics.Product.Subtype
import           Data.List (lookup, span)
import           Servant.Server

import Euler.Common.Types.DefaultDate
import Euler.Common.Types.Gateway
import Euler.Common.Types.Order
import Euler.Common.Types.OrderMetadata
import Euler.Product.Domain.Order
import Euler.Product.OLTP.Order.OrderStatus (getOrderStatusRequest, getOrderStatusWithoutAuth)
import Euler.Product.OLTP.Services.RedisService

-- EHS: Storage interaction should be extracted from here into separate modules
import Euler.Storage.Types.Customer
import Euler.Storage.Types.Mandate
import Euler.Storage.Types.MerchantAccount
import Euler.Storage.Types.MerchantIframePreferences
import Euler.Storage.Types.OrderAddress
import Euler.Storage.Types.OrderMetadataV2
import Euler.Storage.Types.OrderReference
import Euler.Storage.Types.ResellerAccount

import Euler.KVDB.Redis (rGet, setCacheWithExpiry)
import Euler.Storage.Types.EulerDB

import qualified Prelude              as P (show, notElem)
import qualified Data.Aeson           as A
import qualified Data.Text            as Text
import qualified Text.Read            as TR

-- EHS: Should not depend on API
import qualified Euler.API.RouteParameters  as RP

import qualified Euler.Common.Types.Mandate as M
import qualified Euler.Common.Types.Order   as C
import qualified Euler.Common.Metric        as Metric
import qualified Euler.Config.Config        as Config
import qualified Euler.Config.ServiceConfiguration as SC
import qualified Euler.Storage.Types.OrderAddress as Address (id)

import Euler.Storage.DBConfig
import qualified Database.Beam as B
import qualified Database.Beam.Backend.SQL as B
import Database.Beam ((==.), (&&.), (||.), (<-.), (/=.))

orderCreate :: Order -> RP.RouteParameters -> MerchantAccount -> Flow  OrderCreateResponse
orderCreate Order {..} routeParams mAccnt = do

  -- EHS: HTTP error codes should exist only on the server level, not in business logic.
  merchantId' <- maybe (throwException err500 {errBody = "1"}) pure (mAccnt ^. _merchantId)
-- * state update. This functionality repeated in another methods.
-- state update with order_id and merchantId
--
-- * end
  (orderIfExists :: Maybe OrderReference) <- do
    conn <- getConn eulerDB
    res <- runDB conn $ do
      let predicate OrderReference {orderId, merchantId} =
            (orderId    ==. B.just_ (B.val_ $ orderCreateReq ^. _order_id)) &&.
            (merchantId ==. B.just_ (B.val_ merchantId'))

      findRow
        $ B.select
        $ B.limit_ 1
        $ B.filter_ predicate
        $ B.all_ (order_reference eulerDBSchema)
    case res of
      Right mordRef -> pure mordRef
    --   Right Nothing -> throwException err404 {errBody = "Order " <> show orderId' <> " not found."}
      Left err -> do
        logError "Find OrderReference" $ toText $ P.show err
        throwException err500 {errBody = "2"}
  --pure $ Nothing --findOne "ECRDB" $
  --    where_ := WHERE ["order_id" /\ String orderId, "merchant_id" /\ String merchantId]
  resp <- case (orderIfExists) of
            (Just orderRef) -> do --this is only create function
              throwException err400 {errBody = "Order already created."}
    --          _ <- updateOrder updatedOrderCreateReq order routeParams mAccnt
    --          -- *             ^ return OrderReference
    --          getOrderStatusWithoutAuth (getOrderStatusRequest orderId) routeParams mAccnt true (Just updatedOrderCreateReq) (Just $ order)
    --          -- * ^ return Foreign (encoded OrderStatusResponse), also may change fields from snake case to camel case in checkEnableCaseForResponse
            Nothing -> createOrder orderCreateReq routeParams mAccnt
       -- * ^ return Foreign (encoded OrderAPIResponse) -- encoded to Foreign because
                                                        -- original function can return different types
  --  log "createOrUpdateOrder response: " resp
  pure resp

mkUdf :: OrderCreateRequest -> UDF
mkUdf orderCreateReq@OrderCreateRequest {..} =
  if orderUDF == emptyUDF
    then emptyUDF
    else udf
      where
        orderUDF = upcast orderCreateReq
        udf = UDF
              { -- * why udf 1-5 are cleaned and others are not?
                udf1 =  cleanUp udf1
              , udf2 =  cleanUp udf2
              , udf3 =  cleanUp udf3
              , udf4 =  cleanUp udf4
              , udf5 =  cleanUp udf5
              , udf6 =  udf6
              , udf7 =  udf7
              , udf8 =  udf8
              , udf9 =  udf9
              , udf10 = udf10
              }

-- CREATE

createOrder :: OrderCreateRequest -> RP.RouteParameters -> MerchantAccount -> Flow OrderCreateResponse -- OrderAPIResponse
createOrder orderCreateReq routeParams mAccnt@MerchantAccount{..} = do
  merchantId <- maybe (throwException err500 {errBody = "4"}) pure merchantId
  mandateOrder <- isMandateOrder orderCreateReq routeParams merchantId -- mAccnt
  orderRef <- createOrder' orderCreateReq routeParams mAccnt mandateOrder
  orderIdPrimary <- getOrderId orderRef
  let orderStatus = getField @"status" orderRef
      maybeResellerId = resellerId --  mAccnt
  maybeResellerEndpoint <- maybe (pure Nothing) handleReseller maybeResellerId
  orderResponse <- makeOrderResponse orderRef maybeResellerEndpoint
 -- _ <- logAnomaly orderRef
  let version = RP.lookupRP @RP.Version routeParams
  if version >= Just "2018-07-01" &&
     (options_get_client_auth_token orderCreateReq == Just True)
    then do
      orderTokenData  <- addOrderToken orderIdPrimary orderCreateReq merchantId
      pure $ updateResponse orderRef orderResponse orderTokenData
    else pure orderResponse

updateResponse :: OrderReference -> OrderCreateResponse -> Maybe OrderTokenResp -> OrderCreateResponse
updateResponse OrderReference{..} apiResp orderTokenResp = do
  (apiResp :: OrderCreateResponse)
    { status = NEW
    , status_id = orderStatusToInt NEW
    , juspay = orderTokenResp
    , udf1 =  udf1 <|> Just ""
    , udf2 =  udf2 <|> Just ""
    , udf3 =  udf3 <|> Just ""
    , udf4 =  udf4 <|> Just ""
    , udf5 =  udf5 <|> Just ""
    , udf6 =  udf6 <|> Just ""
    , udf7 =  udf7 <|> Just ""
    , udf8 =  udf8 <|> Just ""
    , udf9 =  udf9 <|> Just ""
    , udf10 =  udf10 <|> Just ""
    , return_url =  returnUrl <|> Just ""
    , refunded = refundedEntirely <|> Just False
    , product_id =  productId <|> Just ""
    , merchant_id =  merchantId <|> Just ""
    , date_created = Just $ dateCreated
    , customer_phone =  customerPhone <|> Just ""
    , customer_id =  customerId <|> Just ""
    , customer_email =  customerEmail <|> Just ""
    , currency =  currency <|> Just ""
    , amount_refunded = amountRefunded <|> Just 0.0
    , amount = amount <|> Just 0.0
    }

addOrderToken :: Int -> OrderCreateRequest -> Text -> Flow  (Maybe OrderTokenResp)
addOrderToken orderId OrderCreateRequest{..} merchantId = do
  case options_get_client_auth_token of
    Just True -> do
      TokenizedResource{token, expiry} <- tokenizeResource (SC.ResourceInt orderId) "ORDER" merchantId

      runIO $ Metric.incrementClientAuthTokenGeneratedCount merchantId

      pure $ Just $ OrderTokenResp
                      { client_auth_token = Just $ token
                      , client_auth_token_expiry = Just $ expiry
                      }
    Just _ -> pure $ Just $ OrderTokenResp {
                                  client_auth_token = Nothing
                                , client_auth_token_expiry = Nothing
                                }
    Nothing -> pure Nothing

isMandateOrder :: OrderCreateRequest -> RP.RouteParameters -> Text {- MerchantAccount -} -> Flow Bool
isMandateOrder orderCreateReq routeParams merchantId {-mAccnt-} = do
 -- merchantId <- maybe (liftErr internalError) pure (unNullOrUndefined <<< _.merchantId <<< unwrap $ mAccnt)
  mandateFeature <- pure $ fromMaybe DISABLED (orderCreateReq ^. _options_create_mandate)
  if mandateFeature == DISABLED
    then pure False
    else do
      validMandate <- isMandateValid orderCreateReq merchantId -- never returns False
      if validMandate
        then pure True
        else throwException $ err400 {errBody = "invalid mandate params"} -- throwCustomException 400 "INVALID_REQUEST" "invalid mandate params"

isMandateValid :: OrderCreateRequest -> Text -> Flow Bool
isMandateValid oReq@OrderCreateRequest {..} merchantId = if isTrueString $ customer_id -- orderCreateReq
  then do
    let maybeMaxAmount = mandate_max_amount -- unNullOrUndefined <<< _.mandate_max_amount <<< unwrap $ orderCreateReq
    case maybeMaxAmount of
      Nothing -> throwException $ err400 {errBody = "Pass mandate_max_amount also to create a mandate order"} -- throwCustomException 400 "INVALID_REQUEST" "Pass mandate_max_amount also to create a mandate order"
      Just maxAmount -> do
        mayMaxAmLimit <- rGet $ merchantId <> "_mandate_max_amount_limit" -- pure Nothing -- fetchFromRedisWithMaybe ecRedis (merchantId <> "_mandate_max_amount_limit")
        maxAmLim <- fromStringToNumber $ case mayMaxAmLimit of
          Just val -> val
          Nothing -> Config.mandateMaxAmountAllowed -- why Text/String not Double?
        maxAmountNum  <- fromStringToNumber maxAmount
        if (maxAmountNum <= 0.0 || maxAmountNum > maxAmLim)
          then throwException $  err400 {errBody = "invalid Mandate Max Amount"} -- throwCustomException 400 "INVALID_MANDATE_MAX_AMOUNT" (invalidMandateMaxAmount <> show maxAm)
          else pure True
  else
    throwException $  err400 {errBody = "pass customer_id"} -- throwCustomException 400 "INVALID_CUSTOMER_ID" "pass customer_id"

-- from src/Utils/Utils.purs
fromStringToNumber :: Text -> Flow Double
fromStringToNumber str = do
  num <- pure $ readMayT str -- Number.fromString str
  case num of
    Just x -> pure $ x
    Nothing -> throwException $  err400 {errBody = "Invalid amount"} -- defaultThrowECException "INVALID_REQUEST" "Invalid amount"

readMayT :: Read a => Text -> Maybe a
readMayT = TR.readMaybe . Text.unpack

    -- from src/Engineering/Commons.purs
-- In groovy empty string is `falsy` value
isTrueString :: Maybe Text -> Bool
isTrueString val =
  case val of
    Just "" -> False
    Just " " -> False
    Just _ -> True
    Nothing -> False

createOrder' ::  OrderCreateRequest -> RP.RouteParameters -> MerchantAccount -> Bool -> Flow OrderReference
createOrder' orderCreateReq routeParams mAccnt@MerchantAccount{..} isMandate = do
  merchantId' <- maybe (throwException err500 {errBody = "5"}) pure (merchantId)
  merchantPrefs <- do
    conn <- getConn eulerDB
    res <- runDB conn $ do
      let predicate MerchantIframePreferences {merchantId} = merchantId ==. (B.val_ merchantId')
      findRow
        $ B.select
        $ B.limit_ 1
        $ B.filter_ predicate
        $ B.all_ (merchant_iframe_preferences eulerDBSchema)
    case res of
      Right (Just mIP) -> pure mIP
      Right Nothing -> do
        logError "merchant_iframe_preferences" $ "Not found for merchant " <> merchantId'
        throwException err500 {errBody = "6"}
      Left err -> do
        logError "SQLDB Interraction." $ toText $ P.show err
        throwException err500 {errBody = "7"}
  --pure defaultMerchantIframePreferences -- findOneWithErr ecDB (where_ := WHERE ["merchantId'" /\ String merchantId]) internalError
  billingAddrId <- getBillingAddrId orderCreateReq mAccnt
  shippingAddrId <- getShippingAddrId orderCreateReq
  orderRefVal <- mkOrder mAccnt merchantPrefs orderCreateReq billingAddrId shippingAddrId
  -- Need sql [insert/update/delete] methods that return values
  orderRef :: OrderReference <- do
    mOref <- withDB eulerDB $ do
      insertRowsReturningList -- insertRows
        $ B.insert (order_reference eulerDBSchema)
        $ B.insertExpressions [(B.val_ orderRefVal) & _id .~ B.default_]
     -- findRow
     --   $ B.select
     --   $ B.filter_ (\ref -> ref ^. _orderId ==. (B.val_ $ orderRefVal ^. _orderId))
     --   $ B.all_ (order_reference eulerDBSchema)
    --maybe (throwException err500 {errBody = "8"}) pure mOref -- orderRefVal
    case mOref of
      [ordRef] -> pure ordRef
      x -> throwException err500 {errBody = EHP.show x}
    --pure orderRefVal -- createWithErr ecDB orderRefVal internalError
  orderIdPrimary <- getOrderId orderRef
  -- TODO: FlipKart alone uses useragent and IP address. Lets remove for others
  defaultMetaData <- mkMetadata routeParams orderCreateReq orderIdPrimary
  _ <- do
    conn <- getConn eulerDB
    runDB conn $ do
      insertRows
        $ B.insert (order_metadata_v2 eulerDBSchema)
        $ B.insertExpressions [ B.val_ defaultMetaData]
  -- pure () -- createWithErr ecDB (defaultMetaData) internalError
  orderId <- maybe (throwException err500 {errBody = "9"}) pure (getField @"orderId"  orderRef)
  _ <- setCacheWithExpiry (merchantId' <> "_orderid_" <> orderId) orderRef Config.orderTtl
  _ <- when isMandate (setMandateInCache orderCreateReq orderIdPrimary orderId merchantId')
  --skipIfB (setMandateInCache orderCreateReq orderIdPrimary orderId merchantId') (not isMandate)
  pure orderRef

getBillingAddrId :: OrderCreateRequest -> MerchantAccount -> Flow  (Maybe Int)
getBillingAddrId orderCreateReq mAccnt = do
  orderReq <- addCustomerInfoToRequest orderCreateReq mAccnt
  if (present orderReq)
  then do
    let newAddr = mkBillingAddress $ upcast orderReq
    mAddr <- withDB eulerDB $ do
      insertRowsReturningList -- insertRows
        $ B.insert (order_address eulerDBSchema)
        $ B.insertExpressions [ (B.val_ newAddr)  & _id .~ B.default_]
      -- findRow $ B.select -- what if insert fail?
      --   $ B.limit_ 1
      --   $ B.orderBy_ (B.desc_ . Address.id ) -- B.aggregate_ (\addr -> B.max_ (addr ^. _id ))
      --   $ B.all_ (order_address eulerDBSchema)
   -- Need sql [insert/update] methods that return values

    pure $   (^. _id) =<< (safeHead mAddr) -- getField @"id" mAddr -- <$> createWithErr ecDB (mkBillingAddress orderReq) internalError
  else pure Nothing
  where present (OrderCreateRequest {..}) = isJust
          $   billing_address_first_name
          <|> billing_address_last_name
          <|> billing_address_line1
          <|> billing_address_line2
          <|> billing_address_line3
          <|> billing_address_city
          <|> billing_address_state
          <|> billing_address_country
          <|> billing_address_postal_code
          <|> billing_address_phone
          <|> billing_address_country_code_iso

getShippingAddrId ::  OrderCreateRequest -> Flow (Maybe Int)
getShippingAddrId orderCreateReq@OrderCreateRequest{..} =
  if (present orderCreateReq)
  then do
    let newAddr = mkShippingAddress $ upcast orderCreateReq
    mAddr <- withDB eulerDB $ do
      insertRowsReturningList -- insertRows
        $ B.insert (order_address eulerDBSchema)
        $ B.insertExpressions [ (B.val_ newAddr)  & _id .~ B.default_]
      --findRow $ B.select -- what if insert fail?
      --  $ B.limit_ 1
      --  $ B.orderBy_ (B.desc_ . Address.id ) -- B.aggregate_ (\addr -> B.max_ (addr ^. _id ))
      --  $ B.all_ (order_address eulerDBSchema)
     -- Need sql [insert/update/delete] methods that return values
    case mAddr of
      [] -> throwException err500 {errBody = "emptyaddr"}
      x -> runIO $ putStrLn $ P.show x
    pure $ (^. _id) =<< (safeHead mAddr) -- (getField @"id") <$> createWithErr ecDB (mkShippingAddress orderCreateReq) internalError
  else pure Nothing
  where present (OrderCreateRequest {..}) = isJust
          $    shipping_address_first_name
          <|>  shipping_address_last_name
          <|>  shipping_address_line1
          <|>  shipping_address_line2
          <|>  shipping_address_line3
          <|>  shipping_address_city
          <|>  shipping_address_state
          <|>  shipping_address_country
          <|>  shipping_address_postal_code
          <|>  shipping_address_phone
          <|>  shipping_address_country_code_iso

-- mkBillingAddress :: OrderCreateRequest -> OrderAddress
-- mkBillingAddress OrderCreateRequest {..} = OrderAddress
--   { id             = Nothing -- in DB it Not Null, Auto increment
--   , version        = 1 -- defaultVersion
-- -- from src/Config/Constants.purs
-- --  defaultVersion :: Int
-- --  defaultVersion = 1
--   , firstName      = billing_address_first_name
--   , lastName       = billing_address_last_name
--   , line1          = billing_address_line1
--   , line2          = billing_address_line2
--   , line3          = billing_address_line3
--   , city           = billing_address_city
--   , state          = billing_address_state
--   , country        = billing_address_country
--   , postalCode     = billing_address_postal_code
--   , phone          = billing_address_phone
--   , countryCodeIso = billing_address_country_code_iso
--   }
-- add firstName and lastName
addCustomerInfoToRequest :: OrderCreateRequest -> MerchantAccount -> Flow  OrderCreateRequest
addCustomerInfoToRequest orderCreateReq mAccnt = do
  if (shouldAddCustomerNames orderCreateReq) then do
    maybeCustomer :: Maybe Customer <- withDB eulerDB $ do
      let predicate Customer {merchantAccountId, id, objectReferenceId}
            = (   id ==.  (B.val_ $  orderCreateReq ^. _customer_id)
              ||. objectReferenceId ==. (B.val_ $  orderCreateReq ^. _customer_id)
              )
            &&. ( B.maybe_ (B.val_ False) (merchantAccountId ==. ) ( B.as_ @(Maybe Int) $ B.val_  $ mAccnt ^. _id))
      findRow
        $ B.select
        $ B.filter_ predicate
        $ B.all_ (customer eulerDBSchema)
      --Customer.findMaybeByMerchantAccountAndCustId (orderCreateReq .^. _customer_id) (mAccnt .^. _id)
    case maybeCustomer of
      Just (Customer {..}) ->
       -- let  orderReq = (unwrap orderCreateReq)
       -- in pure $ OrderCreateReq orderReq
          let OrderCreateRequest {..} = orderCreateReq
          in
            pure (orderCreateReq :: OrderCreateRequest)
                   { billing_address_first_name = (billing_address_first_name <|> firstName)
                   , billing_address_last_name = (billing_address_last_name <|> lastName)
                   }
      Nothing -> pure orderCreateReq
  else pure orderCreateReq

  where shouldAddCustomerNames OrderCreateRequest {..} =
           (isJust customer_id) -- isPresent = isJust
              && ((isNothing billing_address_first_name)  -- isAbsent = isNothing
                 || (isNothing billing_address_last_name))

-- from src/Types/Storage/EC/Customer.purs
findMaybeByMerchantAccountAndCustId :: Text -> Int -> Flow (Maybe Customer)
findMaybeByMerchantAccountAndCustId custId mId = pure Nothing -- (DB.findOne ecDB $ where_ := And ["merchant_account_id" ==? Int mId, Or ["id" ==? String custId, "object_reference_id" ==? String custId]] :: Where Customer)

mkOrder :: MerchantAccount -> MerchantIframePreferences -> OrderCreateRequest -> Maybe Int -> Maybe Int -> Flow OrderReference
mkOrder account prefs req@OrderCreateRequest{..} billingAddrId shippingAddrId = do
  orderUuid      <- ("ordeu_" <> ) <$> getUUID32 -- pure "someUUID32" -- getUUID32 -- returns UUID with deleted '-' . From src/Utils/PrestoBackend.js
  currentDate    <- getCurrentTimeUTC --getCurrentDate -- UTC from src/Engineering/DB/Types.js
  gatewayName    <- getGatewayName $ gateway_id -- req
  autoRefund     <- pure $ maybe (Just False) Just auto_refund -- req -- !* not used?
  mandateFeature <- pure $ Just DISABLED -- just (nullOrUndefinedToAny req."options.create_mandate" DISABLED)
  orderType      <- pure $ getOrderType (fromMaybe DISABLED mandateFeature) -- nullOrUndefinedToAny = flip fromMaybe
  mId            <- maybe (throwException err500 {errBody = "10"}) pure (getField @"id" account)
  maybeCustomer  <- maybe (pure Nothing) (\x -> findMaybeByMerchantAccountAndCustId x mId) customer_id
  customerId     <- case (isJust maybeCustomer, orderType == MANDATE_REGISTER) of
                      (False,True) -> throwException $ err500 {errBody = "Customer not found"} -- defaultThrowECException "INVALID_REQUEST" "Customer not found"
                      _            -> pure customer_id
      -- * roundOff2 looks like
      -- * foo x = (\(a,b) -> read @Double $ a <> b) $ bimap id (take 3) $ span (/='.') $ show x
  roundedAmount         <- pure $ testRoundOff2 amount -- roundOff2 $ req.amount
  let orderRef = defaultOrderReference
          { orderId = Just order_id
          , merchantId = getField @"merchantId" account
          , amount = Just roundedAmount
          , billingAddressId  = billingAddrId
          , shippingAddressId = shippingAddrId
          , currency = currency'
          , customerId = customerId
          , customerEmail = customer_email
          , customerPhone = customer_phone
          , returnUrl = returnUrl'
          , description = description'
          , orderUuid = Just orderUuid
          , productId = product_id
          , preferredGateway = gatewayName
          , status = NEW
          , dateCreated = currentDate
          , lastModified = currentDate
          , orderType = Just orderType
          , mandateFeature = mandateFeature
          }
  addCustomerInfo req account (mapUDFParams req orderRef)
  where currency'              =  ( currency) <|> (defaultCurrency prefs) <|> Just "INR"
        returnUrl'             =  (( return_url) <|> Nothing)
        description'           =  (( description) <|> Just "")
        getOrderType mandate  = if mandate == REQUIRED then MANDATE_REGISTER else ORDER_PAYMENT

testRoundOff2 x = (\(a,b) -> read @Double $ a <> b) $ bimap EHP.id (take 3) $ span (/='.') $ P.show x

cleanUp :: Maybe Text -> Maybe Text
cleanUp mStr =  cleanUpSpecialChars <$>  mStr

cleanUpSpecialChars :: Text -> Text
cleanUpSpecialChars = Text.filter (`P.notElem` ("~!#%^=+\\|:;,\"'()-.&/" :: String))
-- from src/Euler/Utils/Utils.js
-- exports.cleanUpSpecialChars = function(val){
--   return val.replace(/[\\\\~!#%^=+\\|:;,\"'()-.&/]/g,"");
-- }

-- mapUDFParams ::   { from }  -> {  to   }-> {    res     }
mapUDFParams :: OrderCreateRequest -> OrderReference -> OrderReference
mapUDFParams req params =  smash newUDF params
  where
    (reqUDF :: UDF) = upcast req
    newUDF' UDF {..} = UDF
      { udf1 = cleanUp $ udf1
      , udf2 = cleanUp $ udf2
      , udf3 = cleanUp $ udf3
      , udf4 = cleanUp $ udf4
      , udf5 = cleanUp $ udf5
      , udf6 = udf6
      , udf7 = udf7
      , udf8 = udf8
      , udf9 = udf9
      , udf10 = udf10
      }
    newUDF = newUDF' reqUDF
  -- * Why are udf fields 1-5 cleaned and the rest not?



getGatewayName :: Maybe Text -> Flow (Maybe Text)
getGatewayName gatewayId = pure $ toText . P.show <$> (fromVal =<< gatewayId)
 --  case gatewayId of
 --    Just id -> case (fromVal id) of
 --      Just gatewayName -> pure $ Just (show gatewayName)
 --      Nothing          -> pure $ Nothing
 --    Nothing -> pure $ nothing


-- from src/Product/Gateway/Utils.purs
fromVal :: Text -> Maybe Gateway
fromVal = (flip lookup $ swap <$> gatewayMap) <=< readMayT
 -- * fromString :: String -> Maybe Int
 -- case fromString strVal of
 --   Just val -> lookup val $ map swap gatewayMap
 --   Nothing -> Nothing

addCustomerInfo :: OrderCreateRequest -> MerchantAccount -> OrderReference -> Flow OrderReference
addCustomerInfo orderCreateReq account orderRef =
  if hasPartialCustomerInfo
  then do -- customer_id :: Maybe Text
    customerId <- maybe (throwException err500 {errBody = "11"}) pure (getField @"customer_id" orderCreateReq)
    accountId  <- maybe (throwException err500 {errBody = "12"}) pure (getField @"id" account)
    -- one more condiotion in DB query is missing!
    optCustomer <- do
      conn <- getConn eulerDB
      res <- runDB conn $ do
        let predicate Customer {objectReferenceId, merchantAccountId, id} =
              merchantAccountId ==. (B.val_ accountId) &&.
              (objectReferenceId ==. B.just_ (B.val_  customerId) ||. id ==. B.just_ (B.val_ customerId) )
        findRow
          $ B.select
          $ B.limit_ 1
          $ B.filter_ predicate
          $ B.all_ (customer eulerDBSchema)
      case res of
        Right mCust -> pure mCust
        Left err -> do
          logError "SQLDB Interraction." $ toText $ P.show err
          throwException err500 {errBody = "13"}

    -- pure Nothing -- findOne ecDB (where_ :=
    --    And ["merchant_account_id" ==? Int accountId
    --  , Or  [ "object_reference_id" ==? String customerId, "id" ==? String customerId ]])
    case optCustomer of
      (Just (Customer {..})) ->
       -- let orderVal = (unwrap orderRef)
           pure $ (orderRef :: OrderReference) --OrderReference orderVal
            { customerPhone =
                 (customerPhone orderRef) <|> (Just mobileNumber)
            , customerEmail =
                 (customerEmail orderRef) <|> emailAddress
            }
      _ -> pure orderRef
  else pure orderRef
  where hasPartialCustomerInfo =
          let OrderCreateRequest{..} = orderCreateReq
          in (isJust customer_id )
              && ((isNothing customer_phone)
                || (isNothing customer_email ))

-- there we need sourceIP and userAgent from ReaderT config
mkMetadata :: RP.RouteParameters -> OrderCreateRequest -> Int -> Flow OrderMetadataV2
mkMetadata routeParams req orderId = do
  let sourceAddress' = RP.lookupRP @RP.SourceIP routeParams
  let userAgent' = RP.lookupRP @RP.UserAgent routeParams
 -- config <- ask
  currentDate <- getCurrentTimeUTC
  pure $ OrderMetadataV2
    { id = Nothing
    , browser = Nothing
    , browserVersion = Nothing
    , device = Nothing
    , ipAddress = sourceAddress'
    , metadata = metaData req
    , mobile = Just False
    , operatingSystem = Nothing
    , orderReferenceId = orderId
    , referer = Nothing
    , userAgent = userAgent'
    , dateCreated = currentDate
    , lastUpdated = currentDate
    }

validateOrderParams :: OrderCreateRequest -> Flow ()
validateOrderParams OrderCreateRequest {..} = if amount < 1
  then throwException $ err400 {errBody = "Invalid amount"}
  else pure ()

getOrderId :: OrderReference -> Flow Int
getOrderId OrderReference{..} = case id of
    Just x -> pure x
    _ -> throwException err500 {errBody = "NO ORDER FOUND"} -- defaultThrowECException "NO_ORDER_FOUND" "NO ORDER FOUND"


setMandateInCache :: OrderCreateRequest -> Int -> Text -> Text -> Flow ()
setMandateInCache req@OrderCreateRequest{..} orderIdPrimary orderId merchantId = do
  maxAmount  <- extractMandatory mandate_max_amount
  maxAmountNum  <- fromStringToNumber maxAmount
  defaultMandateData <- mkMandatedata merchantId req orderIdPrimary maxAmountNum
  _ <- setCacheWithExpiry (merchantId <> "_mandate_data_" <> orderId)
      defaultMandateData Config.mandateTtl
  pure ()

-- from src/Utils/Utils.purs
extractMandatory :: Maybe a -> Flow a
--extractMandatory obj = extractMandatory' $ unNullOrUndefined obj
extractMandatory = maybe (throwException $ err500 {errBody = "Mandatory object not found"}) pure

-- extractMandatory' :: forall rt localState b. Maybe b -> BackendFlow localState rt b
-- extractMandatory' obj = case (obj) of
--                         Just val -> pure val
--                         Nothing -> defaultThrowECException "OBJECT_NOT_FOUND" "OBJECT NOT FOUND"

mkMandatedata :: Text -> OrderCreateRequest -> Int -> Double -> Flow Mandate
mkMandatedata merchantId OrderCreateRequest{..} orderId maxAmount = do
  mandateId <- getShortUUID
  currentDate <- getCurrentTimeUTC
  token <- getUUID32
  pure $ Mandate {
    id = Nothing
  , merchantId = merchantId
  , currency = currency
  , endDate = Nothing
  , startDate = Nothing
  , maxAmount = Just maxAmount
  , merchantCustomerId = customer_id
  , paymentMethod = Nothing
  , paymentMethodType = Nothing
  , paymentMethodId = Nothing
  , gateway = Nothing
  , gatewayParams = Nothing
  , token = token
  , mandateId = mandateId
  , status = M.CREATED
  , authOrderId = Just orderId
  , activatedAt = Nothing
  , dateCreated = currentDate
  , lastModified = currentDate
  , authTxnCardInfo = Nothing
  , merchantGatewayAccountId = Nothing
  , metadata = Nothing
  }

handleReseller ::  Text -> Flow (Maybe Text)
handleReseller resellerId' = do
 -- resellerAccount :: ResellerAccount <- do
    conn <- getConn eulerDB
    res <- runDB conn $ do
      let predicate ResellerAccount {resellerId} = resellerId ==. B.val_ resellerId'
      findRow
        $ B.select
        $ B.limit_ 1
        $ B.filter_ predicate
        $ B.all_ (reseller_account eulerDBSchema)
    case res of
      Right mRAcc -> pure $ (^. _resellerApiEndpoint) =<< mRAcc
      Left err -> do
        logError "Find ResellerAccount" $ toText $ P.show err
        throwException err500 {errBody = "14"}
  -- pure defaultResellerAccount -- findOneWithErr ecDB (where_ := WHERE ["resellerId" /\ String resellerId]) internalError
 -- pure $ resellerApiEndpoint resellerAccount


makeOrderResponse :: OrderReference -> Maybe Text -> Flow OrderCreateResponse -- OrderAPIResponse
makeOrderResponse orderRef@OrderReference{..} maybeResellerEndpoint = do
  cfg <- runIO Config.getECRConfig
  let url = maybe (cfg ^. _protocol <> "://" <>  cfg ^. _host) EHP.id maybeResellerEndpoint
  pure $ (defaultOrderCreateResponse :: OrderCreateResponse) --OrderCreateResp $ (unwrap defaultOrderAPIResponse)  {
    { status = CREATED
    , status_id = orderStatusToInt CREATED
    , id = orderUuid'
    , order_id = fromMaybe "" orderId -- nullOrUndefinedToStr orderRef.orderId
    , payment_links= Paymentlinks
        { web    = Just (url <> "/merchant/pay/") <> Just orderUuid'
        , mobile = Just (url <> "/merchant/pay/") <> Just orderUuid' <> Just "?mobile=true"
        , iframe = Just (url <> "/merchant/ipay/") <> Just orderUuid'
        }
    }
  where
   -- Config {..} = defaultConfig -- Config.getECRConfig -- from (src/Config/Config.purs) looks like constant, but depend on ENV
   -- protocol = (unwrap config).protocol
   -- host = (unwrap config).host
   -- url = maybe ( cfg ^. _protocol <> "://" <>  cfg ^. _host) EHP.id maybeResellerEndpoint
    orderUuid' = fromMaybe "" orderUuid -- nullOrUndefinedToStr orderRef.orderUuid
