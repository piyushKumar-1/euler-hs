module Euler.Product.OLTP.Order.CreateUpdate where


import EulerHS.Prelude hiding (id, show, get)
import EulerHS.Language
import Euler.Lens
import WebService.Language


import qualified EulerHS.Prelude as P (id)

import           Data.Generics.Product.Fields
import           Data.Generics.Product.Subtype
import           Data.List (lookup, span)
import           Servant.Server


import Euler.API.Order
import Euler.Common.Types.DefaultDate
import Euler.Common.Types.Gateway
import Euler.Common.Types.Order
import Euler.Product.Domain.OrderMetadataV2
import Euler.Product.OLTP.Order.OrderStatus (getOrderStatusWithoutAuth)
import Euler.Product.OLTP.Services.RedisService

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

orderCreate :: OrderCreateRequest -> RP.RouteParameters -> MerchantAccount -> Flow  OrderCreateResponse
orderCreate orderCreateReq@OrderCreateRequest {..} routeParams mAccnt = do
  _           <- validateOrderParams orderCreateReq
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

orderUpdate :: Text -> OrderCreateRequest -> Text {- RouteParameters -} -> MerchantAccount -> Flow OrderStatusResponse
orderUpdate orderId orderCreateReq@OrderCreateRequest {..} routeParams mAccnt = do
  _           <- validateOrderParams orderCreateReq
  merchantId  <- maybe (throwException err500 {errBody = "3"}) pure (getField @"merchantId" mAccnt)
-- * state update. This functionality repeated in another methods.
-- state update with order_id and merchantId
  let updatedOrderCreateReq = (orderCreateReq :: OrderCreateRequest) {order_id = orderId}
-- * end
  (orderIfExists :: Maybe OrderReference) <- pure $ Just defaultOrderReference --findOne "ECRDB" $
  --    where_ := WHERE ["order_id" /\ String orderId, "merchant_id" /\ String merchantId]
  resp <- case (orderIfExists) of
            (Just orderRef) -> do --this is only create function
              updateOrder orderCreateReq orderRef "routParams" mAccnt
    --          _ <- updateOrder updatedOrderCreateReq order routeParams mAccnt
    --          -- *             ^ return OrderReference
              getOrderStatusWithoutAuth (getOrderStatusRequest orderId) routeParams mAccnt True (Just updatedOrderCreateReq) (Just orderRef)
    --          -- * ^ return Foreign (encoded OrderStatusResponse), also may change fields from snake case to camel case in checkEnableCaseForResponse
            Nothing -> throwException err400 {errBody = "Order does not exists."}
       -- * ^ return Foreign (encoded OrderAPIResponse) -- encoded to Foreign because
                                                        -- original function can return different types
  --  log "createOrUpdateOrder response: " resp
  pure resp

-- UPDATE

updateOrder :: OrderCreateRequest -> OrderReference -> {-RouteParameters-} Text -> MerchantAccount -> Flow OrderReference -- OrderStatusResponse --Order
updateOrder orderCreateReq order@OrderReference {..} routeParams mAccnt = do
  case status of
    C.SUCCESS -> do
      logError "not_updating_successful_order" $ Text.pack("Order: " <> P.show ( orderId) <> " has already succeeded. Not updating any field.")
      pure order
    _ ->  do
      -- update shipping address
      let sa = mkShippingAddress $ upcast orderCreateReq
      shippingAddressId' <- if isAddressEmpty sa
        then pure Nothing
        else
          case (shippingAddressId) of
            Nothing -> getShippingAddrId orderCreateReq
            Just saId -> do
              _ <- updateOrderAddress saId sa
              pure Nothing

      -- update billing address
      let ba = mkBillingAddress $ upcast orderCreateReq
      billingAddressId' <- if isAddressEmpty ba
        then pure Nothing
        else
          case (billingAddressId) of
          Nothing -> getBillingAddrId orderCreateReq mAccnt
          Just baId -> do
            _ <- updateOrderAddress baId ba
            pure Nothing

      -- update amount, udf and last modified

      -- * roundOff2 looks like
      -- * foo x = (\(a,b) -> read @Double $ a <> b) $ bimap id (take 3) $ span (/='.') $ show x
      -- * with logging incoming and outcoming values
      newAmount <- pure $ testRoundOff2 $ getField @"amount" orderCreateReq
      let udf = mkUdf orderCreateReq
      currentDate <- pure defaultDate -- getCurrentDate
      updateOrderRefAndInValidateCache (smash udf $ order
        { amount = Just newAmount
        , billingAddressId = billingAddressId'
        , shippingAddressId = shippingAddressId'
        , lastModified = currentDate
      })

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

-- from src/Types/Storage/EC/OrderReference.purs
updateOrderRefAndInValidateCache :: OrderReference -> Flow OrderReference
updateOrderRefAndInValidateCache order = do
 -- order <- DB.updateOne' ecDB order $ where_ := WHERE ["id" /\ Int (order .^. _id)] :: WHERE OrderReference
 -- _     <- RedisService.invalidateOrderStatusCache (order .^. _orderId) (order .^. _merchantId)
  pure order

-- from src/Types/Storage/EC/OrderAddress.purs
updateOrderAddress :: Int -> OrderAddress -> Flow OrderAddress
updateOrderAddress addrId orderAddress = do --pure orderAddress --do
  withDB eulerDB $ do
    updateRows
    $ B.save (order_address eulerDBSchema)
    $ orderAddress & _id .~ (Just addrId)
  pure orderAddress
--  let opts = getOrderAddressOpts orderAddress
--  (update "ECRDB" opts $ where_ := WHERE ["id" /\ Int id] :: WHERE OrderAddress) >>= extractUpdateOneObj

--getOrderAddressOpts :: OrderAddress -> Options OrderAddress
--getOrderAddressOpts (OrderAddress orderAddress) =
--  ((maybe mempty (assoc firstName) (unNullOrUndefined orderAddress.firstName))
--  <> (maybe mempty (assoc lastName) (unNullOrUndefined orderAddress.lastName))
--  <> (maybe mempty (assoc line1) (unNullOrUndefined orderAddress.line1))
--  <> (maybe mempty (assoc line2) (unNullOrUndefined orderAddress.line2))
--  <> (maybe mempty (assoc line3) (unNullOrUndefined orderAddress.line3))
--  <> (maybe mempty (assoc city) (unNullOrUndefined orderAddress.city))
--  <> (maybe mempty (assoc state) (unNullOrUndefined orderAddress.state))
--  <> (maybe mempty (assoc country) (unNullOrUndefined orderAddress.country))
--  <> (maybe mempty (assoc postalCode) (unNullOrUndefined orderAddress.postalCode))
--  <> (maybe mempty (assoc phone) (unNullOrUndefined orderAddress.phone))
--  <> (maybe mempty (assoc countryCodeIso) (unNullOrUndefined orderAddress.countryCodeIso)))

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
        mayMaxAmLimit <- L.rGet $ merchantId <> "_mandate_max_amount_limit" -- pure Nothing -- fetchFromRedisWithMaybe ecRedis (merchantId <> "_mandate_max_amount_limit")
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
      insertRows
        $ B.insert (order_reference eulerDBSchema)
        $ B.insertValues [orderRefVal]
      findRow
        $ B.select
        $ B.filter_ (\ref -> ref ^. _orderId ==. (B.val_ $ orderRefVal ^. _orderId))
        $ B.all_ (order_reference eulerDBSchema)
    maybe (throwException err500 {errBody = "8"}) pure mOref -- orderRefVal
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
      insertRows
        $ B.insert (order_address eulerDBSchema)
        $ B.insertExpressions [ (B.val_ newAddr)  & _id .~ B.default_]
      findRow $ B.select -- what if insert fail?
        $ B.limit_ 1
        $ B.orderBy_ (B.desc_ . Address.id ) -- B.aggregate_ (\addr -> B.max_ (addr ^. _id ))
        $ B.all_ (order_address eulerDBSchema)
   -- Need sql [insert/update] methods that return values

    pure $   (^. _id) =<< mAddr -- getField @"id" mAddr -- <$> createWithErr ecDB (mkBillingAddress orderReq) internalError
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
      insertRows
        $ B.insert (order_address eulerDBSchema)
        $ B.insertExpressions [ (B.val_ newAddr)  & _id .~ B.default_]
      findRow $ B.select -- what if insert fail?
        $ B.limit_ 1
        $ B.orderBy_ (B.desc_ . Address.id ) -- B.aggregate_ (\addr -> B.max_ (addr ^. _id ))
        $ B.all_ (order_address eulerDBSchema)
     -- Need sql [insert/update/delete] methods that return values
    pure $ (^. _id) =<< mAddr -- (getField @"id") <$> createWithErr ecDB (mkShippingAddress orderCreateReq) internalError
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

testRoundOff2 x = (\(a,b) -> read @Double $ a <> b) $ bimap P.id (take 3) $ span (/='.') $ P.show x

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
  let url = maybe (cfg ^. _protocol <> "://" <>  cfg ^. _host) P.id maybeResellerEndpoint
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
   -- url = maybe ( cfg ^. _protocol <> "://" <>  cfg ^. _host) P.id maybeResellerEndpoint
    orderUuid' = fromMaybe "" orderUuid -- nullOrUndefinedToStr orderRef.orderUuid


-- data Config = Config
--   { protocol :: Text
--   , host :: Text
--   , internalECHost :: Text
--   }
--   deriving (Show, Read, Eq, Ord, Generic)
-- defaultConfig = Config
--     { protocol = "https"
--     , host = "defaulthost"
--     , internalECHost = "defaultInternalECHost"
--     }
-- #########<<<<<
-- #########<<<<<
-- #########<<<<<
-- #########<<<<<
-- #########<<<<<
-- #########<<<<<
-- #########<<<<<
-- getOrderId :: OrderReference -> BackendFlow _ _ Int
-- getOrderId (OrderReference orderReference) = case orderReference.id of
--                                                 NullOrUndefined (Just x ) -> pure x
--                                                 _ -> defaultThrowECException "NO_ORDER_FOUND" "NO ORDER FOUND"

-- foreign import data APIResponse :: Type

-- fetchOrderStatus :: MerchantAccount -> OrderCreateReq -> BackendFlow _ _ OrderStatusResponseBody
-- fetchOrderStatus mAccnt orderCreateReq = do
--   apiKey <- maybe (liftErr internalError) pure (unNullOrUndefined <<< _.apiKey <<< unwrap $ mAccnt)
--   config <- ask
--   reqHeaders <- either (defaultThrowECException "Error Getting Headers") pure config.headers
--   resp <- callAPI reqHeaders orderCreateReq
--   either ((defaultThrowECException "Error in Fetch Status Response") <<< show) pure resp


-- createOrder' :: forall r st. Newtype st { orderId :: Maybe String, merchantId :: Maybe String, isDBMeshEnabled :: Maybe Boolean, isMemCacheEnabled :: Boolean | r } => OrderCreateReq -> RouteParameters -> MerchantAccount -> Boolean -> BackendFlow st _ OrderReference
-- createOrder' orderCreateReq routeParams mAccnt isMandate = do
--   merchantId <- maybe (liftErr internalError) pure (unNullOrUndefined <<< _.merchantId <<< unwrap $ mAccnt)
--   merchantPrefs <- findOneWithErr ecDB (where_ := WHERE ["merchantId" /\ String merchantId]) internalError
--   billingAddrId <- getBillingAddrId orderCreateReq mAccnt
--   shippingAddrId <- getShippingAddrId orderCreateReq
--   orderRefVal <- mkOrder mAccnt merchantPrefs orderCreateReq billingAddrId shippingAddrId
--   orderRef :: OrderReference <- createWithErr ecDB orderRefVal internalError
--   orderIdPrimary <- getOrderId orderRef
--   -- TODO: FlipKart alone uses useragent and IP address. Lets remove for others
--   defaultMetaData <- mkMetadata orderCreateReq orderIdPrimary
--   _ <- createWithErr ecDB (defaultMetaData) internalError
--   orderId <- maybe (liftErr internalError) pure (unNullOrUndefined <<< _.orderId <<< unwrap $ orderRef)
--   _ <- setCacheWithExpiry ecRedis (merchantId <> "_orderid_" <> orderId) (encodeJSON orderRef) Config.orderTtl
--   _ <- skipIfB (setMandateInCache orderCreateReq orderIdPrimary orderId merchantId) (not isMandate)
--   pure orderRef
--
-- isMandateOrder :: OrderCreateReq -> RouteParameters -> MerchantAccount -> BackendFlow _ _ Boolean
-- isMandateOrder orderCreateReq routeParams mAccnt = do
--   merchantId <- maybe (liftErr internalError) pure (unNullOrUndefined <<< _.merchantId <<< unwrap $ mAccnt)
--   mandateFeature <- pure $ (nullOrUndefinedToAny (unwrap orderCreateReq)."options.create_mandate" DISABLED)
--   if mandateFeature == DISABLED
--     then pure false
--     else do
--       validMandate <- isMandateValid orderCreateReq merchantId
--       if validMandate
--         then pure true
--         else throwCustomException 400 "INVALID_REQUEST" "invalid mandate params"
--
-- createOrder :: OrderCreateReq -> RouteParameters -> MerchantAccount -> BackendFlow OrderLocalState _ OrderAPIResponse
-- createOrder orderCreateReq@(OrderCreateReq orderCreateReqVal) routeParams mAccnt = do
--   merchantId <- maybe (liftErr internalError) pure (unNullOrUndefined <<< _.merchantId <<< unwrap $ mAccnt)
--   mandateOrder <- isMandateOrder orderCreateReq routeParams mAccnt
--   orderRef <- createOrder' orderCreateReq routeParams mAccnt mandateOrder
--   orderIdPrimary <- getOrderId orderRef
--   let orderStatus = _.status <<< unwrap $ orderRef
--       maybeResellerId = unNullOrUndefined <<< _.resellerId <<< unwrap $ mAccnt
--   maybeResellerEndpoint <- maybe (pure Nothing) handleReseller maybeResellerId
--   orderResponse <- pure $ makeOrderResponse orderRef maybeResellerEndpoint
--   _ <- logAnomaly orderRef
--   let version = lookup "version" routeParams
--   if (version >= Just "2018-07-01") &&
--      (orderCreateReqVal."options.get_client_auth_token" == just true)
--     then do
--       orderTokenData  <- addOrderToken orderIdPrimary orderCreateReq merchantId
--       pure $ updateResponse orderRef orderResponse orderTokenData
--     else pure orderResponse
--
-- -- * why we need this log?
-- logAnomaly :: forall st rt. OrderReference -> BackendFlow st _ Unit
-- logAnomaly ord = do
--   config <- ask
--   void $ doAffRR' "logAnomaly" (do
--     liftAff $ rawLogRunner "anomaly detector" $ encode (anomalyLog config)
--     pure UnitEx
--     )
--     where anomalyLog config = AnomalyLog {
--                       status          : nothing
--                     , merchant_id     : ord ^. _merchantId
--                     , log_version     : just $ anomalyLogVersion
--                     , txn_uuid        : nothing
--                     , gateway         : nothing
--                     , app_version     : appVersion
--                     , service         : service
--                     , status_code     : "200"
--                     , order_id        : ord ^. _orderId
--                     , order_type      : maybe nothing (just <<< show) (unNullOrUndefined (ord ^. _orderType))
--                     , payment_method  : nothing
--                     , payment_method_type : nothing
--                     , card_switch_provider : nothing
--                     , card_type       : nothing
--                     , auth_type       : nothing
--                     , is_emi          : nothing
--                     , source_object   : nothing
--                     , card_issuer_bank_name : nothing
--                     , route           : config.url }
--
-- addOrderToken :: forall st r. Newtype st { orderId :: Maybe String, merchantId :: Maybe String, isDBMeshEnabled :: Maybe Boolean, isMemCacheEnabled :: Boolean | r } => Int -> OrderCreateReq -> String -> BackendFlow st _ (NullOrUndefined OrderTokenResp)
-- addOrderToken orderId (OrderCreateReq orderCreateReq) merchantId = do
--   case (unNullOrUndefined orderCreateReq."options.get_client_auth_token") of
--     Just true -> do
--       {token,expiry} <- RedisService.tokenizeResource (ResourceInt orderId) "ORDER" merchantId
--       incrementClientAuthTokenGeneratedCount
--       pure $ just $ OrderTokenResp {
--               client_auth_token : just $ token
--             , client_auth_token_expiry : just $ expiry
--             }
--     Just _ -> pure $ just $ OrderTokenResp {
--                                   client_auth_token : nothing
--                                 , client_auth_token_expiry : nothing
--                                 }
--     Nothing -> pure $ nothing
--
-- isMandateValid :: OrderCreateReq -> String -> BackendFlow _ _ Boolean
-- isMandateValid orderCreateReq merchantId = if isTrueString $ orderCreateReq ^. _customer_id
--   then do
--     let maybeMaxAmount = unNullOrUndefined <<< _.mandate_max_amount <<< unwrap $ orderCreateReq
--     case maybeMaxAmount of
--       Nothing -> throwCustomException 400 "INVALID_REQUEST" "Pass mandate_max_amount also to create a mandate order"
--       Just maxAmount -> do
--         maxAmLimit <- fetchFromRedisWithMaybe ecRedis (merchantId <> "_mandate_max_amount_limit")
--         maxAm <- fromStringToNumber $ case maxAmLimit of
--           Just val -> val
--           Nothing -> Config.mandateMaxAmountAllowed
--         maxAmountNum  <- fromStringToNumber maxAmount
--         if (maxAmountNum <= 0.0 || maxAmountNum > maxAm)
--           then throwCustomException 400 "INVALID_MANDATE_MAX_AMOUNT" (invalidMandateMaxAmount <> show maxAm)
--           else pure true
--   else
--     throwCustomException 400 "INVALID_CUSTOMER_ID" "pass customer_id"
--
-- setMandateInCache :: OrderCreateReq -> Int -> String -> String -> BackendFlow _ _ Unit
-- setMandateInCache orderCreateReq orderIdPrimary orderId merchantId = do
--   maxAmount  <- extractMandatory (_.mandate_max_amount <<< unwrap $ orderCreateReq)
--   maxAmountNum  <- fromStringToNumber maxAmount
--   defaultMandateData <- mkMandatedata merchantId orderCreateReq orderIdPrimary maxAmountNum
--   _ <- setCacheWithExpiry ecRedis (merchantId <> "_mandate_data_" <> orderId)
--       (encodeJSON defaultMandateData) Config.mandateTtl
--   pure unit
--
--
-- handleReseller :: forall r st. Newtype st { orderId :: Maybe String, merchantId :: Maybe String, isDBMeshEnabled :: Maybe Boolean, isMemCacheEnabled :: Boolean | r } => String -> BackendFlow st _ (Maybe String)
-- handleReseller resellerId = do
--   resellerAccount :: ResellerAccount <- findOneWithErr ecDB (where_ := WHERE ["resellerId" /\ String resellerId]) internalError
--   pure <<< unNullOrUndefined <<< _.resellerApiEndpoint <<< unwrap $ resellerAccount
--
-- getBillingAddrId :: forall r st. Newtype st { orderId :: Maybe String, merchantId :: Maybe String, isDBMeshEnabled :: Maybe Boolean, isMemCacheEnabled :: Boolean | r } => OrderCreateReq -> MerchantAccount -> BackendFlow st _ (NullOrUndefined Int)
-- getBillingAddrId orderCreateReq mAccnt = do
--   orderReq <- addCustomerInfoToRequest orderCreateReq mAccnt
--   if (present orderReq) then (_.id <<< unwrap) <$> createWithErr ecDB (mkBillingAddress orderReq) internalError
--    else pure nothing
--   where present (OrderCreateReq req) = isJust $
--           (unNullOrUndefined req.billing_address_first_name)
--           <|> (unNullOrUndefined req.billing_address_last_name)
--           <|> (unNullOrUndefined req.billing_address_line1)
--           <|> (unNullOrUndefined req.billing_address_line2)
--           <|> (unNullOrUndefined req.billing_address_line3)
--           <|> (unNullOrUndefined req.billing_address_city)
--           <|> (unNullOrUndefined req.billing_address_state)
--           <|> (unNullOrUndefined req.billing_address_country)
--           <|> (unNullOrUndefined req.billing_address_postal_code)
--           <|> (unNullOrUndefined req.billing_address_phone)
--           <|> (unNullOrUndefined req.billing_address_country_code_iso)
--
-- addCustomerInfoToRequest :: forall r st. Newtype st { orderId :: Maybe String, merchantId :: Maybe String, isDBMeshEnabled :: Maybe Boolean, isMemCacheEnabled :: Boolean | r } => OrderCreateReq -> MerchantAccount -> BackendFlow st _ OrderCreateReq
-- addCustomerInfoToRequest orderCreateReq mAccnt = do
--   if (shouldAddCustomerNames) then do
--           maybeCustomer <- Customer.findMaybeByMerchantAccountAndCustId (orderCreateReq .^. _customer_id) (mAccnt .^. _id)
--           case maybeCustomer of
--             Just (Customer cust) ->
--               let orderReq = (unwrap orderCreateReq)
--               in pure $ OrderCreateReq orderReq
--                   { billing_address_first_name =
--                       NullOrUndefined $ (unNullOrUndefined orderReq.billing_address_first_name) <|> (unNullOrUndefined cust.firstName)
--                   , billing_address_last_name =
--                       NullOrUndefined $ (unNullOrUndefined orderReq.billing_address_last_name) <|> (unNullOrUndefined cust.lastName)
--                   }
--             Nothing -> pure orderCreateReq
--     else pure orderCreateReq
--
--   where shouldAddCustomerNames =
--            (isPresent (orderCreateReq ^. _customer_id))
--               && ((isAbsent (orderCreateReq ^. _billing_address_first_name))
--                 || (isAbsent (orderCreateReq ^. _billing_address_last_name)))
--
-- getShippingAddrId :: forall r st. Newtype st { orderId :: Maybe String, merchantId :: Maybe String, isDBMeshEnabled :: Maybe Boolean, isMemCacheEnabled :: Boolean | r } => OrderCreateReq -> BackendFlow st _ (NullOrUndefined Int)
-- getShippingAddrId orderCreateReq =
--   if (present orderCreateReq)
--   then (_.id <<< unwrap) <$> createWithErr ecDB (mkShippingAddress orderCreateReq) internalError
--   else (pure $ NullOrUndefined Nothing)
--   where present (OrderCreateReq req) = isJust $
--           (unNullOrUndefined req.shipping_address_first_name)
--           <|> (unNullOrUndefined req.shipping_address_last_name)
--           <|> (unNullOrUndefined req.shipping_address_line1)
--           <|> (unNullOrUndefined req.shipping_address_line2)
--           <|> (unNullOrUndefined req.shipping_address_line3)
--           <|> (unNullOrUndefined req.shipping_address_city)
--           <|> (unNullOrUndefined req.shipping_address_state)
--           <|> (unNullOrUndefined req.shipping_address_country)
--           <|> (unNullOrUndefined req.shipping_address_postal_code)
--           <|> (unNullOrUndefined req.shipping_address_phone)
--           <|> (unNullOrUndefined req.shipping_address_country_code_iso)
--
-- {-
-- for creation order_id lay in post body in OrderCreateReq,
-- for update - in path params (another info in post body)
-- so, for creation order_id required in post body
-- but for update not present in post body but required in path.
--
-- For update the API returns a response type that is the same as that of /order/status call.
-- So returning types are dufferent.
-- Can we separate create and update to two methods?
--
-- Also Only amount, address and udf fields can be updated,
-- so another fields not present in update request
--  -}
--
-- createOrUpdateOrder :: forall a. OrderCreateReq -> RouteParameters -> MerchantAccount -> BackendFlow _ _ Foreign
-- createOrUpdateOrder orderCreateReq routeParams mAccnt = do
--   _           <- validateOrderParams orderCreateReq -- validate only amount
--   -- * trying to get order_id from post body & path
--   let reqOrderId = _.order_id <<< unwrap $ orderCreateReq -- get orderId from post body
--   _ <- setTransactionTracersInLogs (unNullOrUndefined reqOrderId) Nothing -- set metrics
--   let routeOrderId = lookupTrueString "order_id" routeParams -- get order_id from path , empty "" = Nothing
--   let maybeOrderId = if isTrueString reqOrderId then unNullOrUndefined reqOrderId else routeOrderId
--
--   let updatedOrderCreateReq = if (isTrueString reqOrderId) || (isNothing routeOrderId) then orderCreateReq else updateOrderCreateReq orderCreateReq (unsafePartial fromJust routeOrderId)
--   -- * end
--   case maybeOrderId of
--     Nothing ->  throwECException 400 (ECErrorPayload {status: "Bad Request", status_id: NullOrUndefined Nothing, error_code :  NullOrUndefined $ Just "order_id is missing", error_message : NullOrUndefined $ Just "order_id is missing"})
--     Just orderId ->  do
--       merchantId  <- maybe (liftErr internalError) pure (unNullOrUndefined <<< _.merchantId <<< unwrap $ mAccnt)
-- -- * state update. This functionality repeated in another methods.
--       state       <- get
--       memCacheEnabled <- getCache ecRedis memCacheEnabled <#> (fromMaybe false)
--       meshEnabled <- isDBMeshEnabled  merchantId (Just orderId) true
--       _           <- log "isDBMeshEnabled" meshEnabled
--       let state'   = state # _merchantId .~ (Just merchantId)
--           updState' = state' # _orderId .~ (Just orderId)
--           updState'' = updState' # _isMemCacheEnabled .~ memCacheEnabled
--           updState  = updState'' # _isDBMeshEnabled .~ (Just meshEnabled)
--       state       <- put updState
-- -- * end
--       orderIfExists :: (Maybe OrderReference) <- findOne "ECRDB" $
--         where_ := WHERE ["order_id" /\ String orderId, "merchant_id" /\ String merchantId]
--       _ <- addMerchantIdToTrackingInfo merchantId -- metrics
--
--       resp <- case orderIfExists of
--         Just order -> do
--           _ <- updateOrder updatedOrderCreateReq order routeParams mAccnt
--           -- *             ^ return OrderReference
--           getOrderStatusWithoutAuth (getOrderStatusRequest orderId) routeParams mAccnt true (Just updatedOrderCreateReq) (Just $ order)
--           -- * ^ return Foreign (encoded OrderStatusResponse), also may change fields from snake case to camel case in checkEnableCaseForResponse
--         Nothing -> createOrder updatedOrderCreateReq routeParams mAccnt >>= (pure <<< encode)
--          -- * ^ return Foreign (encoded OrderAPIResponse)
--       log "createOrUpdateOrder response: " resp
--       pure resp
--
-- -- updates the order_id of the orderCreateReq
-- updateOrderCreateReq :: OrderCreateReq -> String -> OrderCreateReq
-- updateOrderCreateReq (OrderCreateReq req) orderId = OrderCreateReq $ req {order_id = just orderId}
--
-- mkOrder :: forall r st. Newtype st { orderId :: Maybe String, merchantId :: Maybe String, isDBMeshEnabled :: Maybe Boolean, isMemCacheEnabled :: Boolean | r } => MerchantAccount -> MerchantIframePreferences -> OrderCreateReq -> (NullOrUndefined Int) -> (NullOrUndefined Int) ->  BackendFlow st _ OrderReference
-- mkOrder (MerchantAccount account) (MerchantIframePreferences prefs) (OrderCreateReq req) billingAddrId shippingAddrId = do
--   orderUuid      <- (append "ordeu_" ) <$> getUUID32
--   currentDate    <- getCurrentDate
--   gatewayName    <- getGatewayName req.gateway_id
--   autoRefund     <- pure $ maybe (just false) just (unNullOrUndefined req.auto_refund)
--   mandateFeature <- pure $ just (nullOrUndefinedToAny req."options.create_mandate" DISABLED)
--   orderType      <- pure $ getOrderType (nullOrUndefinedToAny mandateFeature DISABLED)
--   maybeCustomer  <- maybe (pure Nothing) (\x -> Customer.findMaybeByMerchantAccountAndCustId x ((MerchantAccount account) .^. _id)) (unNullOrUndefined req.customer_id)
--   customerId     <- case [(isJust maybeCustomer),(orderType == MANDATE_REGISTER)] of
--                       [false,true] -> defaultThrowECException "INVALID_REQUEST" "Customer not found"
--                       _            -> pure $ req.customer_id
--   amount         <- roundOff2 $ req.amount
--   let (OrderReference default) = defaultOrder
--       orderRef =
--         (mapUDFParams req) default
--           { orderId = req.order_id
--           , merchantId = account.merchantId
--           , amount = just amount
--           , billingAddressId  = billingAddrId
--           , shippingAddressId = shippingAddrId
--           , currency = currency
--           , customerId = customerId
--           , customerEmail = req.customer_email
--           , customerPhone = req.customer_phone
--           , returnUrl = returnUrl
--           , description = description
--           , orderUuid = just orderUuid
--           , productId = req.product_id
--           , preferredGateway = gatewayName
--           , status = NEW
--           , dateCreated = currentDate
--           , lastModified = currentDate
--           , orderType = just orderType
--           , mandateFeature = mandateFeature
--           }
--   addCustomerInfo (OrderCreateReq req) (MerchantAccount account) (OrderReference orderRef)
--   where currency              = NullOrUndefined $ (unNullOrUndefined req.currency) <|> (unNullOrUndefined prefs.defaultCurrency) <|> Just "INR"
--         returnUrl             = NullOrUndefined $ ((unNullOrUndefined req.return_url) <|> Nothing)
--         description           = NullOrUndefined $ ((unNullOrUndefined req.description) <|> Just "")
--         getOrderType mandate  = if mandate == REQUIRED then MANDATE_REGISTER else ORDER_PAYMENT
--
-- getGatewayName :: NullOrUndefined String -> BackendFlow _ _ (NullOrUndefined String)
-- getGatewayName gatewayId = case (unNullOrUndefined gatewayId) of
--   Just id -> case (fromVal id) of
--     Just gatewayName -> pure $ just (show gatewayName)
--     Nothing          -> pure $ nothing
--   Nothing -> pure $ nothing
--
-- -- do nothing?
-- -- cleanupOrderParams :: forall a. OrderCreateReq -> BackendFlow _ _ OrderCreateReq
-- -- cleanupOrderParams orderCreateReq = pure orderCreateReq
--
-- -- any other checks?
-- -- validateOrderParams :: forall a. OrderCreateReq -> BackendFlow _ _ Unit
-- -- validateOrderParams (OrderCreateReq req) = if req.amount < 1.00
-- --   then throwECException 400 (ECErrorPayload {status: "ERROR", status_id: NullOrUndefined Nothing, error_code :  NullOrUndefined $ Just "invalid_request", error_message : NullOrUndefined $ Just "Invalid amount"})
-- --   else pure unit
--
-- addCustomerInfo :: forall st r. Newtype st { orderId :: Maybe String, merchantId :: Maybe String, isDBMeshEnabled :: Maybe Boolean, isMemCacheEnabled :: Boolean | r } => OrderCreateReq -> MerchantAccount -> OrderReference -> BackendFlow st _ OrderReference
-- addCustomerInfo orderCreateReq (MerchantAccount account) orderRef =
--   if hasPartialCustomerInfo
--   then do
--     customerId <- maybe (liftErr internalError) pure (unNullOrUndefined <<< _.customer_id <<< unwrap $ orderCreateReq)
--     accountId  <- maybe (liftErr internalError) pure (unNullOrUndefined <<< _.id $ account)
--     -- one more condiotion in DB query is missing!
--     optCustomer <- findOne ecDB (where_ :=
--         And ["merchant_account_id" ==? Int accountId
--       , Or  [ "object_reference_id" ==? String customerId, "id" ==? String customerId ]])
--     case optCustomer of
--       (Just (Customer cust)) ->
--         let orderVal = (unwrap orderRef)
--         in pure $ OrderReference orderVal
--             { customerPhone =
--                 NullOrUndefined $ (unNullOrUndefined orderVal.customerPhone) <|> (Just cust.mobileNumber)
--             , customerEmail =
--                 NullOrUndefined $ (unNullOrUndefined orderVal.customerEmail) <|> (unNullOrUndefined cust.emailAddress)
--             }
--       _ -> pure orderRef
--   else pure orderRef
--   where hasPartialCustomerInfo =
--           let orderParams = unwrap orderCreateReq
--           in (isJust <<< unNullOrUndefined <<< _.customer_id $ orderParams)
--               && ((isNothing <<< unNullOrUndefined <<< _.customer_phone $ orderParams)
--                 || (isNothing <<< unNullOrUndefined <<< _.customer_email $ orderParams))
--
--
-- mkBillingAddress :: OrderCreateReq -> OrderAddress
-- mkBillingAddress (OrderCreateReq req) = OrderAddress
--   { id : nothing
--   , version : defaultVersion
--   , firstName : req.billing_address_first_name
--   , lastName : req.billing_address_last_name
--   , line1 : req.billing_address_line1
--   , line2 : req.billing_address_line2
--   , line3 : req.billing_address_line3
--   , city : req.billing_address_city
--   , state : req.billing_address_state
--   , country : req.billing_address_country
--   , postalCode : req.billing_address_postal_code
--   , phone : req.billing_address_phone
--   , countryCodeIso : req.billing_address_country_code_iso
--   }
--
-- mkShippingAddress :: OrderCreateReq -> OrderAddress
-- mkShippingAddress (OrderCreateReq req) = OrderAddress
--   { id : nothing
--   , version : defaultVersion
--   , firstName : req.shipping_address_first_name
--   , lastName : req.shipping_address_last_name
--   , line1 : req.shipping_address_line1
--   , line2 : req.shipping_address_line2
--   , line3 : req.shipping_address_line3
--   , city  : req.shipping_address_city
--   , state : req.shipping_address_state
--   , country : req.shipping_address_country
--   , postalCode : req.shipping_address_postal_code
--   , phone : req.shipping_address_phone
--   , countryCodeIso : req.shipping_address_country_code_iso
--   }
--
-- mkMetadata :: OrderCreateReq -> Int -> BackendFlow _ _ OrderMetadataV2
-- mkMetadata (OrderCreateReq req) orderId = do
--   config <- ask
--   currentDate <- getCurrentDate
--   pure $ OrderMetadataV2 {
--     id : nothing
--   , browser : nothing
--   , browserVersion : nothing
--   , device : nothing
--   , ipAddress : just config.sourceIP
--   , metadata : req.metaData
--   , mobile : nothing
--   , operatingSystem : nothing
--   , orderReferenceId : orderId
--   , referer : nothing
--   , userAgent : just config.userAgent
--   , dateCreated : currentDate
--   , lastUpdated : currentDate
--   }
--
-- mkMandatedata :: String -> OrderCreateReq -> Int -> Number -> BackendFlow _ _ Mandate
-- mkMandatedata merchantId (OrderCreateReq req) orderId maxAmount = do
--   mandateId <- getShortUUID
--   currentDate <- getCurrentDate
--   token <- getUUID32
--   pure $ Mandate {
--     id : nothing
--   , merchantId : merchantId
--   , currency : req.currency
--   , endDate : nothing
--   , startDate : nothing
--   , maxAmount : just maxAmount
--   , merchantCustomerId : req.customer_id
--   , paymentMethod : nothing
--   , paymentMethodType : nothing
--   , paymentMethodId : nothing
--   , gateway : nothing
--   , gatewayParams : nothing
--   , token : token
--   , mandateId : mandateId
--   , status : MANDATE.CREATED
--   , authOrderId : just orderId
--   , activatedAt : nothing
--   , dateCreated : currentDate
--   , lastModified : currentDate
--   , authTxnCardInfo : nothing
--   , merchantGatewayAccountId : nothing
--   , metadata : nothing
--   }
--
-- cleanUp :: NullOrUndefined String -> NullOrUndefined String
-- cleanUp udf = NullOrUndefined $ cleanUpSpecialCharsForMaybe $ unNullOrUndefined udf
--
-- mapUDFParams :: forall t613 t623 t626 t629 t632 t635 t637 t638 t639 t640 t641 t642 t643 t644 t645 t646 t647.
--   { udf1 :: NullOrUndefined String
--   , udf2 :: NullOrUndefined String
--   , udf3 :: NullOrUndefined String
--   , udf4 :: NullOrUndefined String
--   , udf5 :: NullOrUndefined String
--   , udf6 :: t623
--   , udf7 :: t626
--   , udf8 :: t629
--   , udf9 :: t632
--   , udf10 :: t635
--   | t637
--   }
--   -> { udf1 :: t638
--      , udf2 :: t639
--      , udf3 :: t640
--      , udf4 :: t641
--      , udf5 :: t642
--      , udf6 :: t643
--      , udf7 :: t644
--      , udf8 :: t645
--      , udf9 :: t646
--      , udf10 :: t647
--      | t613
--      }
--      -> { udf1 :: NullOrUndefined String
--         , udf2 :: NullOrUndefined String
--         , udf3 :: NullOrUndefined String
--         , udf4 :: NullOrUndefined String
--         , udf5 :: NullOrUndefined String
--         , udf6 :: t623
--         , udf7 :: t626
--         , udf8 :: t629
--         , udf9 :: t632
--         , udf10 :: t635
--         | t613
--         }
-- mapUDFParams req params =
--   params
--     {  -- * why udf 1-5 cleaned and another not?
--       udf1 = cleanUp req.udf1
--     , udf2 = cleanUp req.udf2
--     , udf3 = cleanUp req.udf3
--     , udf4 = cleanUp req.udf4
--     , udf5 = cleanUp req.udf5
--     , udf6 = req.udf6
--     , udf7 = req.udf7
--     , udf8 = req.udf8
--     , udf9 = req.udf9
--     , udf10 = req.udf10
--     }
--
-- updateOrder :: forall a r st. Newtype st { orderId :: Maybe String, merchantId :: Maybe String, isDBMeshEnabled :: Maybe Boolean, isMemCacheEnabled :: Boolean | r } => OrderCreateReq → OrderReference → RouteParameters → MerchantAccount → BackendFlow st _ OrderReference
-- updateOrder orderCreateReq@(OrderCreateReq req) orderRef@(OrderReference order) routeParams mAccnt@(MerchantAccount mAccntVal) = do
--   case order.status of
--     SUCCESS -> do
--       log "not_updating_successful_order" ("Order: " <> show (unNullOrUndefined order.orderId) <> " has already succeeded. Not updating any field.")
--       pure orderRef
--     _ ->  do
--       -- update shipping address
--       let sa = mkShippingAddress orderCreateReq
--       shippingAddressId <- if isAddressEmpty sa
--         then pure nothing
--         else
--           case (unNullOrUndefined order.shippingAddressId) of
--             Nothing -> getShippingAddrId orderCreateReq
--             Just id -> do
--               _ <- updateOrderAddress id sa
--               pure nothing
--
--       -- update billing address
--       let ba = mkBillingAddress orderCreateReq
--       billingAddressId <- if isAddressEmpty ba
--         then pure nothing
--         else
--           case (unNullOrUndefined order.billingAddressId) of
--           Nothing -> getBillingAddrId orderCreateReq mAccnt
--           Just id -> do
--             let ba = mkBillingAddress orderCreateReq
--             _ <- updateOrderAddress id ba
--             pure nothing
--
--       -- update amount, udf and last modified
--
--       -- * roundOff2 looks like
--       -- * foo x = (\(a,b) -> read @Double $ a <> b) $ bimap id (take 3) $ span (/='.') $ show x
--       -- * with logging incoming and outcoming values
--       amount <- roundOff2 req.amount
--       let udf = mkUdf orderCreateReq
--       currentDate <- getCurrentDate
--       updateOrderRefAndInValidateCache (OrderReference $ order
--         { amount = just amount
--         , billingAddressId = billingAddressId
--         , shippingAddressId = shippingAddressId
--         , udf1 = udf.udf1
--         , udf2 = udf.udf2
--         , udf3 = udf.udf3
--         , udf4 = udf.udf4
--         , udf5 = udf.udf5
--         , udf6 = udf.udf6
--         , udf7 = udf.udf7
--         , udf8 = udf.udf8
--         , udf9 = udf.udf9
--         , udf10 = udf.udf10
--         , lastModified = currentDate
--       })
--
--
-- mkUdf :: OrderCreateReq -> UDF
-- mkUdf orderCreateReq@(OrderCreateReq req) =
--    if isUdfEmpty orderCreateReq
--      then
--       {
--         udf1 : nothing
--       , udf2 : nothing
--       , udf3 : nothing
--       , udf4 : nothing
--       , udf5 : nothing
--       , udf6 : nothing
--       , udf7 : nothing
--       , udf8 : nothing
--       , udf9 : nothing
--       , udf10 : nothing
--       }
--      else udf
--       where udf =
--               { -- * why udf 1-5 are cleaned and others are not?
--                 udf1: cleanUp req.udf1
--               , udf2: cleanUp req.udf2
--               , udf3: cleanUp req.udf3
--               , udf4: cleanUp req.udf4
--               , udf5: cleanUp req.udf5
--               , udf6: req.udf6
--               , udf7: req.udf7
--               , udf8: req.udf8
--               , udf9: req.udf9
--               , udf10: req.udf10
--               }
--
-- -- * isAbsent from src/Engineering/Commons.purs
-- -- * = isNothing
--
-- isUdfEmpty :: OrderCreateReq -> Boolean
-- isUdfEmpty (OrderCreateReq {udf1, udf2, udf3, udf4, udf5, udf6, udf7, udf8, udf9, udf10}) =
--  isAbsent udf1
--   && isAbsent udf2
--   && isAbsent udf3
--   && isAbsent udf4
--   && isAbsent udf5
--   && isAbsent udf6
--   && isAbsent udf7
--   && isAbsent udf8
--   && isAbsent udf9
--   && isAbsent udf10
--
-- isAddressEmpty :: OrderAddress -> Boolean
-- isAddressEmpty (OrderAddress {
--     firstName
--   , lastName
--   , line1
--   , line2
--   , line3
--   , city
--   , state
--   , country
--   , postalCode
--   , phone
--   , countryCodeIso
--   }) =
--    isAbsent firstName
--     && isAbsent lastName
--     && isAbsent line1
--     && isAbsent line2
--     && isAbsent line3
--     && isAbsent city
--     && isAbsent state
--     && isAbsent country
--     && isAbsent postalCode
--     && isAbsent phone
--     && isAbsent countryCodeIso
--
-- -- the host has to change for the reseller account!
-- -- One more db call required for fetching reseller account. !!!
-- makeOrderResponse :: OrderReference -> Maybe String -> OrderAPIResponse
-- makeOrderResponse (OrderReference orderRef) maybeResellerEndpoint = OrderCreateResp $ (unwrap defaultOrderAPIResponse)  {
--     status= CREATED
--   , status_id= orderStatusToInt CREATED
--   , id= orderUuid
--   , order_id = nullOrUndefinedToStr orderRef.orderId
--   , payment_links= Paymentlinks {
--       web: NullOrUndefined $ Just (url <> "/merchant/pay/") <> Just orderUuid,
--       mobile: NullOrUndefined $ Just (url <> "/merchant/pay/") <> Just orderUuid <> Just "?mobile=true",
--       iframe: NullOrUndefined $ Just (url <> "/merchant/ipay/") <> Just orderUuid
--     }
-- }
--   where
--     config   = Config.getECRConfig
--     protocol = (unwrap config).protocol
--     host = (unwrap config).host
--     url = maybe (protocol <> "://" <> host) id maybeResellerEndpoint
--     orderUuid = nullOrUndefinedToStr orderRef.orderUuid
--
-- updateResponse :: OrderReference -> OrderAPIResponse -> NullOrUndefined OrderTokenResp -> OrderAPIResponse
-- updateResponse (OrderReference orderRef) (OrderCreateResp apiResp) orderTokenResp = do
--  OrderCreateResp $ apiResp {
--     status= NEW
--   , status_id= orderStatusToInt NEW
--   , juspay = orderTokenResp
--   , udf1 = getEmptyStringVal orderRef.udf1
--   , udf2 = getEmptyStringVal orderRef.udf2
--   , udf3 = getEmptyStringVal orderRef.udf3
--   , udf4 = getEmptyStringVal orderRef.udf4
--   , udf5 = getEmptyStringVal orderRef.udf5
--   , udf6 = getEmptyStringVal orderRef.udf6
--   , udf7 = getEmptyStringVal orderRef.udf7
--   , udf8 = getEmptyStringVal orderRef.udf8
--   , udf9 = getEmptyStringVal orderRef.udf9
--   , udf10 = getEmptyStringVal orderRef.udf10
--   , return_url = getEmptyStringVal orderRef.returnUrl
--   , refunded = getEmptyBooleanVal orderRef.refundedEntirely
--   , product_id = getEmptyStringVal orderRef.productId
--   , merchant_id = getEmptyStringVal orderRef.merchantId
--   , date_created = just $ orderRef.dateCreated
--   , customer_phone = getEmptyStringVal orderRef.customerPhone
--   , customer_id = getEmptyStringVal orderRef.customerId
--   , customer_email = getEmptyStringVal orderRef.customerEmail
--   , currency = getEmptyStringVal orderRef.currency
--   , amount_refunded = getEmptyNumberVal orderRef.amountRefunded
--   , amount = getEmptyNumberVal orderRef.amount
-- }
--
-- {-
-- newtype OrderAPIResponse = OrderCreateResp
--  { status :: OrderStatus
--   , order_id :: String
--   , payment_links :: Paymentlinks
--   , product_id :: NullOrUndefined String
--   , date_created :: NullOrUndefined Date
--   , customer_phone :: NullOrUndefined String
--   , customer_id :: NullOrUndefined String
--   , customer_email :: NullOrUndefined String
-- }
--
-- --- difference
--
-- newtype OrderStatusResponse = OrderStatusResponse
--     {  order_id :: NullOrUndefined String
--     ,  date_created :: String
--     ,  product_id :: String
--     ,  customer_email :: NullOrUndefined Foreign
--     ,  customer_phone :: NullOrUndefined Foreign
--     ,  customer_id :: NullOrUndefined Foreign
--     ,  payment_links :: Paymentlinks
--     ,  txn_id :: NullOrUndefined String
--     ,  status :: String -- Check --venkat
--     ,  payment_method_type :: NullOrUndefined String
--     ,  auth_type :: NullOrUndefined String
--     ,  card :: NullOrUndefined Card
--     ,  payment_method :: NullOrUndefined String
--     ,  chargebacks :: NullOrUndefined (Array Chargeback')
--     ,  refunds :: NullOrUndefined (Array Refund')
--     ,  mandate :: NullOrUndefined Mandate'
--     ,  promotion :: NullOrUndefined Promotion'
--     ,  risk :: NullOrUndefined Risk
--     ,  bank_error_code :: NullOrUndefined String
--     ,  bank_error_message :: NullOrUndefined String
--     ,  txn_uuid :: NullOrUndefined String
--     ,  gateway_payload :: NullOrUndefined String
--     ,  txn_detail :: NullOrUndefined TxnDetail'
--     ,  payment_gateway_response' :: NullOrUndefined MerchantPaymentGatewayResponse'
--     ,  payment_gateway_response :: NullOrUndefined MerchantPaymentGatewayResponse
--     ,  gateway_id :: NullOrUndefined Int
--     ,  emi_bank :: NullOrUndefined String
--     ,  emi_tenure :: NullOrUndefined Int
--     ,  gateway_reference_id :: NullOrUndefined Foreign
--     ,  payer_vpa :: NullOrUndefined Foreign
--     ,  payer_app_name :: NullOrUndefined Foreign
--     }
-- -}
--
--
--
