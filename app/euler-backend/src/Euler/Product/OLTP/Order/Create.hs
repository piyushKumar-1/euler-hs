{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns  #-}

module Euler.Product.OLTP.Order.Create where

import EulerHS.Prelude hiding (id, show, get)
import qualified EulerHS.Prelude as EHP
import EulerHS.Language
import WebService.Language

import           Data.Generics.Product.Fields
import           Data.Generics.Product.Subtype
import           Data.List (lookup, span)
import           Servant.Server

-- EHS: Storage interaction should be extracted from here into separate modules
import Euler.Storage.Types.Customer
import Euler.Storage.Types.Mandate
import qualified Euler.Storage.Types.MerchantAccount as DBMA
import qualified Euler.Storage.Types.OrderAddress as Address (id)
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

-- EHS: Should not depend on API?
import qualified Euler.API.RouteParameters  as RP
import qualified Euler.API.Order            as API

import Euler.Common.Types.DefaultDate
import Euler.Common.Types.Gateway
import Euler.Common.Types.Order
import Euler.Common.Types.OrderMetadata
import qualified Euler.Common.Types.Mandate as M
import qualified Euler.Common.Types.Order   as C
import qualified Euler.Common.Metric        as Metric
import Euler.Product.Domain.Order
import Euler.Product.Domain.MerchantAccount
import Euler.Product.OLTP.Order.OrderStatus (getOrderStatusRequest, getOrderStatusWithoutAuth)
import Euler.Product.OLTP.Services.RedisService
import qualified Euler.Config.Config        as Config
import qualified Euler.Config.ServiceConfiguration as SC

import qualified Euler.Product.Domain.Templates as Ts

import Euler.Lens
import Euler.Storage.DBConfig
import qualified Database.Beam as B
import qualified Database.Beam.Backend.SQL as B
import Database.Beam ((==.), (&&.), (||.), (<-.), (/=.))

loadOrder :: OrderId -> MerchantId -> Flow (Maybe Order)
loadOrder orderId' merchantId' = do
    conn <- getConn eulerDB       -- EHS: withDB
    res <- runDB conn $ do

      let predicate OrderReference {orderId, merchantId} =
            (orderId    ==. B.just_ (B.val_ orderId')) &&.
            (merchantId ==. B.just_ (B.val_ merchantId'))
      findRow
        $ B.select
        $ B.limit_ 1
        $ B.filter_ predicate
        $ B.all_ (order_reference eulerDBSchema)

    case res of
      Right mordRef -> pure mordRef
      Left err -> do
        logError "Find OrderReference" $ toText $ P.show err

        -- EHS: magical number
        -- A: number only for debug
        -- EHS: why throwing error?
        --      Is this a critical situation?
        --      Rework the return type. Should return Either / Maybe instead of throwing.
        -- A: this error from db interraction (lost connection, etc) so should be 505 internal error
        -- EHS: what flow should be on not found?
        -- A: return type is Maybe, so return Nothing and consumer chose what to do
        throwException err500 {errBody = "2"}

-- EHS: should not depend on API types. Rethink OrderCreateResponse.
-- EHS: this function is about validation.
orderCreate :: RP.RouteParameters -> Ts.OrderCreateTemplate -> MerchantAccount -> Flow API.OrderCreateResponse
orderCreate routeParams order' mAccnt = do

  -- EHS: HTTP error codes should exist only on the server level, not in business logic.
  -- EHS: loading merchant is a effectful pre-validation procedure.
  --      Should be done on the validation layer.
  -- EHS: (Maybe MerchantId) should not be Maybe.
  merchantId' <- maybe (throwException err500 {errBody = "1"}) pure (mAccnt ^. _merchantId)

  existingOrder <- loadOrder (order' ^. _order_id) merchantId'

  resp <- case existingOrder of
        -- EHS: rework exceptions
      Just orderRef -> throwException err400 {errBody = "Order already created."}
      Nothing       -> doOrderCreate routeParams order' mAccnt
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

doOrderCreate :: RP.RouteParameters -> Ts.OrderCreateTemplate -> MerchantAccount -> Flow API.OrderCreateResponse -- OrderAPIResponse
doOrderCreate routeParams order' mAccnt@MerchantAccount{..} = do

  mandateOrder <- isMandateOrder routeParams order' merchantId

  orderRef <- createOrder' routeParams order' mAccnt mandateOrder

  -- EHS: orderIdPrimary should exist to the moment.
  -- orderIdPrimary <- getOrderId orderRef

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

isMandateOrder :: RP.RouteParameters -> Ts.OrderCreateTemplate -> MerchantId -> Flow Bool
isMandateOrder routeParams (Ts.OrderCreateTemplate {optionsCreateMandate}) merchantId = do
  case optionsCreateMandate of
    DISABLED -> pure False
    _        -> do
      validMandate <- isMandateValid order' merchantId

      -- EHS: rework exceptions.
      -- throwCustomException 400 "INVALID_REQUEST" "invalid mandate params"
      -- EHS: isMandateValid never returns False? Then throwing is not needed
      unless validMandate $ throwException $ err400 {errBody = "invalid mandate params"}

      pure True


-- EHS: this function is really bad.
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
-- added validator "notBlank" for "customer_id" field in OrderCreateRequest
isTrueString :: Maybe Text -> Bool
isTrueString val =
  case val of
    Just "" -> False
    Just " " -> False
    Just _ -> True
    Nothing -> False

-- EHS: should be MerchantIframePreferences converted to domain type?
-- EHS: rework this function.
loadMerchantPrefs :: MerchantId -> Flow MerchantIframePreferences
loadMerchantPrefs merchantId' = do
  conn <- getConn eulerDB   -- EHS: withDB
  res <- runDB conn $ do
    let predicate MerchantIframePreferences {merchantId} = merchantId ==. (B.val_ merchantId')
    findRow
      $ B.select
      $ B.limit_ 1
      $ B.filter_ predicate
      $ B.all_ (merchant_iframe_preferences eulerDBSchema)

  case res of
    Right (Just mIPrefs) -> pure mIPrefs
    Right Nothing -> do
      logError "merchant_iframe_preferences" $ "Not found for merchant " <> merchantId'
      throwException err500 {errBody = "6"}   -- EHS: rework exceptions
    Left err -> do
      logError "SQLDB Interraction." $ toText $ P.show err
      throwException err500 {errBody = "7"}   -- EHS: rework exceptions

createOrder' :: RP.RouteParameters -> Ts.OrderCreateTemplate -> MerchantAccount -> Bool -> Flow OrderReference
createOrder' routeParams order' mAccnt isMandate = do

  -- EHS: TODO
  merchantPrefs  <- loadMerchantPrefs $ mAccnt ^. _merchantId

  do
    mbCustomer                <- loadCustomer (order' ^. _customerId) (mAccnt ^. _id)
    let billingAddrHolder     = fillBillingAddressHolder mbCustomer (order' ^. _billingAddrHolder)

    mbBillingAddrId           <- updateAddress (order' ^. _billingAddr) billingAddrHolder
    -- EHS: not sure why we don't load customer info for shipping addr as we did for billing addr.
    mbShippingAddrId          <- updateAddress (order' ^. _shippingAddr) (order' ^. _shippingAddrHolder)

    -- .............................................................................
    orderRefVal               <- mkOrder order' mAccnt merchantPrefs mbBillingAddrId mbShippingAddrId billingAddrCustomer

  orderRef :: OrderReference <- do
    mOref <- withDB eulerDB $ do
      insertRowsReturningList
        $ B.insert (order_reference eulerDBSchema)
        $ B.insertExpressions [(B.val_ orderRefVal) & _id .~ B.default_]
    case mOref of
      [ordRef] -> pure ordRef
      x -> throwException err500 {errBody = EHP.show x}
  orderIdPrimary <- getOrderId orderRef
  -- TODO: FlipKart alone uses useragent and IP address. Lets remove for others
  defaultMetaData <- mkMetadata routeParams order' orderIdPrimary
  _ <- do
    conn <- getConn eulerDB
    runDB conn $ do
      insertRows
        $ B.insert (order_metadata_v2 eulerDBSchema)
        $ B.insertExpressions [ B.val_ defaultMetaData]
  orderId <- maybe (throwException err500 {errBody = "9"}) pure (getField @"orderId"  orderRef)
  _ <- setCacheWithExpiry (merchantId' <> "_orderid_" <> orderId) orderRef Config.orderTtl
  _ <- when isMandate (setMandateInCache orderCreateReq orderIdPrimary orderId merchantId')
  pure orderRef

fillBillingAddressHolder :: Maybe Ts.CustomerTemplate -> AddressHolderTemplate -> Ts.AddressHolderTemplate
fillBillingAddressHolder Nothing addressHolder = addressHolder
fillBillingAddressHolder (Just (TS.CustomerTemplate _ mbFirstName mbLastName)) (Ts.AddressHolderTemplate {..})
  = Ts.AddressHolderTemplate (firstName <|> mbFirstName) (lastName <|> mbLastName)


loadCustomer :: Maybe CustomerId -> MerchantAccountId -> Flow (Maybe Ts.CustomerTemplate)
loadCustomer Nothing _ = pure Nothing
loadCustomer (Just customerId) mAccntId = do

    -- EHS: after refactoring, we query DB every time when customerId is available.
    -- EHS: rework query
    -- EHS: DB type Customer should be explicit or qualified
    mbCustomer :: Maybe Customer <- withDB eulerDB $ do
      let predicate Customer {merchantAccountId, id, objectReferenceId}
            = (   id ==.  (B.val_ customerId)
              ||. objectReferenceId ==. (B.val_ customerId)  -- EHS: objectReferenceId is customerId ?
                                                             -- A: in customer DB table "objectReferenceId" - id from merchant
                                                             -- (how merchant identifies customer in their DB eg. by email or mobile number )
                                                             -- and "id" - our id. So merchant can use both
              )

            -- EHS: B.as_ @(Maybe Int) ? Why Maybe?
            &&. (B.maybe_ (B.val_ False) (merchantAccountId ==.) (B.as_ @(Maybe Int) $ B.val_ mAccntId))
      findRow
        $ B.select
        $ B.filter_ predicate
        $ B.all_ (customer eulerDBSchema)

    pure $ case mbCustomer of
      Just (Customer {..}) -> Just $ Ts.CustomerTemplate customerId firstName lastName
      Nothing              -> Nothing

updateAddress :: Ts.AddressTemplate -> Ts.AddressHolderTemplate -> Flow (Maybe AddressId)
updateAddress addr addrHolder =
  case toDBAddress addr addrHolder of
    Nothing -> pure Nothing
    Just dbAddr -> do
      mAddr <- withDB eulerDB $ do
        insertRowsReturningList
          $ B.insert (order_address eulerDBSchema)
          $ B.insertExpressions [(B.val_ dbAddr) & _id .~ B.default_]
      pure $ (^. _id) =<< (safeHead mAddr)


-- EHS: get function invalidly does inserting into DB.
-- A: Address field "id" in DB is autoincremental, cant be created in flow
-- maybe another function name will be better
-- getBillingAddrId :: Ts.OrderCreateTemplate -> MerchantAccount -> Flow  (Maybe Int)
-- getBillingAddrId order' mAccnt = do
--   orderReq <- addCustomerInfoToRequest order' mAccnt
--
--   if (present orderReq)
--   then do
--     let newAddr = mkBillingAddress $ upcast orderReq
--
--     mAddr <- withDB eulerDB $ do
--       insertRowsReturningList
--         $ B.insert (order_address eulerDBSchema)
--         $ B.insertExpressions [ (B.val_ newAddr)  & _id .~ B.default_]
--
--     pure $   (^. _id) =<< (safeHead mAddr) -- getField @"id" mAddr -- <$> createWithErr ecDB (mkBillingAddress orderReq) internalError
--   else pure Nothing
--   where present (OrderCreateRequest {..}) = isJust
--           $   billing_address_first_name
--           <|> billing_address_last_name
--           <|> billing_address_line1
--           <|> billing_address_line2
--           <|> billing_address_line3
--           <|> billing_address_city
--           <|> billing_address_state
--           <|> billing_address_country
--           <|> billing_address_postal_code
--           <|> billing_address_phone
--           <|> billing_address_country_code_iso

-- EHS: get function invalidly does inserting into DB.
-- A: Address field "id" in DB is autoincremental, cant be created in flow
-- maybe another function name will be better
-- getShippingAddrId ::  OrderCreateRequest -> Flow (Maybe Int)
-- getShippingAddrId orderCreateReq@OrderCreateRequest{..} =
--   if (present orderCreateReq)
--   then do
--     let newAddr = mkShippingAddress $ upcast orderCreateReq
--     mAddr <- withDB eulerDB $ do
--       insertRowsReturningList -- insertRows
--         $ B.insert (order_address eulerDBSchema)
--         $ B.insertExpressions [ (B.val_ newAddr)  & _id .~ B.default_]
--     case mAddr of
--       [] -> throwException err500 {errBody = "emptyaddr"}
--       x -> runIO $ putStrLn $ P.show x
--     pure $ (^. _id) =<< (safeHead mAddr) -- (getField @"id") <$> createWithErr ecDB (mkShippingAddress orderCreateReq) internalError
--   else pure Nothing
--   where present (OrderCreateRequest {..}) = isJust
--           $    shipping_address_first_name
--           <|>  shipping_address_last_name
--           <|>  shipping_address_line1
--           <|>  shipping_address_line2
--           <|>  shipping_address_line3
--           <|>  shipping_address_city
--           <|>  shipping_address_state
--           <|>  shipping_address_country
--           <|>  shipping_address_postal_code
--           <|>  shipping_address_phone
--           <|>  shipping_address_country_code_iso

-- EHS: DB types should be denoted explicitly.
toDBAddress :: Ts.AddressTemplate -> Ts.AddressHolderTemplate -> Maybe OrderAddress
toDBAddress (Ts.AddressTemplate {..}) (Ts.AddressHolderTemplate {..})
  | nothingSet = Nothing
  | otherwise = Just $ OrderAddress
        { id             = Nothing  -- in DB it's not Null, Auto increment
        , version        = 1        -- defaultVersion
      -- from src/Config/Constants.purs
      --  defaultVersion :: Int
      --  defaultVersion = 1
        ..
        }
  where
    nothingSet = isNothing
      $   firstName
      <|> lastName
      <|> line1
      <|> line2
      <|> line3
      <|> city
      <|> state
      <|> country
      <|> postalCode
      <|> phone
      <|> countryCodeIso

mkBillingAddress :: OrderCreateRequest -> OrderAddress
mkBillingAddress OrderCreateRequest {..} = OrderAddress
  { id             = Nothing -- in DB it Not Null, Auto increment
  , version        = 1 -- defaultVersion
-- from src/Config/Constants.purs
--  defaultVersion :: Int
--  defaultVersion = 1
  , firstName      = billing_address_first_name
  , lastName       = billing_address_last_name
  , line1          = billing_address_line1
  , line2          = billing_address_line2
  , line3          = billing_address_line3
  , city           = billing_address_city
  , state          = billing_address_state
  , country        = billing_address_country
  , postalCode     = billing_address_postal_code
  , phone          = billing_address_phone
  , countryCodeIso = billing_address_country_code_iso
  }
-- add firstName and lastName

---- ????????????
-- addCustomerInfoToRequest :: Ts.OrderCreateTemplate -> MerchantAccount -> Flow OrderCreateRequest
-- addCustomerInfoToRequest order' mAccnt = do
--
--   if shouldAddCustomerNames order' then do
--
--     -- EHS: rework query
--     maybeCustomer :: Maybe Customer <- withDB eulerDB $ do
--       let predicate Customer {merchantAccountId, id, objectReferenceId}
--             = (   id ==.  (B.val_ $  order' ^. _customer_id)
--               ||. objectReferenceId ==. (B.val_ $  order' ^. _customer_id)  -- EHS: objectReferenceId is customerId ?
--                                                                             -- A: in customer DB table "objectReferenceId" - id from merchant
--                                                                             -- (how merchant identifies customer in their DB eg. by email or mobile number )
--                                                                             -- and "id" - our id. So merchant can use both
--               )
--             &&. ( B.maybe_ (B.val_ False) (merchantAccountId ==. ) ( B.as_ @(Maybe Int) $ B.val_  $ mAccnt ^. _id))
--       findRow
--         $ B.select
--         $ B.filter_ predicate
--         $ B.all_ (customer eulerDBSchema)
--
--     case maybeCustomer of
--       Just (Customer {..}) ->
--           let OrderCreateRequest {..} = order'
--           in
--             pure (order' :: OrderCreateRequest)
--                    { billing_address_first_name = (billing_address_first_name <|> firstName)
--                    , billing_address_last_name = (billing_address_last_name <|> lastName)
--                    }
--       Nothing -> pure order'
--   else pure order'
--
--   where shouldAddCustomerNames OrderCreateRequest {..} =
--            (isJust customer_id)
--               && ((isNothing billing_address_first_name)
--                  || (isNothing billing_address_last_name))

-- EHS: previously findMaybeByMerchantAccountAndCustId
-- EHS: why it does nothing???
-- findMaybeByMerchantAccountAndCustId :: Text -> Int -> Flow (Maybe Customer)
-- findMaybeByMerchantAccountAndCustId custId mId = pure Nothing
-- -- (DB.findOne ecDB $ where_ := And ["merchant_account_id" ==? Int mId, Or ["id" ==? String custId, "object_reference_id" ==? String custId]] :: Where Customer)

-- EHS: 'mk' is a bad prefix for this.
mkOrder
  :: Ts.OrderCreateTemplate
  -> MerchantAccount
  -> MerchantIframePreferences
  -> Maybe Int
  -> Maybe Int
  -> CustomerInfoTemplate
  -> Flow OrderReference
mkOrder (order'@Ts.OrderCreateTemplate {..}) mAccnt prefs mbBillingAddrId mbShippingAddrId customer = do

  -- Returns UUID with deleted '-'.
  -- EHS: what are other requirements to UUID?
  -- EHS: magic constant
  orderUuid      <- ("ordeu_" <> ) <$> getUUID32

  -- EHS: is this time ok?
  currentDate    <- getCurrentTimeUTC

  let mbGatewayName = gatewayId >>= lookupGatewayName

   -- EHS: not used?
  -- autoRefund     <- pure $ maybe (Just False) Just auto_refund

  -- maybeCustomer  <- maybe (pure Nothing) (\x -> findMaybeByMerchantAccountAndCustId x (mAccnt ^. _id)) customer_id
  -- EHS: we're updated addresses in the DB when realized the request is invalid.
  -- EHS: this is essentially a security problem.
  -- EHS: this rule should be enforced earlier, before any updates.
  mbCustomerId <- case (customer ^. _id, orderType == MANDATE_REGISTER) of
      (Nothing, True) -> throwException $ err500 {errBody = "Customer not found"}
      (mbCId, _)      -> pure mbCId


  -- * roundOff2 looks like
  -- * foo x = (\(a,b) -> read @Double $ a <> b) $ bimap id (take 3) $ span (/='.') $ show x
  -- EHS: we need a test on this rounding logic.
  let roundedAmount = testRoundOff2 amount -- roundOff2 $ req.amount
  let orderRef = defaultOrderReference
          { orderId = Just orderId
          , merchantId = getField @"merchantId" account
          , amount = Just roundedAmount
          , billingAddressId  = mbBillingAddrId
          , shippingAddressId = mbShippingAddrId
          , currency = currency'
          , customerId = mbCustomerId
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
          , mandateFeature = optionsCreateMandate
          }
  addCustomerInfo req account (mapUDFParams req orderRef)
  where currency'              =  ( currency) <|> (defaultCurrency prefs) <|> Just "INR"
        returnUrl'             =  (( return_url) <|> Nothing)
        description'           =  (( description) <|> Just "")


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
  -- EHS: Why are udf fields 1-5 cleaned and the rest not?

-- EHS: Bad funciton
-- getGatewayName :: Maybe Text -> Flow (Maybe Text)
-- getGatewayName gatewayId = pure $ toText . P.show <$> (fromVal =<< gatewayId)
 --  case gatewayId of
 --    Just id -> case (fromVal id) of
 --      Just gatewayName -> pure $ Just (show gatewayName)
 --      Nothing          -> pure $ Nothing
 --    Nothing -> pure $ nothing

-- EHS: awful function
-- -- from src/Product/Gateway/Utils.purs
-- fromVal :: Text -> Maybe Gateway
-- fromVal = (flip lookup $ swap <$> gatewayMap) <=< readMayT
--  -- * fromString :: String -> Maybe Int
--  -- case fromString strVal of
--  --   Just val -> lookup val $ map swap gatewayMap
--  --   Nothing -> Nothing

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

-- getOrderId :: OrderReference -> Flow Int
-- getOrderId OrderReference{..} = case id of
--     Just x -> pure x
--     _ -> throwException err500 {errBody = "NO ORDER FOUND"} -- defaultThrowECException "NO_ORDER_FOUND" "NO ORDER FOUND"


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
