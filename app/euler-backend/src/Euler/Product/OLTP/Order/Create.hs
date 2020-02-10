{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns  #-}

module Euler.Product.OLTP.Order.Create where

import EulerHS.Prelude hiding (id, show, get)
import qualified EulerHS.Prelude as EHP

import qualified Prelude              as P (show, notElem)
import qualified Data.Aeson           as A
import qualified Data.Text            as Text
import qualified Text.Read            as TR
import           Data.Generics.Product.Fields
import           Data.Generics.Product.Subtype
import           Data.List (lookup, span)
import qualified Database.Beam as B
import qualified Database.Beam.Backend.SQL as B
import           Database.Beam ((==.), (&&.), (||.), (<-.), (/=.))

-- EHS: Storage interaction should be extracted from here into separate modules.
-- EHS: Storage namespace should have a single top level module.
import qualified Euler.Storage.Types as DB
import           Euler.Storage.Types.EulerDB (eulerDBSchema)
import           Euler.Storage.DBConfig

import           Euler.KVDB.Redis (rGet, setCacheWithExpiry)
import           EulerHS.Language
import           WebService.Language

-- EHS: Should not depend on API?
import qualified Euler.API.RouteParameters  as RP
import qualified Euler.API.Order            as API

-- EHS: rework imports. Use top level modules.
-- EHS: do not duplicate imports. Each import should not be specified several times.
import           Euler.Common.Types.DefaultDate
import           Euler.Common.Types.Gateway
import           Euler.Common.Types.Order
import           Euler.Common.Types.OrderMetadata
import qualified Euler.Common.Types.Mandate as M
import qualified Euler.Common.Metric        as Metric
import qualified Euler.Product.Domain.Order as D
import           Euler.Product.Domain.MerchantAccount
import           Euler.Product.OLTP.Services.RedisService
import qualified Euler.Product.Domain.Templates    as Ts
import qualified Euler.Config.Config               as Config
import qualified Euler.Config.ServiceConfiguration as SC
import           Euler.Lens


loadOrder :: OrderId -> MerchantId -> Flow (Maybe Order)
loadOrder orderId' merchantId' = do
    res <- withDB eulerDB $ do
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

        -- EHS: why throwing error?
        --      Is this a critical situation?
        --      Rework the return type. Should return Either / Maybe instead of throwing.
        -- A: this error from db interraction (lost connection, etc) so should be 505 internal error
        -- EHS: what flow should be on not found?
        -- A: return type is Maybe, so return Nothing and consumer chose what to do
        -- EHS: rework errors.
        throwException err500 {errBody = "2"}

-- EHS: should not depend on API types. Rethink OrderCreateResponse.
-- EHS: this function is about validation.
orderCreate :: RP.RouteParameters -> Ts.OrderCreateTemplate -> MerchantAccount -> Flow API.OrderCreateResponse
orderCreate routeParams order' mAccnt = do
  -- EHS: loading merchant is a effectful pre-validation procedure.
  --      Should be done on the validation layer.
  existingOrder <- loadOrder (order' ^. _order_id) (mAccnt ^. _merchantId)

  case existingOrder of
      -- EHS: rework exceptions
      -- EHS: HTTP error codes should exist only on the server level, not in business logic.
    Just orderRef -> throwException err400 {errBody = "Order already created."}
    Nothing       -> doOrderCreate routeParams order' mAccnt

doOrderCreate :: RP.RouteParameters -> Ts.OrderCreateTemplate -> MerchantAccount -> Flow API.OrderCreateResponse
doOrderCreate routeParams order' mAccnt@MerchantAccount{..} = do
  mandateOrder <- isMandateOrder routeParams order' (mAccnt ^. _merchantId)
  order        <- createOrder' routeParams order' mAccnt mandateOrder
  mbReseller   <- loadReseller (mAccnt ^. _resellerId)
  -- EHS: config should be requested on the start of the logic and passed purely.
  cfg          <- runIO Config.getECRConfig

  let orderResponse = mkOrderResponse cfg order mbReseller

  -- EHS: magic constants
  let version = RP.lookupRP @RP.Version routeParams
  if version >= Just "2018-07-01" &&
     (order' ^. _options_get_client_auth_token == Just True)
    then do
      orderTokenData <- acquireOrderToken (order ^. _id) orderCreateReq merchantId
      pure $ updateResponse order' orderResponse orderTokenData
    else pure orderResponse

updateResponse :: Ts.OrderCreateTemplate-> API.OrderCreateResponse -> OrderTokenResp -> API.OrderCreateResponse
updateResponse Ts.OrderCreateTemplate{..} apiResp orderTokenResp = apiResp
    { API.status          = NEW
    , API.status_id       = orderStatusToInt NEW      -- EHS: this logic should be tested.
    , API.juspay          = Just orderTokenResp
    , API.udf1            = udf1 <|> Just ""
    , API.udf2            = udf2 <|> Just ""
    , API.udf3            = udf3 <|> Just ""
    , API.udf4            = udf4 <|> Just ""
    , API.udf5            = udf5 <|> Just ""
    , API.udf6            = udf6 <|> Just ""
    , API.udf7            = udf7 <|> Just ""
    , API.udf8            = udf8 <|> Just ""
    , API.udf9            = udf9 <|> Just ""
    , API.udf10           = udf10 <|> Just ""
    , API.return_url      = returnUrl <|> Just ""
    , API.refunded        = refundedEntirely <|> Just False
    , API.product_id      = productId <|> Just ""
    , API.merchant_id     = merchantId <|> Just ""
    , API.date_created    = Just $ dateCreated
    , API.customer_phone  = customerPhone <|> Just ""
    , API.customer_id     = customerId <|> Just ""
    , API.customer_email  = customerEmail <|> Just ""
    , API.currency        = currency <|> Just ""
    , API.amount_refunded = amountRefunded <|> Just 0.0
    , API.amount          = amount <|> Just 0.0
    }

-- EHS: previously addOrderToken
acquireOrderToken :: OrderPId -> MerchantId -> Flow OrderTokenResp
acquireOrderToken orderPId merchantId = do
  TokenizedResource {token, expiry} <- tokenizeResource (SC.ResourceInt orderPId) "ORDER" merchantId

  -- EHS: check this
  runIO $ Metric.incrementClientAuthTokenGeneratedCount merchantId

  pure $ OrderTokenResp
    { client_auth_token        = Just token
    , client_auth_token_expiry = Just expiry
    }

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


-- EHS: this function is really bad. Rework
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
  num <- pure $ readMayT str
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
  res <- withDB eulerDB $ do
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

createOrder' :: RP.RouteParameters -> Ts.OrderCreateTemplate -> MerchantAccount -> Bool -> Flow D.Order
createOrder' routeParams order' mAccnt isMandate = do

  let merchantId = mAccnt ^. _merchantId
  -- EHS: rework this function.
  merchantPrefs <- loadMerchantPrefs merchantId
  let currency' = (order' ^. _currency)
        <|> defaultCurrency merchantPrefs
        <|> Just INR

  mbCustomer                <- loadCustomer (order' ^. _customerId) (mAccnt ^. _id)
  let billingAddrHolder     = fillBillingAddressHolder mbCustomer (order' ^. _billingAddrHolder)
  let customerContacts = mkCustContacts mbCustomer order'
  mbBillingAddrId           <- updateAddress (order' ^. _billingAddr) billingAddrHolder
  -- EHS: not sure why we don't load customer info for shipping addr as we did for billing addr.
  mbShippingAddrId          <- updateAddress (order' ^. _shippingAddr) (order' ^. _shippingAddrHolder)

  -- EHS: why findMaybeByMerchantAccountAndCustId does nothing?
  -- maybeCustomer  <- maybe (pure Nothing) (\x -> findMaybeByMerchantAccountAndCustId x (mAccnt ^. _id)) customer_id
  -- EHS: Situation: we're updated addresses in the DB when realized the request is invalid.
  -- EHS: This is essentially a security problem.
  -- EHS: This rule should be enforced earlier, before any updates.
  mbCustomerId <- case (customer ^. _id, orderType == MANDATE_REGISTER) of
      (Nothing, True) -> throwException $ err500 {errBody = "Customer not found"}
      (mbCId, _)      -> pure mbCId

  order <- saveOrder order' mAccnt currency' mbBillingAddrId mbShippingAddrId billingAddrCustomer customerContacts
  _     <- saveOrderMetadata routeParams order

  -- EHS: rework this. Skipping for now.
  let orderId = order ^. _orderId
  _ <- setCacheWithExpiry (merchantId <> "_orderid_" <> orderId) order Config.orderTtl
  _ <- when isMandate (setMandateInCache orderCreateReq (order ^. _id) orderId merchantId)
  pure order

fillBillingAddressHolder :: Maybe Ts.CustomerTemplate -> Ts.AddressHolderTemplate -> Ts.AddressHolderTemplate
fillBillingAddressHolder Nothing addressHolder = addressHolder
fillBillingAddressHolder (Just (TS.CustomerTemplate _ mbFirstName mbLastName _ _)) (Ts.AddressHolderTemplate {..})
  = Ts.AddressHolderTemplate (firstName <|> mbFirstName) (lastName <|> mbLastName)

-- previously addCustomerInfo
mkCustContacts :: Maybe Ts.CustomerTemplate -> Ts.OrderCreateTemplate -> Ts.CustomerContacts
mkCustContacts Nothing orderCreateTemplate{..} = Ts.CustomerContacts customerEmail customerPhone
mkCustContacts (Just Ts.CustomerTemplate {emailAddress, mobileNumber}) orderCreateTemplate =
  Ts.CustomerContacts
    { customerEmail = customerEmail <|> emailAddress
    , customerPhone = customerPhone <|> Just mobileNumber
    }

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
      mAddr <- withDB eulerDB
          $ insertRowsReturningList
          $ B.insert (order_address eulerDBSchema)
          $ B.insertExpressions [(B.val_ dbAddr) & _id .~ B.default_]
      pure $ (^. _id) =<< (safeHead mAddr)

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

-- EHS: objectReferenceId is customerId ?
-- A: in customer DB table "objectReferenceId" - id from merchant
-- (how merchant identifies customer in their DB eg. by email or mobile number )
-- and "id" - our id. So merchant can use both

-- EHS: previously mkOrder. Didn't do any writings into DB. Now does.
saveOrder
  :: Ts.OrderCreateTemplate
  -> MerchantAccount
  -> Maybe Currency
  -> Maybe Int
  -> Maybe Int
  -> Maybe CustomerId
  -> Ts.CustomerContacts
  -> Flow D.Order
saveOrder
  (order'@Ts.OrderCreateTemplate {..})
  mAccnt
  currency'
  mbBillingAddrId
  mbShippingAddrId
  customerContacts{..}
  mbCustomerId = do

  -- Returns UUID with deleted '-'.
  -- EHS: what are other requirements to UUID? What about curly braces?
  -- EHS: magic constant
  orderUuid      <- ("ordeu_" <> ) <$> getUUID32

  -- EHS: is this time ok?
  orderTimestamp    <- getCurrentTimeUTC

   -- EHS: not used? Check euler-ps
  -- autoRefund     <- pure $ maybe (Just False) Just auto_refund

  let orderRefVal = DB.defaultOrderReference
          { DB.orderId            = Just orderId
          , DB.merchantId         = account ^. _merchantId
          , DB.amount             = Just $ testRoundOff2 amount -- EHS: We need a test on this rounding logic.
          , DB.billingAddressId   = mbBillingAddrId
          , DB.shippingAddressId  = mbShippingAddrId
          , DB.currency           = currency'
          , DB.customerId         = mbCustomerId
          , DB.customerEmail      = customerEmailAddress
          , DB.customerPhone      = customerMobileNumber
          , DB.returnUrl          = returnUrl
          , DB.description        = description
          , DB.orderUuid          = Just orderUuid
          , DB.productId          = productId
          , DB.preferredGateway   = gatewayId >>= lookupGatewayName
          , DB.status             = NEW
          , DB.dateCreated        = orderTimestamp
          , DB.lastModified       = orderTimestamp
          , DB.orderType          = Just orderType
          , DB.mandateFeature     = optionsCreateMandate
          }

  -- EHS: rework it, skipping for now.
  -- It's not clear what this function does.
  -- Doesn't seem to be a right place for it.
  -- now mkCustContacts
 -- addCustomerInfo req account (mapUDFParams req orderRef)

  refs <- withDB eulerDB
      $ insertRowsReturningList
      $ B.insert (order_reference eulerDBSchema)
      $ B.insertExpressions [(B.val_ orderRefVal) & _id .~ B.default_]

  -- EHS: this extraction should be done much easier.
  -- Move it into a combinator based on the `insertRowsReturningList`.
  -- (Better to have id not null)
  -- EHS: rework exception codes
  orderPId <- case refs of
    [(^. _id) -> Just pId] -> pure pId
    [(^. _id) -> Nothing]  -> throwException err500 {errBody = "NO ORDER FOUND"}
    x                      -> throwException err500 {errBody = EHP.show x}

  -- EHS: fill the Order data type, skipping for now.
  -- N.B., orderUUID can't be Nothing
  pure $ D.Order {..}

-- EHS: return domain type instead of DB type.
saveOrderMetadata :: RP.RouteParameters -> D.Order -> Flow OrderMetadataV2
saveOrderMetadata routeParams order = do
  -- EHS: Investigate this TODO.
  -- TODO: FlipKart alone uses useragent and IP address. Lets remove for others
  -- EHS: why not use order's timestamp?
  orderMetadataTimestamp <- getCurrentTimeUTC

  -- EHS: investigate this comment
  -- there we need sourceIP and userAgent from ReaderT config
  sourceAddress' = RP.lookupRP @RP.SourceIP routeParams
  userAgent'     = RP.lookupRP @RP.UserAgent routeParams
  -- EHS: DB type should be denoted exclicitly
  let orderMetadataDB = OrderMetadataV2
        { id = Nothing
        , browser = Nothing
        , browserVersion = Nothing
        , device = Nothing
        , ipAddress = sourceAddress'
        -- , metadata = metaData req
        , metadata = getOrderMetadata order   -- EHS: implement this function
        , mobile = Just False
        , operatingSystem = Nothing
        , orderReferenceId = order ^. _id
        , referer = Nothing
        , userAgent = userAgent'
        , dateCreated = orderMetadataTimestamp
        , lastUpdated = orderMetadataTimestamp
        }

  -- EHS: why we're rejecting the result? What if error?
  rows <- withDB eulerDB
    $ insertRowsReturningList
    $ B.insert (order_metadata_v2 eulerDBSchema)
    $ B.insertExpressions [ B.val_ orderMetadataDB ]    -- EHS: not used? (_id .~ B.default_ )

  -- EHS: this extraction should be done much easier.
  -- Move it into a combinator based on the `insertRowsReturningList`.
  -- (Better to have id not null)
  -- EHS: rework exception codes
  pId <- case rows of
    [(^. _id) -> Just pId] -> pure pId
    x                      -> throwException err500 {errBody = EHP.show x}

  pure $ orderMetadataDB & (_id .~ pId)


-- EHS: should be tested.
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

-- EHS: very bad function.
-- this function set in orderReference
--   customerPhone <- Customer mobileNumber
--   customerEmail <- Customer emailAddress
-- if customer_id present in OrderCreateRequest
-- and some of customer_phone or customer_email are not
-- now fillCustPhoneMail
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

-- EHS: usless, we check this in validation.
validateOrderParams :: OrderCreateRequest -> Flow ()
validateOrderParams OrderCreateRequest {..} = if amount < 1
  then throwException $ err400 {errBody = "Invalid amount"}
  else pure ()

-- EHS: bad function.
-- EHS: previously getOrderId
-- EHS: use validated Order domain type instead of DB OrderReference.
--      In this case this function is useless
getPId :: OrderReference -> Flow Int
getPId OrderReference {id} = case id of
    Just x -> pure x
    _ -> throwException err500 {errBody = "NO ORDER FOUND"}


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

-- EHS: previously handleReseller
-- EHS: return domain type for Reseller instead of DB type
loadReseller :: Maybe Text -> Flow (Maybe ResellerAccount)
loadReseller Nothing = pure Nothing
loadReseller (Just resellerId') = do
  eRes <- withDB eulerDB $ do
    -- EHS: DB types should be qualified or explicitly named.
    let predicate ResellerAccount {resellerId} = resellerId ==. B.val_ resellerId'
    findRow
      $ B.select
      $ B.limit_ 1
      $ B.filter_ predicate
      $ B.all_ (reseller_account eulerDBSchema)

  case eRes of
    Right mbRAcc -> pure mbRAcc
    Left err -> do
      -- EHS: rework error messages
      logError "Find ResellerAccount" $ toText $ P.show err
      -- EHS: rework exceptions
      throwException err500 {errBody = "14"}

-- EHS: API type should not be used in the logic.
-- EHS: previously makeOrderResponse
mkOrderResponse :: Config.Config -> D.Order -> Maybe ResellerAccount -> API.OrderCreateResponse
mkOrderResponse cfg (D.Order {..}) mbResellerAcc = API.defaultOrderCreateResponse
    { API.status        = CREATED
    , API.status_id     = orderStatusToInt CREATED        -- EHS: this logic should be tested.
    , API.id            = orderUuid
    , API.order_id      = orderId
    , API.payment_links = Paymentlinks
        -- EHS: magic constants
        { web    = Just url'
        , mobile = Just $ url' <> "?mobile=true"
        , iframe = Just url'
        }
    }
  where
  -- EHS: magic constants
    mbResellerEndpoint = mbResellerAcc >>= (^. _resellerApiEndpoint)
    url = maybe (cfg ^. _protocol <> "://" <>  cfg ^. _host) EHP.id mbResellerEndpoint
    url' = url <> "/merchant/pay/" <> orderUuid
