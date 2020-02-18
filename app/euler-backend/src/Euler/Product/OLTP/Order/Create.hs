{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns  #-}

module Euler.Product.OLTP.Order.Create where

import EulerHS.Prelude hiding (id, get)
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
-- EHS: it's beter to get rid of this dependency.
-- Rework exceptions. Introduce app specific exceptions.
-- Map to Servant in handlers.
import           Servant.Server (errBody, err500)
import qualified EulerHS.Extra.Validation as V

-- EHS: this dep should be moved somewhere. Additional busines logic for KV DB
import qualified Euler.KVDB.Redis as KVDBExtra (rGet, setCacheWithExpiry)
import           EulerHS.Language
import           WebService.Language

-- EHS: Storage interaction should be extracted from here into separate modules.
-- EHS: Storage namespace should have a single top level module.
import qualified Euler.Storage.Validators.Order as SV
import qualified Euler.Storage.Types as DB
import           Euler.Storage.Types.EulerDB (eulerDBSchema)
import           Euler.Storage.DBConfig

-- EHS: Should not depend on API?
import qualified Euler.API.RouteParameters  as RP
import qualified Euler.API.Order            as API

-- EHS: rework imports. Use top level modules.
import qualified Euler.Common.Types                as D
import qualified Euler.Common.Types.External.Mandate as MEx
import qualified Euler.Common.Types.External.Order as OEx
import qualified Euler.Common.Metric               as Metric
import qualified Euler.Common.Errors.PredefinedErrors as Errs
import qualified Euler.Product.Domain.Order        as D
import           Euler.Product.Domain.MerchantAccount
import           Euler.Product.OLTP.Services.RedisService
import qualified Euler.Product.Domain.Templates    as Ts
import qualified Euler.Config.Config               as Config
import qualified Euler.Config.ServiceConfiguration as SC
import           Euler.Lens

-- EHS: should not depend on API types. Rethink OrderCreateResponse.
orderCreate
  :: RP.RouteParameters
  -> Ts.OrderCreateTemplate
  -> MerchantAccount
  -> Flow API.OrderCreateResponse
orderCreate routeParams order' mAccnt = do

  -- EHS: effectful validation found.
  _               <- validateMandate order' (mAccnt ^. _merchantId)

  -- EHS: loading merchant is a effectful pre-validation procedure.
  --      Should be done on the validation layer.
  mbExistingOrder <- loadOrder (order' ^. _order_id) (mAccnt ^. _merchantId)

  case mbExistingOrder of
    -- EHS: should we throw exception or return a previous order?
    Just orderRef -> throwException $ orderAlreadyCreated (order' ^. _order_id)
    Nothing       -> doOrderCreate routeParams order' mAccnt

doOrderCreate :: RP.RouteParameters -> Ts.OrderCreateTemplate -> MerchantAccount -> Flow API.OrderCreateResponse
doOrderCreate routeParams order' mAccnt@MerchantAccount{..} = do
  order        <- createOrder' routeParams order' mAccnt

  _            <- updateOrderCache order
  _            <- updateMandateCache order

  mbReseller   <- loadReseller (mAccnt ^. _resellerId)
  -- EHS: config should be requested on the start of the logic and passed purely.
  cfg          <- runIO Config.getECRConfig

  let orderResponse = mkOrderResponse cfg order mbReseller

  -- EHS: magic constants
  -- EHS: versioning should be done using a service
  let version = RP.lookupRP @RP.Version routeParams
  if (version >= Just "2018-07-01")
      && (order' ^. _acquireOrderToken)
    then do
      orderToken <- acquireOrderToken (order ^. _id) merchantId
      pure $ mkTokenizedOrderResponse order' orderResponse orderToken
    else pure orderResponse

-- EHS: There is no code reading for order from cache (lookup by "_orderid_" gives nothing).
--      Why this cache exist? What it does?

-- EHS: previously setMandateInCache
-- EHS: Seems mandate_max_amount should always be set irrespective the option mandate.
--      In the previous code, we're expecting max amount to be set even if mandate is DISABLED.
--      It's not clear whether this a valid behaviour (seems not).
-- EHS: There is no code reading for mandate from cache (lookup by "_mandate_data_" gives nothing).
--      Why this cache exist? What it does?
updateMandateCache
  :: D.Order
  -> D.MerchantId
  -> Flow ()
updateMandateCache order = case order ^. _mandate of
    D.MandateDisabled -> pure ()
    D.MandateRequired maxAmount -> updateMandateCache' maxAmount
    D.MandateOptional maxAmount -> updateMandateCache' maxAmount
    -- EHS: Need to do something with this.
    D.MandateReqUndefined       -> error "MandateReqUndefined not handled."
    D.MandateOptUndefined       -> error "MandateReqUndefined not handled."
  where
    updateMandateCache' maxAmount = do
        let merchantId = order ^. _merchantId
        let orderId    = order ^. _orderId
        mandate <- createMandate order maxAmount
        -- EHS: magic constant
        void $ setCacheWithExpiry (merchantId <> "_mandate_data_" <> orderId) mandate Config.mandateTtl

    createMandate :: D.Order -> Double -> Flow DB.Mandate
    createMandate order maxAmount = do
      mandateId   <- getShortUUID
      currentDate <- getCurrentTimeUTC
      token       <- getUUID32
      pure $ DB.Mandate
        { DB.id = Nothing
        , DB.merchantId = order ^. _merchantId
        , DB.currency = order ^. _currency
        , DB.endDate = Nothing
        , DB.startDate = Nothing
        , DB.maxAmount = Just maxAmount
        , DB.merchantCustomerId = order ^. _customerId
        , DB.paymentMethod = Nothing
        , DB.paymentMethodType = Nothing
        , DB.paymentMethodId = Nothing
        , DB.gateway = Nothing
        , DB.gatewayParams = Nothing
        , DB.token = token
        , DB.mandateId = mandateId
        , DB.status = MEx.CREATED
        , DB.authOrderId = Just $ order ^. _id
        , DB.activatedAt = Nothing
        , DB.dateCreated = currentDate
        , DB.lastModified = currentDate
        , DB.authTxnCardInfo = Nothing
        , DB.merchantGatewayAccountId = Nothing
        , DB.metadata = Nothing
        }

mkTokenizedOrderResponse
  :: Ts.OrderCreateTemplate
  -> API.OrderCreateResponse
  -> API.OrderTokenResp
  -> API.OrderCreateResponse
mkTokenizedOrderResponse Ts.OrderCreateTemplate{..} apiResp orderTokenResp = apiResp
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
-- EHS: API type should not be used.
acquireOrderToken :: D.OrderPId -> D.MerchantId -> Flow API.OrderTokenResp
acquireOrderToken orderPId merchantId = do
  -- EHS: magic constant
  TokenizedResource {token, expiry} <- tokenizeResource (SC.ResourceInt orderPId) "ORDER" merchantId

  -- EHS: check this
  runIO $ Metric.incrementClientAuthTokenGeneratedCount merchantId

  pure $ API.OrderTokenResp
    { API.client_auth_token        = Just token
    , API.client_auth_token_expiry = Just expiry
    }

-- EHS: previously isMandateOrder
-- EHS: bad function. Throws exception on invalid data.
validateMandate :: Ts.OrderCreateTemplate -> D.MerchantId -> Flow ()
validateMandate (Ts.OrderCreateTemplate {mandate}) merchantId = do
  case mandate of
    D.MandateDisabled           -> pure ()
    D.MandateRequired maxAmount -> validateMaxAmount maxAmount
    D.MandateOptional maxAmount -> validateMaxAmount maxAmount
    -- EHS: Need to do something with this.
    D.MandateReqUndefined       -> error "MandateReqUndefined not handled."
    D.MandateOptUndefined       -> error "MandateReqUndefined not handled."
  where
    validateMaxAmount maxAmount = do

      -- EHS: magic constant
      -- EHS: getting values unsafely. What if we don't have these values??
      --      What flow should be then?
      mbMaxAmLimitStr <- KVDBExtra.rGet $ merchantId <> "_mandate_max_amount_limit"

      -- EHS: awful conversion. Rework.
      maxAmLimit <- fromStringToNumber $ maybe Config.mandateMaxAmountAllowed id mbMaxAmLimitStr

      when (maxAmount <= 0.0 || maxAmount > maxAmLimit)
        $ throwException
        $ invalidMandateMaxAmount maxAmLimit


-- EHS: bad function. Get rid of it.
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
loadMerchantPrefs :: D.MerchantId -> Flow DB.MerchantIframePreferences
loadMerchantPrefs merchantId' = do
  res <- withDB eulerDB $ do
    let predicate DB.MerchantIframePreferences {merchantId} = merchantId ==. (B.val_ merchantId')
    findRow
      $ B.select
      $ B.limit_ 1
      $ B.filter_ predicate
      $ B.all_ (merchant_iframe_preferences eulerDBSchema)

  case res of
    Just mIPrefs -> pure mIPrefs
    Nothing -> do
      logError "merchant_iframe_preferences" $ "Not found for merchant " <> merchantId'
      throwException Errs.internalError    -- EHS: error should be specified.

createOrder'
  :: RP.RouteParameters
  -> Ts.OrderCreateTemplate
  -> MerchantAccount
  -> Flow D.Order
createOrder' routeParams order' mAccnt = do

  merchantPrefs <- loadMerchantPrefs merchantId

  let merchantId = mAccnt ^. _merchantId
  let currency' = (order' ^. _currency)
        <|> defaultCurrency merchantPrefs
        <|> Just INR

  mbLoadedCustomer <- loadCustomer (order' ^. _customerId) (mAccnt ^. _id)
  let mbCustomer = fillCustomerContacts order' mbLoadedCustomer

  -- EHS: strange validation in the middle of logic.
  case (mbCustomer >>= (^. _customerId), (order' ^. _orderType) == MANDATE_REGISTER) of
      -- EHS: misleading error message. Should explicitly state that customerId has to be set on MANDATE_REGISTER
      (Nothing, True) -> throwException customerNotFound
      (mbCId, _)      -> pure ()

  let billingAddrHolder     = fillBillingAddressHolder mbCustomer (order' ^. _billingAddrHolder)
  mbBillingAddrId           <- createAddress (order' ^. _billingAddr) billingAddrHolder
  -- EHS: not sure why we don't load customer info for shipping addr as we did for billing addr.
  mbShippingAddrId          <- createAddress (order' ^. _shippingAddr) (order' ^. _shippingAddrHolder)

  order <- saveOrder order' mAccnt currency' mbBillingAddrId mbShippingAddrId billingAddrCustomer mbCustomer
  _     <- saveOrderMetadata routeParams (order .^ _id) (order' .^ _metadata)

  pure order

fillCustomerContacts
  :: Maybe Ts.CustomerTemplate
  -> Ts.OrderCreateTemplate
  -> Maybe Ts.CustomerTemplate
fillCustomerContacts Nothing _ = Nothing
fillCustomerContacts (Just customer) (Ts.OrderCreateTemplate {..}) =
  Just $ customer
    { Ts.email        = customerEmail <|> (customer ^. _email)
    , Ts.mobileNumber = fromMaybe (customer ^. _mobileNumber) customerPhone
    }

fillBillingAddressHolder :: Maybe Ts.CustomerTemplate -> Ts.AddressHolderTemplate -> Ts.AddressHolderTemplate
fillBillingAddressHolder Nothing addressHolder = addressHolder
fillBillingAddressHolder (Just customer) (Ts.AddressHolderTemplate {..})
  = Ts.AddressHolderTemplate
      (firstName <|> (customer ^. _firstName))
      (lastName <|> (customer ^. _lastName))

loadCustomer :: Maybe D.CustomerId -> MerchantAccountId -> Flow (Maybe Ts.CustomerTemplate)
loadCustomer Nothing _ = pure Nothing
loadCustomer (Just customerId) mAccntId = do

    -- EHS: after refactoring, we query DB every time when customerId is available.
    -- EHS: DB type Customer should be explicit or qualified
    mbCustomer :: Maybe DB.Customer <- withDB eulerDB $ do
      let predicate DB.Customer {merchantAccountId, id, objectReferenceId}
            = (   id ==.  (B.val_ customerId)
              ||. objectReferenceId ==. (B.val_ customerId)  -- EHS: objectReferenceId is customerId ?
                                                             -- A: in customer DB table "objectReferenceId" - id from merchant
                                                             -- (how merchant identifies customer in their DB eg. by email or mobile number )
                                                             -- and "id" - our id. So merchant can use both
              )
              &&. (merchantAccountId ==. (B.val_ mAccntId))

      findRow
        $ B.select
        $ B.filter_ predicate
        $ B.all_ (customer eulerDBSchema)

    pure $ case mbCustomer of
      Just (DB.Customer {..}) -> Just $ Ts.CustomerTemplate
        customerId
        firstName
        lastName
        emailAddress
        -- mobileCountryCode
        mobileNumber
      Nothing              -> Nothing


createAddress :: Ts.AddressTemplate -> Ts.AddressHolderTemplate -> Flow (Maybe D.AddressId)
createAddress addr addrHolder =
  case toDBAddress addr addrHolder of
    Nothing -> pure Nothing
    Just dbAddr -> do
      mAddr <- withDB eulerDB
          $ insertRowsReturningList
          $ B.insert (order_address eulerDBSchema)
          $ B.insertExpressions [(B.val_ dbAddr) & _id .~ B.default_]
      pure $ (^. _id) =<< (safeHead mAddr)

-- EHS: DB types should be denoted explicitly.
toDBAddress :: Ts.AddressTemplate -> Ts.AddressHolderTemplate -> Maybe DB.OrderAddress
toDBAddress (Ts.AddressTemplate {..}) (Ts.AddressHolderTemplate {..})
  | nothingSet = Nothing
  | otherwise = Just $ DB.OrderAddress
        { DB.id             = Nothing  -- in DB it's not Null, Auto increment
        , DB.version        = 1        -- defaultVersion
      -- from src/Config/Constants.purs
      --  defaultVersion :: Int
      --  defaultVersion = 1

        -- EHS: fill address
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

loadOrder :: D.OrderId -> D.MerchantId -> Flow (D.Order)
loadOrder orderId' merchantId' = do
    mbOrderRef <- withDB eulerDB $ do
      let predicate DB.OrderReference {orderId, merchantId} =
            (orderId    ==. B.just_ (B.val_ orderId')) &&.
            (merchantId ==. B.just_ (B.val_ merchantId'))
      findRow
        $ B.select
        $ B.limit_ 1
        $ B.filter_ predicate
        $ B.all_ (DB.order_reference eulerDBSchema)
    case mbOrderRef of
      Nothing -> pure Nothing
      Just ordRef -> do
        case SV.transSOrderToDOrder ordRef of
          V.Success order -> pure $ Just order
          V.Failure e -> do
            logError "Incorrect order in DB"
              $  "orderId: "    <> orderId'
              <> "merchantId: " <> merchantId'
              <> "error: "      <> show e
            throwException Errs.internalError

-- EHS: objectReferenceId is customerId ?
-- A: in the Customer DB table "objectReferenceId" - id from merchant
-- (how merchant identifies customer in their DB eg. by email or mobile number )
-- and "id" - our id. So merchant can use both

saveOrder
  :: Ts.OrderCreateTemplate
  -> MerchantAccount
  -> Maybe D.Currency
  -> Maybe Int
  -> Maybe Int
  -> Maybe Ts.CustomerTemplate
  -> Flow D.Order
saveOrder
  (order'@Ts.OrderCreateTemplate {..})
  mAccnt
  mbCurrency
  mbBillingAddrId
  mbShippingAddrId
  mbCustomer = do

  -- EHS: what are other requirements to UUID? UUID functions can be found in src/Utils/PrestoBackend.purs
  -- EHS: magic constant
  orderUuid         <- ("ordeu_" <> ) <$> getUUID32

  -- EHS: is this time ok?
  orderTimestamp    <- getCurrentTimeUTC

  -- EHS: TODO: add UDF fields
  let orderRefVal' = DB.defaultOrderReference
          { DB.orderId            = Just orderId
          , DB.merchantId         = mAccnt ^. _merchantId
          , DB.amount             = Just $ fromMoney amount
          , DB.billingAddressId   = mbBillingAddrId
          , DB.shippingAddressId  = mbShippingAddrId
          , DB.currency           = mbCurrency
          , DB.customerId         = mbCustomer >>= (^. _customerId)
          , DB.customerEmail      = mbCustomer >>= (^. _email)
          , DB.customerPhone      = mbCustomer >>= (\c -> Just $ c ^. _mobileNumber)
          , DB.returnUrl          = returnUrl
          , DB.description        = description
          , DB.orderUuid          = Just orderUuid
          , DB.productId          = productId
          , DB.preferredGateway   = gatewayId >>= D.lookupGatewayName
          , DB.status             = OEx.NEW
          , DB.dateCreated        = orderTimestamp
          , DB.lastModified       = orderTimestamp
          , DB.orderType          = Just orderType
          , DB.mandateFeature     = D.toMandateEx mandate
          }
  let orderRefVal = fillUDFParams order' orderRefVal'

  orderRef <- unsafeInsertRow (err500 {errBody = "Inserting order reference failed."}) eulerDB
      $ B.insert (DB.order_reference eulerDBSchema)
      $ B.insertExpressions [(B.val_ orderRefVal) & _id .~ B.default_]

  -- EHS: should not happen, ideally.
  -- EHS: should we skip the validation and just return the orderRefVal updated with the id from insertion?
  case SV.transSOrderToDOrder orderRef of
    V.Success order -> pure order
    V.Failure e -> do
      logError $ "Unexpectedly got an invalid order reference after save: " <> show orderRef
      throwException Errs.internalError


-- EHS: return domain type instead of DB type.
saveOrderMetadata :: RP.RouteParameters -> D.OrderPId -> Maybe Text -> Flow DB.OrderMetadataV2
saveOrderMetadata routeParams orderPId metadata' = do
  -- EHS: Investigate this TODO.
  -- TODO: FlipKart alone uses useragent and IP address. Lets remove for others
  -- EHS: why not use order's timestamp?
  orderMetadataTimestamp <- getCurrentTimeUTC

  let sourceAddress' = RP.lookupRP @RP.SourceIP routeParams
  let userAgent'     = RP.lookupRP @RP.UserAgent routeParams
  -- EHS: DB type should be denoted exclicitly
  let orderMetadataDB = DB.OrderMetadataV2
        { DB.id = Nothing
        , DB.browser = Nothing
        , DB.browserVersion = Nothing
        , DB.device = Nothing
        , DB.ipAddress = sourceAddress'
        , DB.metadata = metadata'
        , DB.mobile = Just False
        , DB.operatingSystem = Nothing
        , DB.orderReferenceId = Just orderPId
        , DB.referer = Nothing
        , DB.userAgent = userAgent'
        , DB.dateCreated = orderMetadataTimestamp
        , DB.lastUpdated = orderMetadataTimestamp
        }

  mbOrderMetadata :: Maybe DB.OrderMetadataV2 <- safeHead <$> withDB eulerDB
    $ insertRowsReturningList
    $ B.insert (DB.order_metadata_v2 eulerDBSchema)
    $ B.insertExpressions [ (B.val_ orderMetadataDB) & _id .~ B.default_ ]

  pure $ orderMetadataDB & (_id .~ orderPId)


cleanUp :: Maybe Text -> Maybe Text
cleanUp mStr =  cleanUpSpecialChars <$>  mStr

cleanUpSpecialChars :: Text -> Text
cleanUpSpecialChars = Text.filter (`P.notElem` ("~!#%^=+\\|:;,\"'()-.&/" :: String))
-- from src/Euler/Utils/Utils.js
-- exports.cleanUpSpecialChars = function(val){
--   return val.replace(/[\\\\~!#%^=+\\|:;,\"'()-.&/]/g,"");
-- }

-- EHS: previously mapUDFParams
fillUDFParams :: Ts.OrderCreateTemplate -> DB.OrderReference -> DB.OrderReference
fillUDFParams order' orderRef = smash newUDF orderRef
  where
    (reqUDF :: D.UDF) = upcast order'
    newUDF' D.UDF {..} = D.UDF
      { D.udf1 = cleanUp udf1
      , D.udf2 = cleanUp udf2
      , D.udf3 = cleanUp udf3
      , D.udf4 = cleanUp udf4
      , D.udf5 = cleanUp udf5
      , ..
      }
    newUDF = newUDF' reqUDF
  -- EHS: Why are udf fields 1-5 cleaned and the rest not?

-- EHS: previously handleReseller
-- EHS: return domain type for Reseller instead of DB type
loadReseller :: Maybe Text -> Flow (Maybe DB.ResellerAccount)
loadReseller Nothing = pure Nothing
loadReseller (Just resellerId') = withDB eulerDB $ do
    -- EHS: DB types should be qualified or explicitly named.
    let predicate DB.ResellerAccount {resellerId} = resellerId ==. B.val_ resellerId'
    findRow
      $ B.select
      $ B.limit_ 1
      $ B.filter_ predicate
      $ B.all_ (DB.reseller_account eulerDBSchema)

-- EHS: API type should not be used in the logic.
-- EHS: previously makeOrderResponse
mkOrderResponse :: Config.Config -> D.Order -> Maybe DB.ResellerAccount -> API.OrderCreateResponse
mkOrderResponse cfg (D.Order {..}) mbResellerAcc = API.defaultOrderCreateResponse
    { API.status        = OEx.CREATED
    , API.status_id     = OEx.orderStatusToInt OEx.CREATED
    , API.id            = orderUuid
    , API.order_id      = orderId
    , API.payment_links = API.Paymentlinks
        -- EHS: magic constants
        { API.web    = Just url'
        , API.mobile = Just $ url' <> "?mobile=true"
        , API.iframe = Just url'
        }
    }
  where
  -- EHS: magic constants
    mbResellerEndpoint = mbResellerAcc >>= (^. _resellerApiEndpoint)
    url = maybe (cfg ^. _protocol <> "://" <>  cfg ^. _host) EHP.id mbResellerEndpoint
    url' = url <> "/merchant/pay/" <> orderUuid
