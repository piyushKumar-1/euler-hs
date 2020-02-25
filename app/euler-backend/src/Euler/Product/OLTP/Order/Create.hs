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
import           Servant.Server (errBody, err500, err400)
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
import qualified Euler.API.Validators.Order as VO

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
import           Euler.Constant.Constants (defaultVersion)
import           Euler.Lens



-- EHS: should not depend on API types. Rethink OrderCreateResponse.
-- EHS: move methods launchers with validations to separate module ?
orderCreate
  :: RP.RouteParameters
  -> API.OrderCreateRequest
  -> MerchantAccount
  -> Flow API.OrderCreateResponse
orderCreate rp req ma = do
  case VO.transApiOrdCreateToOrdCreateT req of
    V.Failure err -> do
      logError "OrderCreateRequest validation" $ show err
      throwException $ Errs.mkValidationError err
    V.Success validatedOrder -> orderCreate'' rp validatedOrder ma

orderCreate''
  :: RP.RouteParameters
  -> Ts.OrderCreateTemplate
  -> MerchantAccount
  -> Flow API.OrderCreateResponse
orderCreate'' routeParams order' mAccnt = do

  -- EHS: effectful validation found.
  _               <- validateMandate order' (mAccnt ^. _merchantId)

  -- EHS: loading merchant is a effectful pre-validation procedure.
  --      Should be done on the validation layer.
  mbExistingOrder <- loadOrder (order' ^. _orderId) (mAccnt ^. _merchantId)

  case mbExistingOrder of
    -- EHS: should we throw exception or return a previous order?
    Just orderRef -> throwException $ Errs.orderAlreadyCreated (order' ^. _orderId)
    Nothing       -> doOrderCreate routeParams order' mAccnt

doOrderCreate :: RP.RouteParameters -> Ts.OrderCreateTemplate -> MerchantAccount -> Flow API.OrderCreateResponse
doOrderCreate routeParams order' mAccnt@MerchantAccount{..} = do
  merchantPrefs <- loadMerchantPrefs merchantId
  order         <- createOrder' routeParams order' mAccnt merchantPrefs

  _             <- updateOrderCache order
  _             <- updateMandateCache order -- maybe pass mandate from OrderCreateTemplate?

  mbReseller    <- loadReseller (mAccnt ^. _resellerId)
  -- EHS: config should be requested on the start of the logic and passed purely.
  cfg           <- runIO Config.getECRConfig

  -- EHS: magic constants
  -- EHS: versioning should be done using a service
  let version = RP.lookupRP @RP.Version routeParams
  let thatVersion = (version >= Just "2018-07-01") && (order' ^. _orderTokenNeeded)
  case thatVersion of
    True  -> mkTokenizedOrderResponse cfg order mAccnt mbReseller
    False -> mkOrderResponse          cfg order mAccnt mbReseller

-- EHS: There is no code reading for order from cache (lookup by "_orderid_" gives nothing).
--      Why this cache exist? What it does?
updateOrderCache :: D.Order -> Flow ()
updateOrderCache order = do
  let orderId    = order ^. _orderId
  let merchantId = order ^. _merchantId
  -- EHS: magic constant.
  void $ KVDBExtra.setCacheWithExpiry (merchantId <> "_orderid_" <> orderId) order Config.orderTtl

-- EHS: previously setMandateInCache
-- EHS: Seems mandate_max_amount should always be set irrespective the option mandate.
--      In the previous code, we're expecting max amount to be set even if mandate is DISABLED.
--      It's not clear whether this a valid behaviour (seems not).
-- EHS: There is no code reading for mandate from cache (lookup by "_mandate_data_" gives nothing).
--      Why this cache exist? What it does?
updateMandateCache :: D.Order -> Flow ()
updateMandateCache order = case order ^. _mandate of
    D.MandateDisabled           -> pure ()
    D.MandateRequired maxAmount -> updateMandateCache' maxAmount
    D.MandateOptional maxAmount -> updateMandateCache' maxAmount
    -- EHS: Need to do something with this.
    D.MandateReqUndefined       -> pure () --error "MandateReqUndefined not handled."
    D.MandateOptUndefined       -> pure () --error "MandateReqUndefined not handled."
  where
    updateMandateCache' ma = do
        let merchantId = order ^. _merchantId
        let orderId    = order ^. _orderId
        mandate <- createMandate order ma
        -- EHS: magic constant
        void $ KVDBExtra.setCacheWithExpiry (merchantId <> "_mandate_data_" <> orderId) mandate Config.mandateTtl

    createMandate :: D.Order -> Double -> Flow DB.Mandate
    createMandate order maxAmount = do
      mandateId   <- getShortUUID
      currentDate <- getCurrentTimeUTC
      token       <- getUUID32
      pure $ DB.Mandate
        { DB.id = Nothing
        , DB.merchantId = order ^. _merchantId
        , DB.currency = Just $ order ^. _currency
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
      maxAmLimit      <- fromStringToNumber $ fromMaybe Config.mandateMaxAmountAllowed mbMaxAmLimitStr

      when (maxAmount <= 0.0 || maxAmount > maxAmLimit)
        $ throwException
        $ Errs.invalidMandateMaxAmount maxAmLimit
    -- EHS: bad function. Get rid of it.
    fromStringToNumber :: Text -> Flow Double
    fromStringToNumber str = do
      num <- pure $ readMayT str
      case num of
        Just x -> pure $ x
        Nothing -> throwException $ err400 {errBody = "Invalid amount"}

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
      $ B.all_ (DB.merchant_iframe_preferences eulerDBSchema)

  case res of
    Just mIPrefs -> pure mIPrefs
    Nothing -> do
      logError "merchant_iframe_preferences" $ "Not found for merchant " <> merchantId'
      throwException Errs.internalError    -- EHS: error should be specified.

createOrder'
  :: RP.RouteParameters
  -> Ts.OrderCreateTemplate
  -> MerchantAccount
  -> DB.MerchantIframePreferences
  -> Flow D.Order
createOrder' routeParams order' mAccnt merchantPrefs = do
  let currency' = (order' ^. _currency)
        <|> DB.defaultCurrency merchantPrefs
        <|> Just D.INR

  mbLoadedCustomer <- loadCustomer (order' ^. _customerId) (mAccnt ^. _id)
  let mbCustomer = fillCustomerContacts order' mbLoadedCustomer

  -- EHS: strange validation in the middle of logic.
  case (mbCustomer >>= (\c -> Just $ c ^. _customerId), (order' ^. _orderType) == D.MANDATE_REGISTER) of
      -- EHS: misleading error message. Should explicitly state that customerId has to be set on MANDATE_REGISTER
      (Nothing, True) -> throwException Errs.customerNotFound
      (mbCId, _)      -> pure ()

  let billingAddrHolder     = fillBillingAddressHolder mbCustomer (order' ^. _billingAddrHolder)
  mbBillingAddrId           <- createAddress (order' ^. _billingAddr) billingAddrHolder
  -- EHS: not sure why we don't load customer info for shipping addr as we did for billing addr.
  mbShippingAddrId          <- createAddress (order' ^. _shippingAddr) (order' ^. _shippingAddrHolder)

  order <- saveOrder order' mAccnt currency' mbBillingAddrId mbShippingAddrId mbCustomer
  _     <- saveOrderMetadata routeParams (order ^. _id) (order' ^. _metadata)

  pure order

fillCustomerContacts
  :: Ts.OrderCreateTemplate
  -> Maybe Ts.CustomerTemplate
  -> Maybe Ts.CustomerTemplate
fillCustomerContacts _ Nothing = Nothing
fillCustomerContacts (Ts.OrderCreateTemplate {..}) (Just customer) =
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
            = (   id ==.  (B.val_ (Just customerId))
              ||. objectReferenceId ==. (B.val_ (Just customerId))  -- EHS: objectReferenceId is customerId ?
                                                             -- A: in customer DB table "objectReferenceId" - id from merchant
                                                             -- (how merchant identifies customer in their DB eg. by email or mobile number )
                                                             -- and "id" - our id. So merchant can use both
              )
              &&. (merchantAccountId ==. (B.val_ mAccntId))

      findRow
        $ B.select
        $ B.filter_ predicate
        $ B.all_ (DB.customer eulerDBSchema)

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
  case toDBAddress Nothing addr addrHolder of
    Nothing -> pure Nothing
    Just dbAddr -> do
      mAddr <- withDB eulerDB
          $ insertRowsReturningList
          $ B.insert (DB.order_address eulerDBSchema)
          $ B.insertExpressions [(B.val_ dbAddr) & _id .~ B.default_]
      pure $ (^. _id) =<< (safeHead mAddr)

-- EHS: DB types should be denoted explicitly.
--toDBAddress' :: Ts.AddressTemplate -> Ts.AddressHolderTemplate -> Maybe DB.OrderAddress
--toDBAddress' (Ts.AddressTemplate {..}) (Ts.AddressHolderTemplate {..})
--  | nothingSet = Nothing
--  | otherwise = Just $ DB.OrderAddress
--        { DB.id             = Nothing  -- in DB it's not Null, Auto increment
--        , DB.version        = 1        -- defaultVersion
--      -- from src/Config/Constants.purs
--      --  defaultVersion :: Int
--      --  defaultVersion = 1
--
--
--        }
--  where
--    nothingSet = isNothing
--      $   firstName
--      <|> lastName
--      <|> line1
--      <|> line2
--      <|> line3
--      <|> city
--      <|> state
--      <|> country
--      <|> postalCode
--      <|> phone
--      <|> countryCodeIso

toDBAddress :: Maybe Ts.CustomerTemplate -> Ts.AddressTemplate -> Ts.AddressHolderTemplate -> Maybe DB.OrderAddress
toDBAddress mCT aT@Ts.AddressTemplate {..} ahT
  | addressEmpty aT ahT = Nothing
  | otherwise = Just $ DB.OrderAddress
        { DB.id             = Nothing  -- in DB it's not Null, Auto increment
        , DB.version        = defaultVersion
        , DB.firstName      = firstName
        , DB.lastName       = lastName
        , DB.line1          = line1
        , DB.line2          = line2
        , DB.line3          = line3
        , DB.city           = city
        , DB.state          = state
        , DB.country        = country
        , DB.countryCodeIso = countryCodeIso
        , DB.postalCode     = postalCode
        , DB.phone          = phone
        }
  where
    Ts.AddressHolderTemplate {..} = fillBillingAddressHolder mCT ahT

addressEmpty :: Ts.AddressTemplate -> Ts.AddressHolderTemplate -> Bool
addressEmpty (Ts.AddressTemplate {..}) (Ts.AddressHolderTemplate {..}) =
  isNothing
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

loadOrder :: D.OrderId -> D.MerchantId -> Flow (Maybe D.Order)
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
              $  " orderId: "    <> orderId'
              <> " merchantId: " <> merchantId'
              <> " error: "      <> show e
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


  let orderRefVal = fillUDFParams order' $ DB.defaultOrderReference
          { DB.orderId            = Just orderId
          , DB.merchantId         = Just $ mAccnt ^. _merchantId
          , DB.amount             = Just $ D.fromMoney amount
          , DB.billingAddressId   = mbBillingAddrId
          , DB.shippingAddressId  = mbShippingAddrId
          , DB.currency           = mbCurrency
          , DB.customerId         = mbCustomer >>= (\c -> Just $ c ^. _customerId)
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
          , DB.mandateFeature     = Just $ D.toMandateEx mandate
          }
 -- let orderRefVal = fillUDFParams order' orderRefVal'

  orderRef <- unsafeInsertRow (err500 {errBody = "Inserting order reference failed."}) eulerDB
      $ B.insert (DB.order_reference eulerDBSchema)
      $ B.insertExpressions [(B.val_ orderRefVal) & _id .~ B.default_]

  -- EHS: should not happen, ideally.
  -- EHS: should we skip the validation and just return the orderRefVal updated with the id from insertion?
  case SV.transSOrderToDOrder orderRef of
    V.Success order -> pure order
    V.Failure e -> do
      logError "orderCreate" $ "Unexpectedly got an invalid order reference after save: " <> show orderRef
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
  let orderMetadataDBVal = DB.OrderMetadataV2
        { DB.id = Nothing
        , DB.browser = Nothing
        , DB.browserVersion = Nothing
        , DB.device = Nothing
        , DB.ipAddress = sourceAddress'
        , DB.metadata = metadata'
        , DB.mobile = Just False
        , DB.operatingSystem = Nothing
        , DB.orderReferenceId = orderPId
        , DB.referer = Nothing
        , DB.userAgent = userAgent'
        , DB.dateCreated = orderMetadataTimestamp
        , DB.lastUpdated = orderMetadataTimestamp
        }

  -- mbOrderMetadata :: Maybe DB.OrderMetadataV2 <- safeHead <$> withDB eulerDB
  --   $ insertRowsReturningList
  orderMetadataDB <- unsafeInsertRow (err500 {errBody = "Inserting order reference failed."}) eulerDB
    $ B.insert (DB.order_metadata_v2 eulerDBSchema)
    $ B.insertExpressions [ (B.val_ orderMetadataDBVal) & _id .~ B.default_ ]

  -- EHS: this is extremely suspicious. Substituting id from DB by orderPid
  pure $ orderMetadataDB & (_id .~ Just orderPId)

-- EHS: previously mapUDFParams
fillUDFParams :: Ts.OrderCreateTemplate -> DB.OrderReference -> DB.OrderReference
fillUDFParams Ts.OrderCreateTemplate{udf} orderRef = smash udf orderRef

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

mkOrderResponse
  :: Config.Config
  -> D.Order
  -> MerchantAccount
  -> Maybe DB.ResellerAccount
  -> Flow API.OrderCreateResponse
mkOrderResponse cfg (D.Order {..}) _ mbResellerAcc = do
  let (r :: API.OrderCreateResponse) = API.defaultOrderCreateResponse
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
        , API.return_url    = returnUrl
        , API.refunded      = Just refundedEntirely
        , API.product_id    = productId
        , API.merchant_id   = Just merchantId
        , API.date_created  = Just dateCreated
        , API.customer_phone = customerPhone
        , API.customer_id   = customerId
        , API.customer_email = customerEmail
        , API.currency      = Just $ show currency
        , API.amount_refunded = amountRefunded
        , API.amount = Just $ D.fromMoney amount
        , API.juspay = Nothing
        }
  pure r
  where
  -- EHS: magic constants
    mbResellerEndpoint = mbResellerAcc >>= (^. _resellerApiEndpoint)
    url = maybe (cfg ^. _protocol <> "://" <>  cfg ^. _host) EHP.id mbResellerEndpoint
    url' = url <> "/merchant/pay/" <> orderUuid


mkTokenizedOrderResponse
  :: Config.Config
  -> D.Order
  -> MerchantAccount
  -> Maybe DB.ResellerAccount
  -> Flow API.OrderCreateResponse
mkTokenizedOrderResponse cfg order@(D.Order {..}) mAcc mbResellerAcc = do
  apiResp    <- mkOrderResponse cfg order mAcc mbResellerAcc
  orderToken <- acquireOrderToken (order ^. _id) merchantId
  let (r :: API.OrderCreateResponse) = apiResp
        { API.status          = OEx.NEW
        , API.status_id       = OEx.orderStatusToInt OEx.NEW
        , API.juspay          = Just orderToken
        , API.udf1            = (D.udf1  udf) <|> Just ""
        , API.udf2            = (D.udf2  udf) <|> Just ""
        , API.udf3            = (D.udf3  udf) <|> Just ""
        , API.udf4            = (D.udf4  udf) <|> Just ""
        , API.udf5            = (D.udf5  udf) <|> Just ""
        , API.udf6            = (D.udf6  udf) <|> Just ""
        , API.udf7            = (D.udf7  udf) <|> Just ""
        , API.udf8            = (D.udf8  udf) <|> Just ""
        , API.udf9            = (D.udf9  udf) <|> Just ""
        , API.udf10           = (D.udf10 udf) <|> Just ""
        , API.return_url      = returnUrl <|> Just ""
        , API.refunded        = Just refundedEntirely
        , API.product_id      = productId <|> Just ""
        , API.merchant_id     = Just merchantId
        , API.date_created    = Just dateCreated
        , API.customer_phone  = customerPhone <|> Just ""
        , API.customer_id     = customerId <|> Just ""
        , API.customer_email  = customerEmail <|> Just ""
        , API.currency        = (Just $ show currency) -- Is show valid here?
        -- , API.amount_refunded = amountRefunded <|> Just 0.0 -- EHS: where this shoud be taken from on order create??
        , API.amount_refunded = Just 0.0
        , API.amount          = Just (D.fromMoney amount) -- <|> Just 0.0
        }
  pure r
