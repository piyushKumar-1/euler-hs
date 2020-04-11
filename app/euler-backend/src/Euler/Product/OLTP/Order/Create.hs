{-# LANGUAGE NamedFieldPuns  #-}

module Euler.Product.OLTP.Order.Create where

import           EulerHS.Prelude hiding (get, id)

import qualified Data.Text            as Text
import           Data.Time (localDay)
import qualified Text.Read            as TR
import           Data.Generics.Product.Subtype

-- EHS: it's beter to get rid of this dependency.
-- Rework exceptions. Introduce app specific exceptions.
-- Map to Servant in handlers.
import           Servant.Server (errBody, err400)
import qualified EulerHS.Extra.Validation as V

import           EulerHS.Language
import           WebService.Language

-- EHS: Storage interaction should be extracted from here into separate modules.
-- EHS: Storage namespace should have a single top level module.
import qualified Euler.Storage.Types        as DB
import qualified Euler.Storage.KVRepository as Rep
import qualified Euler.Storage.Repository   as Rep

-- EHS: Should not depend on API?
import qualified Euler.API.Order as API
import qualified Euler.API.RouteParameters as RP
import qualified Euler.API.Validators.Order as VO

-- EHS: rework imports. Use top level modules.
import qualified Euler.Common.Types                as D
import qualified Euler.Common.Types.External.Order as OEx
import qualified Euler.Common.Errors.PredefinedErrors as Errs
import qualified Euler.Product.Domain              as D
import           Euler.Product.Domain.MerchantAccount
import qualified Euler.Product.Domain.Templates as Ts
import qualified Euler.Product.OLTP.Order.OrderVersioningService as OVS
import qualified Euler.Config.Config               as Config
import           Euler.Lens

orderCreate
  :: RP.RouteParameters
  -> API.OrderCreateRequest
  -> MerchantAccount
  -> Flow API.OrderCreateResponse
orderCreate routeParams req ma = do
  let mbVersion     = RP.lookupRP @RP.Version routeParams
  let mbTokenNeeded = req ^. _options_get_client_auth_token
  let service       = OVS.mkOrderVersioningService mbVersion mbTokenNeeded
  case VO.transApiOrdCreateToOrdCreateT req of
    V.Failure err -> do
      logError @String "OrderCreateRequest validation" $ show err
      throwException $ Errs.mkValidationError err
    V.Success validatedOrder -> orderCreate'' routeParams service validatedOrder ma

-- EHS: get rid of RouteParameters. The logic should not know anything about params.
orderCreate''
  :: RP.RouteParameters
  -> OVS.OrderVersioningService
  -> Ts.OrderCreateTemplate
  -> MerchantAccount
  -> Flow API.OrderCreateResponse
orderCreate'' routeParams service order' mAccnt = do

  _               <- validateMandate order' (mAccnt ^. _merchantId)
  mbExistingOrder <- Rep.loadOrder (order' ^. _orderId) (mAccnt ^. _merchantId)

  case mbExistingOrder of
    -- EHS: should we throw exception or return a previous order?
    Just _  -> throwException $ Errs.orderAlreadyCreated (order' ^. _orderId)
    Nothing -> doOrderCreate routeParams service order' mAccnt

doOrderCreate
  :: RP.RouteParameters
  -> OVS.OrderVersioningService
  -> Ts.OrderCreateTemplate
  -> MerchantAccount
  -> Flow API.OrderCreateResponse
doOrderCreate routeParams (OVS.OrderVersioningService {makeOrderResponse}) order' mAccnt@MerchantAccount{..} = do
  merchantPrefs <- Rep.loadMerchantPrefs merchantId
  (dbOrder, order) <- createOrder' routeParams order' mAccnt merchantPrefs
  -- EHS: I find alias `Rep` sort of misleading here
  _                <- Rep.updateOrderCache (order ^. _orderId) (order ^. _merchantId) dbOrder
  _                <- Rep.updateMandateCache order $ order' ^. _mandate

  mbReseller    <- Rep.loadReseller (mAccnt ^. _resellerId)
  makeOrderResponse Config.getECRConfig order mAccnt mbReseller


-- EHS: previously isMandateOrder
-- EHS: bad function. Throws exception on invalid data.
validateMandate :: Ts.OrderCreateTemplate -> D.MerchantId -> Flow ()
validateMandate (Ts.OrderCreateTemplate {mandate}) merchantId = do
  case mandate of
    D.MandateRequired maxAmount        -> validateMaxAmount maxAmount
    D.MandateOptional (Just maxAmount) -> validateMaxAmount maxAmount
    _                                  -> pure ()
  --  D.MandateDisabled           -> pure ()
  --  D.MandateRequired maxAmount -> validateMaxAmount maxAmount
  --  D.MandateOptional maxAmount -> validateMaxAmount maxAmount

  where
    validateMaxAmount maxAmount = do

      -- EHS: magic constant
      -- EHS: getting values unsafely. What if we don't have these values??
      --      What flow should be then?
      mbMaxAmLimitStr <- rGet Config.redisConnName $ merchantId <> "_mandate_max_amount_limit"
      -- EHS: awful conversion. Rework.
      maxAmLimit      <- fromStringToNumber $ fromMaybe Config.mandateMaxAmountAllowed mbMaxAmLimitStr
-- EHS: maxAmount <= 0 verified on validation
      when (maxAmount <= 0.0 || maxAmount > maxAmLimit)
        $ throwException
        $ Errs.invalidMandateMaxAmount maxAmLimit
    -- EHS: bad function. Get rid of it.
    fromStringToNumber :: Text -> Flow Double
    fromStringToNumber str = do
      num <- pure $ readMayT str
      case num of
        Just x  -> pure $ x
        Nothing -> throwException $ err400 {errBody = "Invalid amount"}

readMayT :: Read a => Text -> Maybe a
readMayT = TR.readMaybe . Text.unpack

createOrder'
  :: RP.RouteParameters
  -> Ts.OrderCreateTemplate
  -> MerchantAccount
  -> DB.MerchantIframePreferences
  -> Flow (DB.OrderReference, D.Order)
createOrder' routeParams order' mAccnt merchantPrefs = do
  let currency' = (order' ^. _currency)
        <|> DB.defaultCurrency merchantPrefs
        <|> Just D.INR

  mbLoadedCustomer <- Rep.loadCustomer (order' ^. _customerId) (mAccnt ^. _id)

  let mbCustomer = fillCustomerContacts order' mbLoadedCustomer

  -- EHS: strange validation in the middle of logic.
  case (mbCustomer >>= (\c -> Just $ c ^. _customerId), (order' ^. _orderType) == D.MANDATE_REGISTER) of
      -- EHS: misleading error message. Should explicitly state that customerId has to be set on MANDATE_REGISTER
      -- EHS: make Explicit pattern match
      (Nothing, True) -> throwException Errs.customerNotFound
      _               -> pure ()

  let billingAddrHolder     = Rep.fillBillingAddressHolder mbCustomer (order' ^. _billingAddrHolder)
  mbBillingAddrId           <- Rep.createAddress (order' ^. _billingAddr) billingAddrHolder
  -- EHS: not sure why we don't load customer info for shipping addr as we did for billing addr.
  mbShippingAddrId          <- Rep.createAddress (order' ^. _shippingAddr) (order' ^. _shippingAddrHolder)

  (dbOrder, order) <- saveOrder order' mAccnt currency' mbBillingAddrId mbShippingAddrId mbCustomer
  _     <- saveOrderMetadata routeParams (order ^. _id) (order' ^. _metadata)

  pure (dbOrder, order)

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
  -> Flow (DB.OrderReference, D.Order)
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

  let orderRefVal :: DB.OrderReference = fillUDFParams order' $ DB.defaultOrderReference
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

  Rep.saveOrder orderRefVal


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
        , DB.lastUpdated = localDay orderMetadataTimestamp
        }

  orderMetadataDB <- Rep.saveOrderMetadataV2 orderMetadataDBVal

  -- EHS: this is extremely suspicious. Substituting id from DB by orderPid
  -- Where this action in purescript version?
  pure $ orderMetadataDB & (_id .~ Just orderPId)

fillUDFParams :: Ts.OrderCreateTemplate -> DB.OrderReference -> DB.OrderReference
fillUDFParams Ts.OrderCreateTemplate{udf} orderRef = smash udf orderRef

