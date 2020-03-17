module Euler.Product.OLTP.Services.AuthTokenService
  ( authWithToken
  ) where

import           EulerHS.Prelude                      hiding (id)

import           EulerHS.Language                     as L

-- EHS: how can we import a datatype from a hidden package?
--import           EulerHS.Core.Types                   (Logger)

-- EHS: is it ok to have it here?
import           Servant.Server                       (err500)

import qualified Data.Text                            as T
import qualified Euler.API.RouteParameters            as RP
--import qualified Euler.Common.Errors.PredefinedErrors as Errs
import           Euler.Common.Types.Order             (ClientAuthTokenData(..), OrderTokenExpiryData (..))
--import qualified Euler.KVDB.Redis                     as R
import           Euler.Lens
import qualified Euler.Product.Domain.Customer        as DMC
--import qualified Euler.Product.Domain.Order           as DMO
import qualified Euler.Product.Domain.MerchantAccount as DM
import qualified Euler.Storage.Repository             as Rep


authWithToken
  :: forall req resp.
  (RP.RouteParameters -> req -> DM.MerchantAccount -> Flow resp)
  -> RP.RouteParameters
  -> req
  -> Flow resp

authWithToken f rp req = do
  merchant <- authenticate rp
  f rp req merchant

authenticate :: RP.RouteParameters -> Flow DM.MerchantAccount
authenticate rps = do
  case (RP.lookupRP @RP.ClientAuthToken rps) of
    Just token -> do
      mbTokenData :: Maybe ClientAuthTokenData <- rGet token
      case mbTokenData of
        Just tokenData -> do
          checkTokenValidityAndIncUsageCounter token tokenData
          mbMA <- findMerchant tokenData
          case mbMA of
            Nothing -> do
              logError' "Can't load merchant for token from cache"
              throwException err500
            Just ma -> do
              return ma
        Nothing ->  do
          logError' "No auth token data found in cache"
          throwException err500
    Nothing -> do
      logError' "no auth token header presents in request"
      throwException err500

-- former updateAuthTokenUsage
checkTokenValidityAndIncUsageCounter :: AuthToken -> ClientAuthTokenData -> Flow ()
checkTokenValidityAndIncUsageCounter token clientAuthToken@ClientAuthTokenData {..} = do
  let newUsageCount = maybe 1 (+1) usageCount
  -- EHS: why did we have here == rather than <=?
  case (newUsageCount <= tokenMaxUsage) of
    True -> do
      -- EHS: shall we handle rDel result somehow?
      _ <- rDel [token]
      pure ()
    False -> do
      tokenExpiryData <- getTokenExpiryData
     -- let ttl = convertDuration $ Seconds $ toNumber tokenExpiryData.expiryInSeconds
      _ <- pure () --setCacheEC ttl authToken (clientAuthToken {usageCount = Just newUsageCount})
      pure ()


getTokenExpiryData :: Flow OrderTokenExpiryData
getTokenExpiryData = undefined
--getTokenExpiryData = do
--  orderToken <- T.append "tkn_" <$> getUUID32
--  currentDateWithOffset <- getCurrentDateStringWithSecOffset orderTokenExpiry
--  let defaultTokenData = OrderTokenExpiryData
--        { expiryInSeconds = orderTokenExpiry
--        , tokenMaxUsage = orderTokenMaxUsage
--        , orderToken = Just orderToken
--        , currentDateWithExpiry = Just currentDateWithOffset
--        }
--
--  mServiceConfiguration <- withDB eulerDB $ do
--      let predicate ServiceConfiguration {name} =
--            name ==. B.val_ "ORDER_TOKEN_EXPIRY_DATA"
--      findRow
--        $ B.select
--        $ B.limit_ 1
--        $ B.filter_ predicate
--        $ B.all_ (DB.service_configuration eulerDBSchema)
--
--  case mServiceConfiguration of
--    (Just serviceConfiguration) -> do
--      let value = getField @"value" serviceConfiguration
--          mDecodedVal = (decode $ BSL.fromStrict $ T.encodeUtf8 value) :: Maybe OrderTokenExpiryData
--      case mDecodedVal of
--        Nothing -> pure defaultOrderTokenExpiryData
--        Just decodedVal -> pure defaultOrderTokenExpiryData
--          { expiryInSeconds = getField @"expiryInSeconds" decodedVal
--          , tokenMaxUsage = getField @"tokenMaxUsage" decodedVal
--          }
--    Nothing -> pure defaultOrderTokenExpiryData

-- former getMerchantAccountForAuthToken from OrderStatus
findMerchant :: ClientAuthTokenData -> Flow (Maybe DM.MerchantAccount)
findMerchant ClientAuthTokenData {..} = do
  case (readType resourceType) of
    Just t -> do
      case t of
        ORDER -> do
          case (readId resourceId) of
            Just id -> do
              mbOrder <- Rep.loadOrderById id
              case mbOrder of
                Just order -> do
                  Rep.loadMerchant $ read $ T.unpack $ order ^. _merchantId
                Nothing -> do
                  logError' $ "order with id: " <> resourceId <> " cannot e loaded"
                  throwException err500
            Nothing -> do
              logError' $ "malformed resource id: " <> resourceId
              throwException err500
        -- EHS: rework
        CUSTOMER -> do
          DMC.Customer {..} <- pure DMC.defaultCustomer -- DB.findOneWithErr ecDB (where_ := WHERE ["id" /\ String otokenData.resourceId]) ecAccessDenied
          pure $ Just DM.defaultMerchantAccount -- DB.findOneWithErr ecDB (where_ := WHERE ["id" /\ Int merchantAccountId]) ecAccessDenied

    -- EHS: rework
    Nothing -> do
        logError' $ "unknown resource type: " <> resourceType
        throwException err500

-- | Alias for token representation in a request's header
type AuthToken = Text

-- | Possible auth token resource types
data AuthTokenResourceType
  = ORDER
  | CUSTOMER
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

readType :: Text -> Maybe AuthTokenResourceType
readType = readMaybe . T.unpack

readId :: Text -> Maybe Int
readId = read . T.unpack

lTag :: Text
lTag = "AuthTokenService"

--logError' :: Logger.Message -> Flow ()
logError' = logError lTag
