module Euler.Product.OLTP.Services.AuthTokenService
  ( authWithToken
  ) where

import           EulerHS.Prelude                      hiding (id)

import           EulerHS.Language                     as L

-- EHS: how can we import a datatype from a hidden package?
--import           EulerHS.Core.Types                   (Logger)
import qualified Euler.Common.Errors.PredefinedErrors as Errs

import qualified Data.Generics.Product                as DGP
import qualified Data.Text                            as T

import qualified Euler.API.RouteParameters            as RP
import qualified Euler.Config.Config                  as Config (orderTokenExpiry)
import qualified Euler.Config.ServiceConfiguration    as SC (findByName, decodeValue)
import           Euler.Common.Types.Order             (ClientAuthTokenData(..), OrderTokenExpiryData (..))
import           Euler.Lens
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
          mbMA <- tokenMerchant tokenData
          case mbMA of
            Nothing -> do
              logError' "Can't load merchant for token from cache"
              throwException Errs.internalError
            Just ma -> do
              return ma
        Nothing ->  do
          logError' "No auth token data found in cache"
          throwException Errs.internalError
    Nothing -> do
      logError' "no auth token header presents in request"
      throwException Errs.internalError

-- former updateAuthTokenUsage
checkTokenValidityAndIncUsageCounter :: AuthToken -> ClientAuthTokenData -> Flow ()
checkTokenValidityAndIncUsageCounter token tokenData@ClientAuthTokenData {..} = do
  let newUsageCount = maybe 1 (+1) usageCount
  -- EHS: why did we have here == rather than >=?
  case (newUsageCount >= tokenMaxUsage) of
    True -> do
      -- EHS: shall we handle rDel result somehow?
      _ <- rDel [token]
      pure ()
    False -> do
      expiry <- tokenExpiry
      _ <- rSetex token (tokenData {usageCount = Just newUsageCount}) expiry
      pure ()

-- EHS: former getTokenExpiryData :: Flow OrderTokenExpiryData
-- EHS: really we need only expireInSeconds value
tokenExpiry :: Flow Int
tokenExpiry = do
  mbServiceConfiguration <- SC.findByName "ORDER_TOKEN_EXPIRY_DATA"
  case mbServiceConfiguration of
    Just serviceConfiguration -> do
      let value = DGP.getField @"value" serviceConfiguration
      let mbDecodedVal = SC.decodeValue @OrderTokenExpiryData value
      case mbDecodedVal of
        Just decodedVal -> pure $ DGP.getField @"expiryInSeconds" decodedVal
        Nothing -> pure Config.orderTokenExpiry
    Nothing -> pure Config.orderTokenExpiry

-- former getMerchantAccountForAuthToken from OrderStatus
tokenMerchant :: ClientAuthTokenData -> Flow (Maybe DM.MerchantAccount)
tokenMerchant ClientAuthTokenData {..} = do
  case (readMay resourceType) of
    Just t -> do
      case t of
        ORDER -> do
          --case (readId resourceId) of
          case (readMay @Int resourceId) of
            Just id -> do
              mbOrder <- Rep.loadOrderById id
              case mbOrder of
                Just order -> do
                  Rep.loadMerchantById $ read $ T.unpack $ order ^. _merchantId
                Nothing -> do
                  logError' $ "order with id: " <> resourceId <> " cannot be loaded"
                  throwException Errs.internalError
            Nothing -> do
              logError' $ "malformed resource id: " <> resourceId
              throwException Errs.internalError
        CUSTOMER -> do
          mbCustomer <- Rep.findCustomerById resourceId
          case mbCustomer of
            Just customer -> do
              let cId = customer ^. _customerMerchantAccountId
              Rep.loadMerchantById cId
            Nothing -> do
              logError' $ "customer with id: " <> resourceId <> " cannot be loaded"
              throwException Errs.internalError
    Nothing -> do
        logError' $ "unknown resource type: " <> resourceType
        throwException Errs.internalError

-- | Alias for token representation in a request's header
type AuthToken = Text

-- | Possible auth token resource types
data AuthTokenResourceType
  = ORDER
  | CUSTOMER
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

-- EHS: looks as if we have lots of similar functions in many modules
readMay :: Read a => Text -> Maybe a
readMay = readMaybe . T.unpack

lTag :: Text
lTag = "AuthTokenService"

-- EHS: how can we import Message type, which resides in a hidden package?
--logError' :: Logger.Message -> Flow ()
logError' = logError @Text lTag
