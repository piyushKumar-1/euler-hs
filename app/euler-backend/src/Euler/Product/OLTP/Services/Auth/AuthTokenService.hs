module Euler.Product.OLTP.Services.Auth.AuthTokenService
  ( newHandle
  ) where

import           EulerHS.Prelude hiding (id)

import           EulerHS.Language as L
import           EulerHS.Types (Message)

import qualified Data.Generics.Product as DGP
import qualified Data.Text as T

import qualified Euler.API.RouteParameters as RP
import           Euler.Common.Types.Order (ClientAuthTokenData (..), OrderTokenExpiryData (..))
import qualified Euler.Constants          as Constants (ecRedis)
import qualified Euler.Config.Config as Config (orderTokenExpiry)
import qualified Euler.Config.ServiceConfiguration as SC (decodeValue, findByName)
import           Euler.Lens
import qualified Euler.Product.Domain.MerchantAccount as DM
import qualified Euler.Product.OLTP.Services.Auth.AuthService as X
import           Euler.Product.OLTP.Services.TokenService as TS
import qualified Euler.Storage.Repository as Rep
import           WebService.Language



newHandle :: X.SHandle
newHandle = X.SHandle
  {X.authenticate = authenticate
  }

authenticate :: RP.RouteParameters -> Flow (Either Text DM.MerchantAccount)
authenticate rps = do
  case (RP.lookupRP @RP.ClientAuthToken rps) of
    Just token -> do
      mbTokenData :: Maybe ClientAuthTokenData <- rGet Constants.ecRedis token
      case mbTokenData of
        Just tokenData -> do
          checkTokenValidityAndIncUsageCounter token tokenData
          mbMA <- tokenMerchant tokenData
          case mbMA of
            Nothing -> do
              let err = "Can't load merchant for token from cache"
              logError' err
              pure $ Left err
            Just ma -> do
              return $ Right ma
        Nothing ->  do
          let err = "No auth token data found in cache"
          logError' err
          pure $ Left err
    Nothing -> do
      let err = "no auth token header presents in request"
      logError' err
      pure $ Left err

-- former updateAuthTokenUsage
checkTokenValidityAndIncUsageCounter :: AuthToken -> ClientAuthTokenData -> Flow ()
checkTokenValidityAndIncUsageCounter token tokenData@ClientAuthTokenData {..} = do
  let newUsageCount = maybe 1 (+1) usageCount
  -- EHS: why did we have here == rather than >=?
  case (newUsageCount >= tokenMaxUsage) of
    True -> do
      -- EHS: shall we handle rDel result somehow?
      _ <- rDel Constants.ecRedis [token]
      pure ()
    False -> do
      expiry <- tokenExpiry
      _ <- rSetex Constants.ecRedis token (tokenData {usageCount = Just newUsageCount}) expiry
      pure ()

-- EHS: former getTokenExpiryData :: Flow OrderTokenExpiryData
-- EHS: really we need only expireInSeconds value
tokenExpiry :: Flow Int
tokenExpiry = do
  -- EHS: use enumeration
  mbServiceConfiguration <- SC.findByName "ORDER_TOKEN_EXPIRY_DATA"
  case mbServiceConfiguration of
    Just serviceConfiguration -> do
      let value = DGP.getField @"value" serviceConfiguration
      let mbDecodedVal = SC.decodeValue @OrderTokenExpiryData value
      case mbDecodedVal of
        Just decodedVal -> pure $ DGP.getField @"expiryInSeconds" decodedVal
        Nothing         -> pure Config.orderTokenExpiry
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
                  pure Nothing
            Nothing -> do
              logError' $ "malformed resource id: " <> resourceId
              pure Nothing
        CUSTOMER -> do
          mbCustomer <- Rep.findCustomerById resourceId
          case mbCustomer of
            Just customer -> do
              let cId = customer ^. _customerMerchantAccountId
              Rep.loadMerchantById cId
            Nothing -> do
              logError' $ "customer with id: " <> resourceId <> " cannot be loaded"
              pure Nothing
    Nothing -> do
        logError' $ "unknown resource type: " <> resourceType
        pure Nothing

-- | Alias for token representation in a request's header
type AuthToken = Text

-- EHS: looks as if we have lots of similar functions in many modules
readMay :: Read a => Text -> Maybe a
readMay = readMaybe . T.unpack

logError' :: Message -> Flow ()
logError' = logErrorT "AuthTokenService"
