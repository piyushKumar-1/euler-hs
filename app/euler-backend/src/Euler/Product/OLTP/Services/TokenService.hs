{-# LANGUAGE DeriveAnyClass #-}

module Euler.Product.OLTP.Services.TokenService
  ( -- * Types
    AuthTokenResourceType(..)
    -- * functions
  , acquireOrderToken
  ) where

import           EulerHS.Language
import           EulerHS.Prelude hiding (id)

import           Euler.Lens


import           Servant.Server
import           WebService.Language


import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map as Map
import qualified Data.Text.Encoding as TE

import qualified Euler.Config.Config as C
import           Euler.Config.ServiceConfiguration (ResourceType (..), TokenExpiryData (..))
import qualified Euler.Config.ServiceConfiguration as SC
import qualified Euler.Constant.Constants as Constants (redis_token_expiry_default,
                                                        token_max_usage_default)

-- EHS: rework imports. Use top level modules.
import qualified Euler.API.Order as API
import qualified Euler.Common.Types as D
--import qualified Euler.Common.Types.External.Order as OEx
import qualified Euler.Common.Metric as Metric
--import qualified Euler.Config.Config               as Config
--import qualified Euler.Config.ServiceConfiguration as SC
--import           Euler.Lens
--import qualified Euler.Product.Domain              as D
--import qualified Euler.Product.Domain.Order        as D
--import           Euler.Product.Domain.MerchantAccount

-- | Possible auth token resource types
data AuthTokenResourceType
  = ORDER
  | CUSTOMER
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

-- | Create an authentication token for an order
acquireOrderToken :: D.OrderPId -> D.MerchantId -> Flow API.OrderTokenResp
acquireOrderToken orderId merchantId = do
    tokenized <- tokenizeResource (SC.ResourceInt orderId) tag merchantId
    -- EHS: check this
    runIO $ Metric.incrementClientAuthTokenGeneratedCount merchantId
    pure $ conv tokenized
  where
    tag = show ORDER
    conv TokenizedResource {token, expiry} =
      API.OrderTokenResp
        { API.client_auth_token        = Just token
        , API.client_auth_token_expiry = Just expiry
        }

-- ----------------------------------------------------------------------------

data TokenizedResource = TokenizedResource
  { token  :: Text
  , expiry :: Text
  }
  deriving (Generic, Eq, Show, ToJSON, FromJSON)

-- EHS: rework
-- EHS: rename, this has nothing to do with tokenizing (seems everyone is fine with that, ok)
tokenizeResource :: ResourceType -> Text -> Text -> Flow TokenizedResource
tokenizeResource resourceId resourceType merchantId = do
  token'      <- ("tkn_" <>) <$> getUUID32
  TokenExpiryData {..} <- getTokenExpiryData resourceType merchantId
  currentDate <- getCurrentDateInMillis
  redisData   <- pure $ getRedisData resourceType resourceId tokenMaxUsage expiryInSeconds currentDate
  _ <- rSetex C.redis token' redisData expiryInSeconds
  --setCacheWithExpiry Constants.ecRedis token redisData (convertDuration $ Seconds $ toNumber expiryInSeconds)
  _           <- logInfo (resourceType <> "_token_cache") (token' <> (show redisData))
  expiry'     <- getCurrentDateStringWithSecOffset Constants.redis_token_expiry_default
  pure $ TokenizedResource {token = token', expiry = expiry'}

  where getRedisData resourceType' resourceId' tokenMaxUsage' expiryInSeconds currentDate = SC.TokenCacheData
          { resourceId = resourceId'
          , resourceType = resourceType'
          , tokenMaxUsage = tokenMaxUsage'
          , source = Just "euler"
          , usageCount = Nothing
          , expiresAt = Just $ currentDate + ((expiryInSeconds * 1000))
          }


getTokenExpiryData :: Text -> Text -> Flow TokenExpiryData -- {expiryInSeconds :: Int ,tokenMaxUsage :: Int}
getTokenExpiryData resourceType merchantId = do
  serviceConfig <- pure $ resourceType <> "_TOKEN_EXPIRY_DATA_MERCHANT_WISE"
  configMaybe   <- SC.findByName serviceConfig
  case configMaybe of
    Just toknExpData -> do
      (SC.MerchantWiseTokenExpiryData val) <- parseAndDecodeJson (toknExpData ^. _value) "INTERNAL_SERVER_ERROR" "Error decoding Service Configuration"
      case Map.lookup merchantId val of
        Just tokenExpiryData -> pure tokenExpiryData -- $  {expiryInSeconds = val.expiryInSeconds ,tokenMaxUsage = val.tokenMaxUsage}
        Nothing -> case Map.lookup "default" val of
          Just tokenExpiryData -> pure tokenExpiryData -- $  {expiryInSeconds = val.expiryInSeconds ,tokenMaxUsage = val.tokenMaxUsage}
          Nothing -> (logError @String "getTokenExpiryData" "Could not find default service config") *>  throwException err500 {errBody = "getTokenExpiryData"} -- defaultThrowECException "INTERNAL_SERVER_ERROR" "Could not find default service config"
    Nothing -> pure $ TokenExpiryData {expiryInSeconds = Constants.redis_token_expiry_default ,tokenMaxUsage = Constants.token_max_usage_default}


-- src/Utils/Utils.purs
parseAndDecodeJson :: FromJSON a => Text -> Text -> Text -> Flow a
parseAndDecodeJson tval errorCode errorMessage = case (A.eitherDecode $ BSL.fromStrict $ TE.encodeUtf8 tval) of
      Right val ->  pure val
      Left _ -> do
        logError errorCode errorMessage
        throwException err500 {errBody = "parseAndDecodeJson"}

-- EHS: seems there is no any usages for this one
--invalidateCardListForMerchantCustomer :: Text -> Text -> Flow ()
--invalidateCardListForMerchantCustomer merchantId customerId =
--  void $ rDel ["ec_cards_:" <> merchantId <> ":" <> customerId]
