{-# LANGUAGE DeriveAnyClass #-}
-- src/Product/OLTP/Services/RedisService.purs
module Euler.Product.OLTP.Services.RedisService where


import EulerHS.Prelude hiding ( id)
import EulerHS.Language
import Euler.Lens

import           Servant.Server
import           WebService.Language

import Euler.Config.ServiceConfiguration (TokenExpiryData(..), ResourceType(..))

--
--import Euler.KVDB.Redis

import qualified Data.Aeson           as A
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map             as Map
import qualified Data.Text.Encoding   as TE

import qualified Euler.Config.ServiceConfiguration as SC
import qualified Euler.Constant.Constants  as Constants (redis_token_expiry_default, token_max_usage_default)

data TokenizedResource = TokenizedResource
  { token  :: Text
  , expiry :: Text
  }
  deriving (Generic, Eq, Show, ToJSON, FromJSON)

-- EHS: rework
-- EHS: rename, this has nothing to do with tokenizing
tokenizeResource :: ResourceType -> Text -> Text -> Flow TokenizedResource
tokenizeResource resourceId resourceType merchantId = do
  token'      <- ("tkn_" <>) <$> getUUID32
  TokenExpiryData {..} <- getTokenExpiryData resourceType merchantId
  currentDate <- getCurrentDateInMillis
  redisData   <- pure $ getRedisData resourceType resourceId tokenMaxUsage expiryInSeconds currentDate
  _ <- rSetex token' redisData expiryInSeconds
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

invalidateCardListForMerchantCustomer :: Text -> Text -> Flow ()
invalidateCardListForMerchantCustomer merchantId customerId =
  void $ rDel ["ec_cards_:" <> merchantId <> ":" <> customerId]


invalidateOrderStatusCache :: Text -> Text -> Flow ()
invalidateOrderStatusCache orderId merchantId = do
  logInfo @String "invalidateOrderStatusCacheStart" $ "Invalidating order status cache for " <> merchantId <> " and order_id " <> orderId
  void $ rDel
        [ "euler_ostatus_" <> merchantId <> "_" <> orderId
        , "euler_ostatus_unauth_" <> merchantId <> "_" <> orderId
        , "ostatus_" <> merchantId <> "_" <> orderId
        , "ostatus_unauth_" <> merchantId <> "_" <> orderId
        ]
  logInfo @String "invalidateOrderStatusCacheEnd" $ "Invalidating order status cache for " <> merchantId <> " and order_id " <> orderId
