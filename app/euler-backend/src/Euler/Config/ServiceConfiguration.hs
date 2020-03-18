{-# LANGUAGE DeriveAnyClass #-}

-- EHS: move some stuff to repository
module Euler.Config.ServiceConfiguration where
-- src/Types/Storage/EC/ServiceConfiguration.purs

import EulerHS.Prelude

import EulerHS.Language

import Euler.Lens

import qualified Data.Aeson            as A
import qualified Data.Text.Encoding   as T
import qualified Data.ByteString.Lazy as BSL

import Euler.Storage.DBConfig (ecDB)
import Euler.Storage.Types.EulerDB
import Euler.Storage.Types.ServiceConfiguration


--import qualified Data.Map                  as Map
import qualified Database.Beam             as B
--import qualified Database.Beam.Backend.SQL as B

import Database.Beam ((==.))

-- EHS: rework this logic.

-- import Data.StrMap
-- import ECPrelude
--
-- import Config.Constants (ecDB)
-- import Config.Constants (maxSubmitOtpAttempts, maxResendOtpAttempts) as Constants
-- import Presto.Backend.Flow (BackendFlow)
-- import Presto.Backend.Flow (log) as Presto
-- import Presto.Backend.Types.API (Header(..))
-- import Sequelize.CRUD.Create (create)
-- import Sequelize.CRUD.Read (findOne)
-- import Sequelize.Class (class DecodeModel, class EncodeModel, class Model, genericDecodeModel, genericEncodeModel)
-- import Sequelize.Models (makeModelOf)
-- import Sequelize.Models.Columns (autoIncrement, columnType, primaryKey)
-- import Sequelize.Models.Options (freezeTableName, timestamps)
-- import Sequelize.Models.Types (PositiveInt(..))
-- import Sequelize.Models.Types as ModelTypes
-- import Sequelize.Query.Options (where_)
-- import Sequelize.Types (Conn, Instance, ModelCols, ModelOf, SEQUELIZE)
-- import Sequelize.Where (Literal(..), WHERE(..))
-- import Types.App (defaultThrowECException, throwCustomException)
-- import Types.Lenses (_value)
-- import Utils.Encoding (defaultDecode, defaultEncode)
-- import Utils.PrestoBackend (TState)
-- import Utils.PrestoBackend as DB
-- import Utils.Utils (createBasicAuthFromCred, fromStringToInt, parseAndDecodeJson, (.^.))


-- data ServiceConfiguration = ServiceConfiguration
--  { id :: Maybe Int
--   , version :: Int
--   , name :: Text
--   , value :: Text
--   }

-- id :: Option ServiceConfiguration Int
-- id = opt "id"
--
-- version :: Option ServiceConfiguration Int
-- version = opt "version"
--
-- name :: Option ServiceConfiguration String
-- name = opt "name"
--
-- value :: Option ServiceConfiguration String
-- value = opt "value"
--
-- createServiceConfiguration :: forall e. ModelOf ServiceConfiguration -> ServiceConfiguration ->  Aff (sequelize :: SEQUELIZE | e) (Instance ServiceConfiguration)
-- createServiceConfiguration model serviceConfiguration = create model serviceConfiguration
--
-- findOneServiceConfiguration :: forall e. ModelOf ServiceConfiguration -> Options ServiceConfiguration ->  Aff (sequelize :: SEQUELIZE | e) (Maybe (Instance ServiceConfiguration ))
-- findOneServiceConfiguration model whereC = findOne model whereC
--
-- makeServiceConfiguration :: forall e. Conn -> Aff ( sequelize :: SEQUELIZE | e) (ModelOf ServiceConfiguration)
-- makeServiceConfiguration = flip makeModelOf (freezeTableName := true <> timestamps := false)
--
-- getServiceConfigurationCols :: ModelCols ServiceConfiguration
-- getServiceConfigurationCols = [ "id" /\ idOpts
--                               , "version" /\ versionOpts
--                               , "name" /\ nameOpts
--                               , "value" /\ valueOpts
--                               ]
-- 	where
--   	idOpts = columnType := ModelTypes.BigInt0
--   		<> primaryKey := true <> autoIncrement := true
--   	versionOpts = columnType := ModelTypes.BigInt0
--   	nameOpts = columnType := ModelTypes.String { length : Just $ PositiveInt 255}
--   	valueOpts = columnType := ModelTypes.String { length : Just $ PositiveInt 512}
--
-- derive instance eqServiceConfiguration :: Eq ServiceConfiguration
--
-- instance showServiceConfiguration :: Show ServiceConfiguration where show = genericShow
--
-- derive instance genericServiceConfiguration :: Generic ServiceConfiguration _
-- derive instance newtypeServiceConfiguration :: Newtype ServiceConfiguration _
--
-- instance decodeServiceConfiguration :: Decode ServiceConfiguration where decode = genericDecodeModel
-- instance encodeServiceConfiguration :: Encode ServiceConfiguration where encode = genericEncodeModel
-- instance decodeModelServiceConfiguration :: DecodeModel ServiceConfiguration where decodeModel = genericDecodeModel
-- instance encodeModelServiceConfiguration :: EncodeModel ServiceConfiguration where encodeModel = genericEncodeModel
--
-- instance isModelServiceConfiguration :: Model ServiceConfiguration where
-- 	modelCols = getServiceConfigurationCols
-- 	modelName _ = "service_configuration"

getServiceConfigurationFromKey :: Text -> Flow (Maybe ServiceConfiguration)
getServiceConfigurationFromKey key = withDB ecDB $ do
  findRow
    $ B.select
    $ B.limit_ 1
    $ B.filter_ (\ServiceConfiguration{name} -> name ==. B.val_ key)
    $ B.all_ (service_configuration eulerDBSchema)
  --DB.findOne ecDB $ where_ := WHERE ["name" /\ String key] :: WHERE ServiceConfiguration

getServiceConfigurationValueFromKey :: Text -> Flow (Maybe Text)
getServiceConfigurationValueFromKey key = do
  maybeServiceConfiguration <- getServiceConfigurationFromKey key
  pure $ ( ^. _value) <$> maybeServiceConfiguration

findByName :: Text -> Flow (Maybe ServiceConfiguration)
findByName name = getServiceConfigurationFromKey name
--   DB.findOne ecDB $ where_ := WHERE ["name" /\ String name] :: WHERE ServiceConfiguration

decodeValue :: FromJSON a => Text -> Maybe a
decodeValue = A.decode . BSL.fromStrict . T.encodeUtf8

data MerchantWiseTokenExpiryData = MerchantWiseTokenExpiryData (Map Text TokenExpiryData)
  deriving (Generic, Eq, Show, ToJSON, FromJSON)
-- derive instance genericMerchantWiseTokenExpiryData :: Generic MerchantWiseTokenExpiryData _
-- instance decodeMerchantWiseTokenExpiryData :: Decode MerchantWiseTokenExpiryData where decode = defaultDecode
-- instance encodeMerchantWiseTokenExpiryData :: Encode MerchantWiseTokenExpiryData where encode = defaultEncode

data TokenExpiryData = TokenExpiryData
  { expiryInSeconds :: Int
  , tokenMaxUsage   :: Int
  }
  deriving (Generic, Eq, Show, ToJSON, FromJSON)

--derive instance genericTokenExpiryData :: Generic TokenExpiryData _
--derive instance newtypeTokenExpiryData :: Newtype TokenExpiryData _
--instance decodeTokenExpiryData :: Decode TokenExpiryData where decode = defaultDecode
--instance encodeTokenExpiryData :: Encode TokenExpiryData where encode = defaultEncode

data ResourceType = ResourceInt Int | ResourceStr Text
  deriving (Generic, Eq, Show, ToJSON, FromJSON)

data TokenCacheData = TokenCacheData
  { resourceId :: ResourceType
  , resourceType :: Text
  , tokenMaxUsage :: Int
  , source :: Maybe Text
  , usageCount :: Maybe Int
  , expiresAt :: Maybe Int
  }
  deriving (Generic, Eq, Show, ToJSON, FromJSON)

--derive instance genericTokenCacheData :: Generic TokenCacheData _
--derive instance newtypeTokenCacheData :: Newtype TokenCacheData _
--instance decodeTokenCacheData :: Decode TokenCacheData where decode = defaultDecode
--instance encodeTokenCacheData :: Encode TokenCacheData where encode = defaultEncode

data GatewayWiseOtpAttempts = GatewayWiseOtpAttempts (Map Text OtpAttempts)
  deriving (Generic, Eq, Show, ToJSON, FromJSON)

--derive instance genericGatewayWiseOtpAttempts :: Generic GatewayWiseOtpAttempts _
--instance decodeGatewayWiseOtpAttempts :: Decode GatewayWiseOtpAttempts where decode = defaultDecode
--instance encodeGatewayWiseOtpAttempts :: Encode GatewayWiseOtpAttempts where encode = defaultEncode

data OtpAttempts = OtpAttempts
  { challengesAttempted :: Int
  , responseAttempted   :: Int
  }
  deriving (Generic, Eq, Show, ToJSON, FromJSON)

-- derive instance genericOtpAttempts :: Generic OtpAttempts _
-- derive instance newtypeOtpAttempts :: Newtype OtpAttempts _
-- instance decodeOtpAttempts :: Decode OtpAttempts where decode = defaultDecode
-- instance encodeOtpAttempts :: Encode OtpAttempts where encode = defaultEncode

-- ##########
-- data RazorpayAuthenticationErrorCodes
--
-- getMaxSubmitOtpAttempts  ::  Text -> Flow Int
-- getMaxSubmitOtpAttempts gateway = do
--   configMaybe <- findByName "GATEWAY_WISE_OTP_ATTEMPTS"
--   case configMaybe of
--     Just otpAttemptsConfig -> do
--       (GatewayWiseOtpAttempts val) <- parseAndDecodeJson (otpAttemptsConfig ^. _value) "INTERNAL_SERVER_ERROR" "Error decoding Service Configuration"
--       case lookup gateway val of
--         Just (OtpAttempts otpAttempts) -> pure $ otpAttempts.responseAttempted
--         Nothing -> case lookup "default" val of
--           Just (OtpAttempts val) -> pure $ val.responseAttempted
--           Nothing -> (Presto.log "validateDirectOtpRequest" "Could not find default service config for GATEWAY_WISE_OTP_ATTEMPTS ") *> defaultThrowECException "INTERNAL_SERVER_ERROR" "Could not find default service config"
--     Nothing -> pure $ Constants.maxSubmitOtpAttempts
--
-- getMaxResendOtpAttempts  :: forall st r. Newtype st { orderId :: Maybe String, merchantId :: Maybe String, isDBMeshEnabled :: Maybe Boolean, isMemCacheEnabled :: Boolean | r } => String -> BackendFlow st _ Int
-- getMaxResendOtpAttempts gateway = do
--   configMaybe <- findByName "GATEWAY_WISE_OTP_ATTEMPTS"
--   case configMaybe of
--     Just otpAttemptsConfig -> do
--       (GatewayWiseOtpAttempts val) <- parseAndDecodeJson (otpAttemptsConfig ^. _value) "INTERNAL_SERVER_ERROR" "Error decoding Service Configuration"
--       case lookup gateway val of
--         Just (OtpAttempts otpAttempts) -> pure $ otpAttempts.challengesAttempted
--         Nothing -> case lookup "default" val of
--           Just (OtpAttempts val) -> pure $ val.challengesAttempted
--           Nothing -> (Presto.log "validateDirectOtpRequest" "Could not find default service config for GATEWAY_WISE_OTP_ATTEMPTS ") *> defaultThrowECException "INTERNAL_SERVER_ERROR" "Could not find default service config"
--     Nothing -> pure $ Constants.maxResendOtpAttempts
--
-- getRazorpayAuthenticationErrorCodes  :: forall st r. Newtype st { orderId :: Maybe String, merchantId :: Maybe String, isDBMeshEnabled :: Maybe Boolean, isMemCacheEnabled :: Boolean | r } => BackendFlow st _ (Array String)
-- getRazorpayAuthenticationErrorCodes = do
--   authErrorCodesMaybe <- findByName "RAZORPAY_AUTHENTICATION_FAILED_ERROR_CODES"
--   case authErrorCodesMaybe of
--     Just authErrorCodes -> do
--       case runExcept $ decodeJSON (authErrorCodes ^. _value) of
--         Right (value :: Array String) -> pure value
--         Left err -> (Presto.log "getRazorpayAuthenticationErrorCodes" $ "Could not decode config for RAZORPAY_AUTHENTICATION_ERROR_CODES " <> (show err)) *> pure []
--     Nothing -> (Presto.log "getRazorpayAuthenticationErrorCodes" $ "Could not find service config for RAZORPAY_AUTHENTICATION_ERROR_CODES") *> pure []
--
-- getPayUAuthenticationErrorCodes :: forall st r. Newtype st { orderId :: Maybe String, merchantId :: Maybe String, isDBMeshEnabled :: Maybe Boolean, isMemCacheEnabled :: Boolean | r } => BackendFlow st _ (Array String)
-- getPayUAuthenticationErrorCodes = do
--   authErrorCodesMaybe <- findByName "PAYU_AUTHENTICATION_ERROR_CODES"
--   case authErrorCodesMaybe of
--     Just authErrorCodes -> do
--       case runExcept $ decodeJSON (authErrorCodes ^. _value) of
--         Right (value :: Array String) -> pure value
--         Left err -> (Presto.log "getPayUAuthenticationErrorCodes" $ "Could not decode config for PAYU_AUTHENTICATION_ERROR_CODES " <> (show err)) *> pure []
--     Nothing -> (Presto.log "getPayUAuthenticationErrorCodes" $ "Could not find service config for PAYU_AUTHENTICATION_ERROR_CODES") *> pure []
--
-- getViesSeamlessFlags :: forall st r. Newtype st { orderId :: Maybe String, merchantId :: Maybe String, isDBMeshEnabled :: Maybe Boolean, isMemCacheEnabled :: Boolean | r } => String -> String -> BackendFlow st _ Boolean
-- getViesSeamlessFlags merchantId flag = do
--   flagValueMaybe <- findByName $ "VIES_SEAMLESS_" <> merchantId <> "_" <> flag
--   case flagValueMaybe of
--     Just serviceConfig -> if (serviceConfig ^. _value == "false") then pure false else pure true
--     Nothing -> (Presto.log ("getViesSeamlessFlag_" <> flag) $ "Defaulting to true. since couldn't find service config for getViesSeamlessFlags_" <> flag) *> pure true
--
-- getValueFromServiceConfiguration :: forall st r a. Newtype st { orderId :: Maybe String, merchantId :: Maybe String, isDBMeshEnabled :: Maybe Boolean, isMemCacheEnabled :: Boolean | r } =>
--                                     Decode a => String -> BackendFlow st _ (Maybe a)
-- getValueFromServiceConfiguration key = do
--   maybeServiceConfiguration <- getServiceConfigurationFromKey key
--   case maybeServiceConfiguration of
--     Just val -> do
--       case (runExcept <<< decodeJSON $ val ^. _value) of
--         Right res -> pure $ Just res
--         Left err -> do
--           _ <- Presto.log "Service Configuration decode error" err
--           pure Nothing
--     Nothing -> pure Nothing
--
-- getRedisConfig :: forall st r value. Newtype st { orderId :: Maybe String, merchantId :: Maybe String, isDBMeshEnabled :: Maybe Boolean, isMemCacheEnabled :: Boolean | r } => Decode value => String -> BackendFlow st _ (Maybe value)
-- getRedisConfig key = do
--   redisConfOpt <- findByName key
--   case redisConfOpt of
--     Just redisConfOpt -> do
--       case runExcept $ decodeJSON (redisConfOpt ^. _value) of
--         Right value -> pure $ Just value
--         Left err -> (Presto.log "getRolloutFeatureConfig" $ "Could not decode config for " <> (toUpper key) <> (show err)) *> pure Nothing
--     Nothing -> (Presto.log "getRolloutFeatureConfig" $ "Could not find service config for " <> (toUpper key)) *> pure Nothing
--
-- fetchEulerInternalApiKey :: ∀ st r e. Newtype st (TState e) ⇒ BackendFlow st { sessionId :: String | r } Header
-- fetchEulerInternalApiKey =
--   getServiceConfigurationValueFromKey "EULER_INTERNAL_API_KEY"
--     >>= case _ of
--             Just value -> pure $ Header "Authorization" $ createBasicAuthFromCred value ""
--             Nothing    -> throwCustomException 500 "NOT FOUND EULER_INTERNAL_API_KEY" "EULER_INTERNAL_API_KEY"
--
-- getSimplSecretKey :: ∀ st r e. Newtype st (TState e) ⇒ BackendFlow st _ (Maybe String)
-- getSimplSecretKey = getServiceConfigurationValueFromKey "SIMPL_SECRET_KEY"
