{-# LANGUAGE DeriveAnyClass #-}
module Euler.Config.Config where

import           EulerHS.Prelude

import qualified Euler.Encryption as E
import           EulerHS.Language

-- import           Data.Time
import           System.Environment (lookupEnv)


import qualified Data.ByteString.Base16 as B16
import qualified Data.Text as Text (pack)

-- EHS: should be Money.
mandateMaxAmountAllowed :: Text
mandateMaxAmountAllowed = "100001.00"

mandateTtl :: Int -- seconds
mandateTtl = 60 * 60 * 24 -- 24 Hours

data Config = Config
  { protocol       :: Text
  , host           :: Text
  , internalECHost :: Text
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

defaultConfig :: Config
defaultConfig = Config
    { protocol = "https"
    , host = "defaulthost"
    , internalECHost = "defaultInternalECHost"
    }

data Env = DEV | UAT | PROD | INTEG
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

getEnv :: IO Env
getEnv = do
  me <- lookupEnv "APP_ENV"
  case me of
    Just "development" -> pure DEV
    Just "uat"         -> pure UAT
    Just "production"  -> pure PROD
    Just "integ"       -> pure INTEG
    _                  -> pure DEV

getInternalECHost :: IO Text
getInternalECHost = do
  mh <- lookupEnv "INTERNAL_EC_HOST"
  pure $ fromMaybe "https://sandbox.juspay.in/internal-ec" (Text.pack <$> mh)

getECRConfig :: IO Config
getECRConfig = do
  env <- getEnv
  internalECHost <- getInternalECHost
  case env of
    DEV  -> pure $ Config
        { protocol = "http"
        , host = "localhost:8081"
        , internalECHost = internalECHost
        }
    UAT  -> pure $ Config
        { protocol = "https"
        , host = "sandbox.juspay.in"
        , internalECHost = internalECHost
        }
    PROD -> pure $ Config
        { protocol = "https"
        , host = "api.juspay.in"
        , internalECHost = internalECHost
        }
    INTEG -> pure $ Config
        { protocol = "https"
        , host = "integ-expresscheckout-api.juspay.in"
        , internalECHost = internalECHost
        }

--orderTtl :: Milliseconds
--orderTtl = convertDuration $ Hours 24.0
orderTtl :: Int -- seconds (because EulerHS setCacheWithExpiry(setex) take ttl in seconds)
orderTtl = 24 * 60 * 60

orderTokenExpiry :: Int
orderTokenExpiry = 900 -- 15 Min

orderTokenMaxUsage :: Int
orderTokenMaxUsage = 20


ecTempCardCred:: Flow (E.Key E.AES256 ByteString)
ecTempCardCred = do
  env <- runIO getEnv
  case env of
    PROD -> decodeECTempCardKey <$> getECTempCardEncryptedKey
    UAT -> decodeECTempCardKey <$> getECTempCardEncryptedKey
    INTEG -> decodeECTempCardKey <$> getECTempCardEncryptedKey
    _ -> pure defaultEcTempCardCredKey

getECTempCardEncryptedKey :: Flow (Maybe String)
getECTempCardEncryptedKey = runIO $ lookupEnv "EC_TEMP_CARD_AES_KEY"

defaultEcTempCardCredKey :: E.Key E.AES256 ByteString
defaultEcTempCardCredKey = E.Key $ fst $ B16.decode "bd3222130d110b9684dac9cc0903ce111b25e97ae93ddc2925f89c4ed6e41bab"

-- EHS: TODO: port for decryptPromotionRules from Euler.Storage.Repository.Promotion
-- former decodeKMS
decodeECTempCardKey :: Maybe String -> E.Key E.AES256 ByteString
decodeECTempCardKey = undefined


-- decrypt :: forall e. String -> Aff e String
-- decrypt = toAff <<< decodeKMS

-- exports.getECTempCardEncryptedKey = process.env.EC_TEMP_CARD_AES_KEY

-- exports.decodeKMS = function(val) {
--   AWS.config.update(config.aws);
--   var kms = new AWS.KMS();

--   var encryptedParams = {
--     CiphertextBlob: Buffer(val, "base64")
--   };

--   return kms
--     .decrypt(encryptedParams)
--     .promise()
--     .then(function(data) {
--       var decryptedString = data.Plaintext.toString("ascii");
--       return Promise.resolve(decryptedString);
--     })
--     .catch(function(err) {
--       return Promise.reject(new Error(err));
--     });
-- };
