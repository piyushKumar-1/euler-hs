{-# LANGUAGE DeriveAnyClass #-}
module Euler.Config.Config where

import           EulerHS.Prelude


import           Data.Time
import           System.Environment (lookupEnv)

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

-- Two versions needed to use in 'getCurrentDateStringWithOffset' while time library is below 1.9.1
orderTokenExpiryND :: NominalDiffTime
orderTokenExpiryND = 900 -- 15 Min

orderTokenExpiryI :: Int
orderTokenExpiryI = 900 -- 15 Min

orderTokenMaxUsage :: Int
orderTokenMaxUsage = 20
