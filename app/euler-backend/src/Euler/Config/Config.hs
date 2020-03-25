{-# LANGUAGE DeriveAnyClass #-}
module Euler.Config.Config where

import EulerHS.Prelude
import EulerHS.Types

import Euler.Config.EnvVars

import qualified Data.List.Extra as LE
import qualified Data.Text as Text (pack)


appVersion :: String
appVersion = take 7 sourceCommit

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
  deriving (Generic, Eq, Show)

data JWT = JWT
  { key    :: String
  , expiry :: String
  }
  deriving (Generic, Eq, Show)

getMandatePathPrefix :: String
getMandatePathPrefix = case getEnv of
  PROD -> "/mandate"
  _ -> "/ecr/mandate"

getEnv :: Env
getEnv = case _getEnv of
    "development" -> DEV
    "uat" -> UAT
    "production" -> PROD
    "integ" -> INTEG
    _ -> DEV

getECRConfig :: Config
getECRConfig = case getEnv of
    DEV  -> Config
        { protocol = "http"
        , host = "localhost:8081"
        , internalECHost = Text.pack getInternalECHost
        }
    UAT  -> Config
        { protocol = "https"
        , host = "sandbox.juspay.in"
        , internalECHost = Text.pack getInternalECHost
        }
    PROD -> Config
        { protocol = "https"
        , host = "api.juspay.in"
        , internalECHost = Text.pack getInternalECHost
        }
    INTEG -> Config
        { protocol = "https"
        , host = "integ-expresscheckout-api.juspay.in"
        , internalECHost = Text.pack getInternalECHost
        }

getECConfig :: Config
getECConfig = case getEnv of
    DEV  -> Config
        { protocol  = "https"
        , host = Text.pack getSandboxEcApiHost
        , internalECHost = Text.pack getInternalECHost
        }
    UAT  -> Config
        { protocol  = "https"
        , host = Text.pack getSandboxEcApiHost
        , internalECHost = Text.pack getInternalECHost
        }
    PROD -> Config
        { protocol  = "https"
        , host = Text.pack getProdEcApiHost
        , internalECHost = Text.pack getInternalECHost
        }
    INTEG -> Config
        { protocol  = "https"
        , host = Text.pack getIntegApiHost
        , internalECHost = Text.pack getInternalECHost
        }

getEulerConfig :: Config
getEulerConfig = do
  case getEnv of
    DEV  -> Config
        { protocol  = "http"
        , host = "localhost:8081"
        , internalECHost = Text.pack getInternalECHost
        }
    UAT  -> Config
        { protocol  = "https"
        , host = "beta.lambda.juspay.in"
        , internalECHost = Text.pack getInternalECHost
        }
    PROD -> Config
        { protocol  = "https"
        , host = "lambda.juspay.in"
        , internalECHost = Text.pack getInternalECHost
        }
    INTEG -> Config
        { protocol  = "https"
        , host = "integ-expresscheckout-api.juspay.in"
        , internalECHost = Text.pack getInternalECHost
        }

dashboardEndpoint :: Config
dashboardEndpoint = case getEnv of
  DEV -> Config
    { protocol  = "http"
    , host = "localhost:8081"
    , internalECHost = Text.pack getInternalECHost
    }
  UAT -> Config
    { protocol  = "https"
    , host = "euler.sandbox.juspay.in"
    , internalECHost = Text.pack getInternalECHost
    }
  PROD -> Config
    { protocol  = "https"
    , host = "lambda.juspay.in"
    , internalECHost = Text.pack getInternalECHost
    }
  INTEG -> Config
    { protocol  = "https"
    , host = "integ-expresscheckout-api.juspay.in"
    , internalECHost = Text.pack getInternalECHost
    }


getDashboardUrl :: Text
getDashboardUrl
  | isInteg = Text.pack getEulerIntegUrl
  | otherwise = protocol dashboardEndpoint <> "://" <> host dashboardEndpoint <> "/"

getBaseUrl :: Text
getBaseUrl = host getECConfig <> "/"

getEulerUrl :: Text
getEulerUrl = protocol getEulerConfig <> "://" <> host getEulerConfig<> "/"

getEulerUpiHost :: Text
getEulerUpiHost = case getEnv of
  PROD -> Text.pack getEulerUpiProdHost
  _    -> Text.pack getEulerUpiUatHost


getECRBaseUrl :: Text
getECRBaseUrl = protocol getECRConfig <> "://" <> host getECRConfig <> "/"

getLockerEndpoint :: Text
getLockerEndpoint = Text.pack getLockerHost

getExpectationsUrl :: Text
getExpectationsUrl = Text.pack expectationsBaseUrl <> "/"

getJWTConfig :: JWT
getJWTConfig = do
  case getEnv of
    DEV  -> JWT {key = "rSMPIDS9t0AtTpk1FSher5h1nMHCRXNJ", expiry = "180m"}
    UAT  -> JWT {key = "rSMPIDS9t0AtTpk1FSher5h1nMHCRXNJ", expiry = "30m"}
    PROD -> JWT {key = "rSMPIDS9t0AtTpk1FSher5h1nMHCRXNJ", expiry = "30m"}
    INTEG  -> JWT {key = "rSMPIDS9t0AtTpk1FSher5h1nMHCRXNJ", expiry = "180m"}

redisLoginTokenExpiry :: String
redisLoginTokenExpiry = let
  jwtExpiry = fromMaybe 180
    $ readMaybe
    $ filter (/='m')
    $ expiry getJWTConfig
  in show $ jwtExpiry * 60


-- decodeKMS not currently implemented
decodeKMS :: String -> IO String
decodeKMS = pure . id

decrypt :: String -> IO String
decrypt = decodeKMS

ecTempCardCred:: IO String
ecTempCardCred = case getEnv of
  PROD -> decrypt getECTempCardEncryptedKey
  UAT -> decrypt getECTempCardEncryptedKey
  INTEG -> decrypt getECTempCardEncryptedKey
  _ -> pure $ "bd3222130d110b9684dac9cc0903ce111b25e97ae93ddc2925f89c4ed6e41bab"

ecMandateParamsCred:: IO String
ecMandateParamsCred = case getEnv of
  PROD -> decrypt getECMandateParamsEncryptedKey
  UAT -> decrypt getECMandateParamsEncryptedKey
  INTEG -> decrypt getECMandateParamsEncryptedKey
  _ -> pure $ "a231ccb856c125486f1891bc5646f30949efcd6d1c14b6acc439ef928b133c32"

getMySQLCfg :: IO MySQLConfig
getMySQLCfg = case getEnv of
  DEV -> pure MySQLConfig
          { connectHost     = devMysqlConnectHost
          , connectPort     = devMysqlConnectPort
          , connectUser     = devMysqlConnectUser
          , connectPassword = devMysqlConnectPassword
          , connectDatabase = devMysqlConnectDatabase
          , connectOptions  = [CharsetName "utf8"]
          , connectPath     = devMysqlConnectPath
          , connectSSL      = Nothing
          }
  _ -> do
    decodedPassword <- decodeKMS getEcDbPass
    pure MySQLConfig
          { connectHost     = getEcDbHost
          , connectPort     = getEcDbPort
          , connectUser     = getEcDbUserName
          , connectPassword = decodedPassword
          , connectDatabase = getEcDbName
          , connectOptions  = [CharsetName "utf8"]
          , connectPath     = ""
          , connectSSL      = Nothing
          }

mySqlpoolConfig :: PoolConfig
mySqlpoolConfig = case getEnv of
  DEV -> PoolConfig
    { stripes = devMysqlPoolStripes
    , keepAlive = fromInteger devMysqlPoolKeepAlive
    , resourcesPerStripe = devMysqlPoolResourcesPerStripe
    }
  _ -> PoolConfig
    { stripes = devMysqlPoolStripes
    , keepAlive = fromInteger getMysqlPoolIdleTime
    , resourcesPerStripe = getMysqlPoolMax
    }


mysqlDBC = do
  mySqlConfig <- getMySQLCfg
  case getEnv of
    DEV -> pure $ mkMySQLPoolConfig (Text.pack devMysqlConnectionName) mySqlConfig mySqlpoolConfig
    _   -> pure $ mkMySQLPoolConfig (Text.pack devMysqlConnectionName) mySqlConfig mySqlpoolConfig

----DB
----read
--host
--port
--username
--password
--database
----write
--host
--port
--username
--password
--database
----pool
--min: getMysqlPoolMin
--max: getMysqlPoolMax
--idle: getMysqlPoolIdleTime
--acquire: getMysqlPoolAcquireTime
--ecDBCred :: Env -> IO (Options ConnOpts)
--ecDBCred env =
--  case env of
--    DEV -> pure (Conn.dialect := MySQL
--             <> Conn.host := "127.0.0.1"
--             <> Conn.port := 3306
--             <> Conn.username := "cloud"
--             <> Conn.password := "scape"
--             <> Conn.database := "jdb"
--             <> Conn.pool := { min: getMysqlPoolMin
--                             , max: getMysqlPoolMax
--                             , idle: getMysqlPoolIdleTime
--                             , acquire: getMysqlPoolAcquireTime
--                             }
--            <> Conn.benchmark := shouldLogQueryTime)
--    UAT -> do
--      password <- decrypt getEcDbPass
--      replicaPassword <- decrypt getEcDbPassR1
--      pure (Conn.dialect := MySQL
--        <> Conn.replication :=
--            { read: [{ host: getEcDbHostR1
--                    , username: getEcDbUserNameR1
--                    , password: replicaPassword
--                    , database: getEcDbNameR1
--                    , port: getEcDbPortR1
--                    }]
--            , write: { host: getEcDbHost
--                     , username: getEcDbUserName
--                     , password: password
--                     , database: getEcDbName
--                     , port: getEcDbPort
--                     }
--            }
--        <> Conn.pool := { min: getMysqlPoolMin
--                        , max: getMysqlPoolMax
--                        , idle: getMysqlPoolIdleTime
--                        , acquire: getMysqlPoolAcquireTime
--                        }
--        <> Conn.benchmark := shouldLogQueryTime)
--    INTEG -> do
--      password <- decrypt getEcDbPass
--      replicaPassword <- decrypt getEcDbPassR1
--      pure (Conn.dialect := MySQL
--        <> Conn.replication :=
--            { read: [{ host: getEcDbHostR1
--                    , username: getEcDbUserNameR1
--                    , password: replicaPassword
--                    , database: getEcDbNameR1
--                    , port: getEcDbPortR1
--                    }]
--            , write: { host: getEcDbHost
--                      , username: getEcDbUserName
--                      , password: password
--                      , database: getEcDbName
--                      , port: getEcDbPort
--                      }
--            }
--        <> Conn.pool := { min: getMysqlPoolMin
--                        , max: getMysqlPoolMax
--                        , idle: getMysqlPoolIdleTime
--                        , acquire: getMysqlPoolAcquireTime
--                        }
--        <> Conn.benchmark := shouldLogQueryTime)
--    PROD -> do
--      password <- decrypt getEcDbPass
--      replicaPassword <- decrypt getEcDbPassR1
--      pure (Conn.dialect := MySQL
--        <> Conn.replication :=
--            { read: [{ host: getEcDbHostR1
--                    , username: getEcDbUserNameR1
--                    , password: replicaPassword
--                    , database: getEcDbNameR1
--                    , port: getEcDbPortR1
--                    }]
--            , write: { host: getEcDbHost
--                     , username: getEcDbUserName
--                     , password: password
--                     , database: getEcDbName
--                     , port: getEcDbPort
--                     }
--            }
--        <> Conn.pool := { min: getMysqlPoolMin
--                        , max: getMysqlPoolMax
--                        , idle: getMysqlPoolIdleTime
--                        , acquire: getMysqlPoolAcquireTime
--                        }
--        <> Conn.benchmark := shouldLogQueryTime)

gatewaySchemeUrl :: String
gatewaySchemeUrl = case getEnv of
  DEV -> "https://s3.ap-south-1.amazonaws.com/jp-lambda-public-assets/uat/gateway-configs/gateway_schema_uat.json"
  UAT -> "https://s3.ap-south-1.amazonaws.com/jp-lambda-public-assets/uat/gateway-configs/gateway_schema_uat.json"
  PROD -> "https://s3.ap-south-1.amazonaws.com/jp-lambda-public-assets/prod/gateway-configs/gateway_schema_prod.json"
  INTEG -> "https://s3.ap-south-1.amazonaws.com/jp-lambda-public-assets/uat/gateway-configs/gateway_schema_uat.json"

merchantAgencyRedisExpiry :: String
merchantAgencyRedisExpiry = "604800" -- 7 days

sevenDayExpiry :: String
sevenDayExpiry = "604800"

defaultLockerId :: String
defaultLockerId = "m0010"

passwordResetLinkTimeout :: String
passwordResetLinkTimeout = "86400" -- 24 hrs

retryAttemptCountExpiry :: String
retryAttemptCountExpiry = "18000" -- 5 hrs

totpSetupTimeout :: String
totpSetupTimeout = "300" -- 5 Min

--orderTtl :: Milliseconds
--orderTtl = convertDuration $ Hours 24.0
orderTtl :: Int -- seconds (because EulerHS setCacheWithExpiry(setex) take ttl in seconds)
orderTtl = 24 * 60 * 60

--payTMOtpVerificationExpiyTime :: Milliseconds
--payTMOtpVerificationExpiyTime =  convertDuration $ Minutes 15.00
payTMOtpVerificationExpiyTime :: Int
payTMOtpVerificationExpiyTime = 15*60

--freeChargeOtpExp :: Milliseconds
--freeChargeOtpExp =  convertDuration $ Minutes 15.00 -- 15 Min
freeChargeOtpExp :: Int
freeChargeOtpExp = 15 * 60


-- airtelMoneyOtpExp :: Milliseconds
-- airtelMoneyOtpExp =  convertDuration $ Minutes 15.00 -- 15 Min
airtelMoneyOtpExp :: Int
airtelMoneyOtpExp = 15 * 60

configTTL :: String
configTTL = "86400" -- 24 hours

clientAuthTokenTTL :: String
clientAuthTokenTTL = "86400" -- 24 hours


-- TO-DO : add getDBConfig
-- eulerSchema :: String
-- eulerSchema = getDBConfig "schema"

razorpayOAuthUrl :: String
razorpayOAuthUrl = "https://auth.razorpay.com"

razorpayClientId :: Bool -> String
razorpayClientId testMode = if testMode then "A0m8HbNtJUSHjZ" else "A0m8HqOuOvde27"

razorpayClientSecret :: String -> IO String
razorpayClientSecret clientSecret = decrypt clientSecret

razorpayRedirectUrl :: Bool -> Text
razorpayRedirectUrl testMode = if testMode
  then getDashboardUrl <> "express-checkout/api/razorpay/callback"
  else "https://dashboard.expresscheckout.juspay.in/api/razorpay/callback"

razorpayOAuthMode :: Bool -> String
razorpayOAuthMode testMode = if testMode then "test" else "live"

shouldLogQueryTime :: Bool
shouldLogQueryTime = case getEnv of
  DEV -> True
  UAT -> True
  PROD -> True
  INTEG -> True

orderTokenExpiry :: Int
orderTokenExpiry = 900 -- 15 Min

orderTokenMaxUsage :: Int
orderTokenMaxUsage = 20

paypalTokenTtl :: Int
paypalTokenTtl = (9 * 60 * 60) - 5

enableScheduler :: Bool
enableScheduler = isSchedulerEnabled
--  case getEnv of
--    DEV -> isSchedulerEnabled == "true"
--    UAT -> isSchedulerEnabled == "true"
--    PROD -> isSchedulerEnabled == "true"
--    INTEG -> isSchedulerEnabled == "true"


-- Sync Refund Configs --
syncRefundInterval :: Double
syncRefundInterval =  getSyncRefundInterval -- 6 hours

syncRefundLaterInterval :: Double
syncRefundLaterInterval = getRefundWaitTime -- 1 day

maxSyncRefundAttempts :: String -> Int
maxSyncRefundAttempts gateway = case gateway of
                                  "MORPHEUS" -> getMaxInstantSyncRefundAttempts
                                  _          -> getMaxSyncRefundAttempts

syncInstantRefundIntervalMin :: Double
syncInstantRefundIntervalMin = getInstantRefundWaitTimeInMin -- 10 min

syncInstantRefundIntervalHour :: Double
syncInstantRefundIntervalHour = getInstantRefundWaitTimeInHour -- 1 hour


getwaitTimeForRefund :: String -> Int -> Double
getwaitTimeForRefund gateway count = do
    case gateway of
      "MORPHEUS" -> if count < getMaxInstantSyncRefundAttemptsInMin
                      then syncInstantRefundIntervalMin
                      else if count < getMaxInstantSyncRefundAttemptsInHours
                        then syncInstantRefundIntervalHour
                        else syncRefundLaterInterval
      _          -> if count < 4
                      then syncRefundInterval -- 10 mins
                      else  syncRefundLaterInterval

syncArnInterval :: Double
syncArnInterval = getSyncArnInterval

getwaitTimeForArn :: Int -> Double
getwaitTimeForArn count = do
    if count < 5
        then syncArnInterval -- 1 day
        else getArnWaitTime

maxSyncArnAttempts :: Int
maxSyncArnAttempts = getMaxSyncArnAttempts

-- WEBHOOK Hit Configs --
maxAttemptsForWebhook :: Int
maxAttemptsForWebhook = 3

getWaitTimeForWebhook :: Int -> Double
getWaitTimeForWebhook count = do
  if count < 2
    then 10.0 * 1000.0 -- 10 secs
    else 15.0 * 60.0 * 1000.0 -- 1 hour

--exMandateReqTTL :: Milliseconds
--exMandateReqTTL = convertDuration $ Hours 24.0
exMandateReqTTL :: Int -- seconds
exMandateReqTTL = 24 * 60 * 60

exMandateRateLimit :: String
exMandateRateLimit = "10"

wrongPasswordAttemptCountExpiry :: String
wrongPasswordAttemptCountExpiry = "900" -- 15 min

defaultMaxWrongPasswordAttemptsCount :: Int
defaultMaxWrongPasswordAttemptsCount = 5


newtype GatewayUrls = GatewayUrls { payu :: PayUGatewayUrls }


data PayUGatewayUrls = PayUGatewayUrls
  { exerciseMandate :: String
  , paymentRequest :: String
  , checkStatus :: String
  }
  deriving (Generic, Eq, Show)

getPayUGatewayUrls :: PayUGatewayUrls
getPayUGatewayUrls = do
  case getEnv of
    DEV  -> PayUGatewayUrls
        { exerciseMandate = "https://test.payu.in/merchant/postservice.php?form=2"
        , paymentRequest  = "https://test.payu.in/_payment"
        , checkStatus = "https://test.payu.in/merchant/postservice.php?form=2"
        }
    UAT  -> PayUGatewayUrls
        { exerciseMandate = "https://test.payu.in/merchant/postservice.php?form=2"
        , paymentRequest  = "https://test.payu.in/_payment"
        , checkStatus = "https://test.payu.in/merchant/postservice.php?form=2"
        }
    INTEG  -> PayUGatewayUrls
        { exerciseMandate = "https://test.payu.in/merchant/postservice.php?form=2"
        , paymentRequest  = "https://test.payu.in/_payment"
        , checkStatus = "https://test.payu.in/merchant/postservice.php?form=2"
        }
    PROD -> PayUGatewayUrls
        { exerciseMandate = "https://info.payu.in/merchant/postservice.php?form=2"
        , paymentRequest  = "https://secure.payu.in/_payment"
        , checkStatus = "https://info.payu.in/merchant/postservice.php?form=2"
        }

data InternalUrls = InternalUrls
  { addCardToLocker :: String
  , listCardFromLocker :: String
  , getCardFromLocker :: String
  }
  deriving (Generic, Eq, Show)

getInternalUrls :: InternalUrls
getInternalUrls = InternalUrls
  { addCardToLocker = getLockerHost <> "/card/addCard"
  , listCardFromLocker = getLockerHost <> "/card/listCardForCustomer"
  , getCardFromLocker = getLockerHost <> "/card/getCard"
  }


paymentPageBundleEnv :: String
paymentPageBundleEnv = case getEnv of
  DEV   -> "beta"
  INTEG -> "beta"
  UAT   -> "beta"
  PROD  -> "release"

paymentPageBundleBaseUrl :: String
paymentPageBundleBaseUrl = case getEnv of
  DEV   -> "https://assets.juspay.in/juspay/payments/in.hyper.pay."
  INTEG -> "https://assets.juspay.in/juspay/payments/in.hyper.pay."
  UAT   -> "https://assets.juspay.in/juspay/payments/in.hyper.pay."
  PROD  -> ""

paymentSourceEndpoint :: Config
paymentSourceEndpoint = case getEnv of
  DEV -> Config
    { protocol  = "http"
    , host = "localhost:8081"
    , internalECHost = Text.pack getInternalECHost
    }
  INTEG -> Config
    { protocol  = "http"
    , host = "integ-expresscheckout-api.juspay.in"
    , internalECHost = Text.pack getInternalECHost
    }
  UAT -> Config
    { protocol  = "https"
    , host = "euler.sandbox.juspay.in"
    , internalECHost = Text.pack getInternalECHost
    }
  PROD -> Config
    { protocol  = "https"
    , host = "api.juspay.in"
    , internalECHost = Text.pack getInternalECHost
    }

getPaymentSourceUrl :: Text
getPaymentSourceUrl = protocol paymentSourceEndpoint <> "://" <> host paymentSourceEndpoint <> "/"

maxRetryExecuteRefundAttempts :: Int
maxRetryExecuteRefundAttempts = getMaxRetryExecuteRefundAttempts

retryExecuteRefundInterval :: Int -- seconds
retryExecuteRefundInterval = getRetryExecuteRefundInterval

getSchedulerRunners' :: [String]
getSchedulerRunners' = LE.upper . LE.trim <$> LE.split (==',') getSchedulerRunners

redis :: Text
redis = Text.pack standaloneRedisName

redisCluster :: Text
redisCluster = Text.pack clusterRedisName