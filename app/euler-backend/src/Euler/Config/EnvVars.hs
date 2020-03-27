module Euler.Config.EnvVars where

-- original purescript src/Config/Config.js

import EulerHS.Prelude

import qualified Data.Map as Map
import           Data.Char (toUpper)
import           System.Environment (getEnvironment)
import           System.IO.Unsafe (unsafePerformIO)

toTitle :: String -> String
toTitle "" = ""
toTitle (x:xs) = toUpper x : xs

{-# NOINLINE environmentVars #-}
environmentVars :: Map String String
environmentVars = Map.fromList $ unsafePerformIO getEnvironment

lookupEnv :: String -> Maybe String
lookupEnv k = Map.lookup k environmentVars

-- Environvent parameters

getCacheConfigInterval :: Double -- maybe Int ?
getCacheConfigInterval = fromMaybe 300000.0 $ readMaybe =<< lookupEnv "CACHE_CONFIG_INTERVAl"

getEulerDbPass :: String
getEulerDbPass = fromMaybe "nodefaultvalue" $ lookupEnv "EULER_DB_PASSWORD"

getEulerDbHost :: String
getEulerDbHost = fromMaybe "jpg-beta.cyijte0yeu00.ap-southeast-1.rds.amazonaws.com"
  $ lookupEnv "EULER_DB_HOST"

getEulerDbUserName :: String
getEulerDbUserName = fromMaybe "jp_euler_db_001" $ lookupEnv "EULER_DB_USERNAME"

getEulerDbPort :: Int
getEulerDbPort = fromMaybe 5432 $ readMaybe =<< lookupEnv "EULER_DB_PORT"

getEulerDbSchema :: String
getEulerDbSchema = fromMaybe "jp_hpcl" $ lookupEnv "EULER_DB_SCHEMA"

getEulerDbName :: String
getEulerDbName = fromMaybe "jp_euler_db" $ lookupEnv "EULER_DB_NAME"

-- Euler read replica details (fallback to master if not applicable)

getEulerDbPassR1 :: String
getEulerDbPassR1 = fromMaybe getEulerDbPass $ lookupEnv "EULER_PASSWORD_R1"

getEulerDbHostR1 :: String
getEulerDbHostR1 = fromMaybe getEulerDbHost $ lookupEnv "EULER_HOST_R1"

getEulerDbUserNameR1 :: String
getEulerDbUserNameR1 = fromMaybe getEulerDbUserName $ lookupEnv "EULER_USERNAME_R1"

getEulerDbPortR1 :: Int
getEulerDbPortR1 = fromMaybe getEulerDbPort $ readMaybe =<< lookupEnv "EULER_PORT_R1"

getEulerDbNameR1 :: String
getEulerDbNameR1 = fromMaybe getEulerDbName $ lookupEnv "EULER_NAME_R1"

--

getEcDbPass :: String
getEcDbPass = fromMaybe "nodefaultvalue" $ lookupEnv "EC_DB_PASSWORD";

getEcDbHost :: String
getEcDbHost = fromMaybe "beta.expresscheckout-db.juspay.in" $ lookupEnv "EC_DB_HOST"

getEcDbUserName :: String
getEcDbUserName = fromMaybe "app_rw" $ lookupEnv "EC_DB_USERNAME"

getEcDbPort :: Word16
getEcDbPort = fromMaybe 3306 $ readMaybe =<< lookupEnv "EC_DB_PORT"

getEcDbName :: String
getEcDbName = fromMaybe "jdb" $ lookupEnv "EC_DB_NAME"

getEcDbPassR1 :: String
getEcDbPassR1 = fromMaybe "nodefaultvalue" $ lookupEnv "EC_DB_PASSWORD_R1";

getEcDbHostR1 :: String
getEcDbHostR1 = fromMaybe "jpdb-beta-slave.cyijte0yeu00.ap-southeast-1.rds.amazonaws.com"
  $ lookupEnv "EC_DB_HOST_R1"

getEcDbUserNameR1 :: String
getEcDbUserNameR1 = fromMaybe "app_rw" $ lookupEnv "EC_DB_USERNAME_R1"

getEcDbPortR1 :: Int
getEcDbPortR1 = fromMaybe 3306 $ readMaybe =<< lookupEnv "EC_DB_PORT_R1"

getEcDbNameR1 :: String
getEcDbNameR1 = fromMaybe "jdb" $ lookupEnv "EC_DB_NAME_R1"

-- Reporting DB EC

getEcReportingDbHost :: String
getEcReportingDbHost = fromMaybe "reporting.in-west.slave.jpdb.juspay.in"
  $ lookupEnv "EC_DB_REPORTING_HOST"

getEcReportingDbUserName :: String
getEcReportingDbUserName = fromMaybe "app_rw" $ lookupEnv "EC_DB_REPORTING_USERNAME"

getEcReportingDbPort :: Int
getEcReportingDbPort = fromMaybe 3306 $ readMaybe =<< lookupEnv "EC_DB_REPORTING_PORT"

getEcReportingDbName :: String
getEcReportingDbName = fromMaybe "jdb" $ lookupEnv "EC_DB_REPORTING_DB_NAME"

getEcReportingDbPassword :: String
getEcReportingDbPassword = fromMaybe "nodefaultvalue" $ lookupEnv "EC_DB_REPORTING_DB_PASSWORD"

getECTempCardEncryptedKey :: String
getECTempCardEncryptedKey = fromMaybe "nodefaultvalue" $ lookupEnv "EC_TEMP_CARD_AES_KEY"

getInternalECHost :: String
getInternalECHost = fromMaybe "http://api-internal.in-west-1.juspay.in" $ lookupEnv "INTERNAL_EC_HOST"

-- For order sync proxy to ec. We must proxy to EC utility servers.


getInternalECUtilsHost :: String
getInternalECUtilsHost = fromMaybe "https://internal-utils-api.in-west-1.juspay.in" $ lookupEnv "INTERNAL_EC_UTILS_HOST"

getMysqlPoolMin :: Int
getMysqlPoolMin = fromMaybe 0 $ readMaybe =<< lookupEnv "MYSQL_POOL_MIN"

getMysqlPoolMax :: Int
getMysqlPoolMax = fromMaybe 5 $ readMaybe =<< lookupEnv "MYSQL_POOL_MAX"

getMysqlPoolIdleTime :: Integer
getMysqlPoolIdleTime = fromMaybe 20000 $ readMaybe =<< lookupEnv "MYSQL_POOL_IDLE_TIME"

getMysqlPoolAcquireTime :: Int
getMysqlPoolAcquireTime = fromMaybe 30000 $ readMaybe =<< lookupEnv "MYSQL_POOL_ACQUIRE_TIME"

getPostgresPoolMin :: Int
getPostgresPoolMin = fromMaybe 0 $ readMaybe =<< lookupEnv "POSTGRES_POOL_MIN"

getPostgresPoolMax :: Int
getPostgresPoolMax = fromMaybe 5 $ readMaybe =<< lookupEnv "POSTGRES_POOL_MAX"

getPostgresPoolIdleTime :: Int
getPostgresPoolIdleTime = fromMaybe 20000 $ readMaybe =<< lookupEnv "POSTGRES_POOL_IDLE_TIME"

getPostgresPoolAcquireTime :: Int
getPostgresPoolAcquireTime = fromMaybe 30000 $ readMaybe =<< lookupEnv "POSTGRES_POOL_ACQUIRE_TIME"

getProdEcApiHost :: String
getProdEcApiHost = fromMaybe "https://api.juspay.in" $ lookupEnv "INTERNAL_EC_HOST"

getSandboxEcApiHost :: String
getSandboxEcApiHost = fromMaybe "https://sandbox.juspay.in" $ lookupEnv "INTERNAL_EC_HOST"

getIntegApiHost :: String
getIntegApiHost = fromMaybe "http://integ-euler-api.juspay.in" $ lookupEnv "INTERNAL_EC_HOST"

_getEnv :: String
_getEnv = fromMaybe "development" $ lookupEnv "NODE_ENV"

isInteg :: Bool
isInteg = fromMaybe False $ readMaybe =<< lookupEnv "INTEG_ENV"

getEulerIntegUrl :: String
getEulerIntegUrl = fromMaybe "http://integ-euler-api.juspay.in/" $ lookupEnv "EULER_INTEG_URL"

getLockerHost :: String
getLockerHost = fromMaybe "http://54.251.145.187:8080" $ lookupEnv "LOCKER_HOST"

expectationsBaseUrl :: String
expectationsBaseUrl = fromMaybe "https://expectations.internal.svc.k8s.staging.juspay.net"
  $ lookupEnv "EXPECTATIONS_URL"

expectationsSystem :: String
expectationsSystem = fromMaybe "euler_prod" $ lookupEnv "EXPECTATIONS_SYSTEM"

getRefundEnabledMerchants :: [String]
getRefundEnabledMerchants = fromMaybe [] $ readMaybe =<< lookupEnv "REFUND_ENABLED_MERCHANTS"

getRefundSupportedGws :: [String]
getRefundSupportedGws = fromMaybe [] $readMaybe =<< lookupEnv "REFUND_SUPPORTED_GW"

getECMandateParamsEncryptedKey :: String
getECMandateParamsEncryptedKey = fromMaybe "a231ccb856c125486f1891bc5646f30949efcd6d1c14b6acc439ef928b133c32"
  $ lookupEnv "EC_MANDATE_PARAMS_AES_KEY"

getEncDecServiceUrl :: String
getEncDecServiceUrl = fromMaybe "nodefaultvalue" $ lookupEnv "ENCDEC_SERVICE_URL"

isSchedulerEnabled :: Bool
isSchedulerEnabled = fromMaybe False $ readMaybe =<< toTitle <$> lookupEnv "ENABLE_SCHEDULER"

getProcessCount :: String
getProcessCount = fromMaybe "nodefaultvalue" $ lookupEnv "PROCESS_COUNT"

getSchedulerRunners :: String
getSchedulerRunners = fromMaybe "REFUND_WORKFLOW_EULER_EC,WEBHOOK_GENERIC_EULER_EC" $ lookupEnv "SCHEDULER_RUNNERS"

schedulerFetchLimit :: Int
schedulerFetchLimit = fromMaybe 100 $ readMaybe =<< lookupEnv "SCHEDULER_FETCH_LIMIT"

schedulerLooperInterval :: Int
schedulerLooperInterval = fromMaybe 10000 $ readMaybe =<< lookupEnv "SCHEDULER_LOOPER_INTERVAL"

sourceCommit :: String
sourceCommit = fromMaybe "NA" $ lookupEnv "SOURCE_COMMIT"

service :: String
service = fromMaybe "NA" $ lookupEnv "SERVICE"

getEulerUpiProdHost :: String
getEulerUpiProdHost = fromMaybe "https://lambda.juspay.in" $ lookupEnv "EULER_UPI_INTERNAL_PROD_HOST"

getEulerUpiUatHost :: String
getEulerUpiUatHost = fromMaybe "https://beta.lambda.juspay.in" $ lookupEnv "EULER_UPI_INTERNAL_UAT_HOST"

getSyncRefundInterval :: Double -- maybe Int ?
getSyncRefundInterval = fromMaybe 21600000.0 $ readMaybe =<< lookupEnv "SYNC_REFUND_INTERVAL" -- 6 hours

getInstantRefundWaitTime :: Double -- maybe Int ?
getInstantRefundWaitTime = fromMaybe 300000.0 $ readMaybe =<< lookupEnv "INSTANT_REFUND_QUEUE_WAIT_TIME" -- 5 min

getInstantSyncRefundInterval :: Double -- maybe Int ?
getInstantSyncRefundInterval = fromMaybe 21600000.0 $ readMaybe =<< lookupEnv "INSTANT_SYNC_REFUND_INTERVAL" -- 6 hours

getMaxSyncRefundAttempts :: Int
getMaxSyncRefundAttempts = fromMaybe 10 $ readMaybe =<< lookupEnv "MAX_SYNC_REFUND_ATTEMPTS"

getMaxInstantSyncRefundAttempts :: Int
getMaxInstantSyncRefundAttempts = fromMaybe 81 $ readMaybe =<< lookupEnv "MAX_SYNC_INSTANT_REFUND_ATTEMPTS"

getMaxInstantSyncRefundAttemptsInMin :: Int
getMaxInstantSyncRefundAttemptsInMin = fromMaybe 6 $ readMaybe =<< lookupEnv "MAX_SYNC_INSTANT_REFUND_ATTEMPTS_IN_MIN"

getMaxInstantSyncRefundAttemptsInHours :: Int
getMaxInstantSyncRefundAttemptsInHours = fromMaybe 77 $ readMaybe =<< lookupEnv "MAX_SYNC_INSTANT_REFUND_ATTEMPTS_IN_HOURS"

getRefundWaitTime :: Double -- maybe Int ?
getRefundWaitTime = fromMaybe 86400000.0 $ readMaybe =<< lookupEnv "REFUND_WAIT_TIME" -- 1 day

getSyncArnInterval :: Double -- maybe Int ?
getSyncArnInterval = fromMaybe 86400000.0 $ readMaybe =<< lookupEnv "SYNC_ARN_INTERVAL" -- 1 day

getArnWaitTime :: Double -- maybe Int ?
getArnWaitTime = fromMaybe 259200000.0 $ readMaybe =<< lookupEnv "ARN_WAIT_TIME" -- 3 days

getMaxSyncArnAttempts :: Int
getMaxSyncArnAttempts = fromMaybe 5 $ readMaybe =<< lookupEnv "MAX_SYNC_ARN_ATTEMPTS"

getRetryExecuteRefundInterval :: Int
getRetryExecuteRefundInterval = fromMaybe 1800 $ readMaybe =<< lookupEnv "RETRY_EXECUTE_REFUND_INTERVAL" -- 30 mins

getMaxRetryExecuteRefundAttempts :: Int
getMaxRetryExecuteRefundAttempts = fromMaybe 4 $ readMaybe =<< lookupEnv "MAX_RETRY_EXECUTE_REFUND_ATTEMPTS"

getInstantRefundWaitTimeInMin :: Double -- maybe Int ?
getInstantRefundWaitTimeInMin = fromMaybe 600000.0 $ readMaybe =<< lookupEnv "INSTANT_SYNC_REFUND_INTERVAL_MIN"

getInstantRefundWaitTimeInHour :: Double -- maybe Int ?
getInstantRefundWaitTimeInHour = fromMaybe 3600000.0 $ readMaybe =<< lookupEnv "INSTANT_SYNC_REFUND_INTERVAL_HOUR" -- 1 hour

getAddCardToLockerAPITimeout :: Int
getAddCardToLockerAPITimeout = fromMaybe 5000 $ readMaybe =<< lookupEnv "ADD_CARD_TO_LOCKER_API_TIMEOUT"

getMorpheusHostFromConfig :: String
getMorpheusHostFromConfig = fromMaybe "https://api.juspay.in" $ lookupEnv "MORPHEUS_INTERNAL_HOST"

getDBSyncStream :: String
getDBSyncStream = fromMaybe "" $ lookupEnv "DBSYNC_STREAM"

getAwsRegion :: String
getAwsRegion = fromMaybe "ap-south-1" $ lookupEnv "AWS_REGION"

-- https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#key-id
getKmsKeyId :: String
getKmsKeyId = fromMaybe "arn:aws:kms:ap-south-1:980691203742:key/2ee54fc6-9bd4-4f9c-8cca-e1c179859a8e" $ lookupEnv "KMS_KEY_ID"

standaloneRedisName :: String
standaloneRedisName = fromMaybe "redis" $ lookupEnv "STANDALONE_REDIS_NAME"

clusterRedisName :: String
clusterRedisName = fromMaybe "redisCluster" $ lookupEnv "CLUSTER_REDIS_NAME"

-- DEV MySQL parameters
devMysqlConnectionName :: String
devMysqlConnectionName = fromMaybe "eulerMysqlDB" $ lookupEnv "DEV_MYSQL_CONNECTION_NAME"

devMysqlConnectHost :: String
devMysqlConnectHost = fromMaybe "localhost" $ lookupEnv "DEV_MYSQL_CONNECT_HOST"

devMysqlConnectPort :: Word16
devMysqlConnectPort = fromMaybe 3306 $ readMaybe =<< lookupEnv "DEV_MYSQL_CONNECT_PORT"

devMysqlConnectUser :: String
devMysqlConnectUser = fromMaybe "cloud" $ lookupEnv "DEV_MYSQL_CONNECT_USER"

devMysqlConnectPassword :: String
devMysqlConnectPassword = fromMaybe "scape" $ lookupEnv "DEV_MYSQL_CONNECT_PASSWORD"

devMysqlConnectDatabase :: String
devMysqlConnectDatabase = fromMaybe "jdb" $ lookupEnv "DEV_MYSQL_CONNECT_DATABASE"

devMysqlConnectPath :: String
devMysqlConnectPath = fromMaybe "" $ lookupEnv "DEV_MYSQL_CONNECT_PATH"

devMysqlPoolStripes :: Int
devMysqlPoolStripes = fromMaybe 1 $ readMaybe =<< lookupEnv "DEV_MYSQL_POOL_STRIPES"

devMysqlPoolKeepAlive :: Integer
devMysqlPoolKeepAlive = fromMaybe 10 $ readMaybe =<< lookupEnv "DEV_MYSQL_POOL_KEEP_ALIVE"

devMysqlPoolResourcesPerStripe :: Int
devMysqlPoolResourcesPerStripe = fromMaybe 50 $ readMaybe =<< lookupEnv "DEV_MYSQL_POOL_RESOURCES_PER_STRIPE"

