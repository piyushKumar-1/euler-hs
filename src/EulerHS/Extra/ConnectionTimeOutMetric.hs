{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DerivingStrategies  #-}

module EulerHS.Extra.ConnectionTimeOutMetric where

import qualified EulerHS.Types as T
import           Euler.Events.MetricApi.MetricApi
import qualified EulerHS.Framework.Language as L
import           EulerHS.Prelude
import qualified Juspay.Extra.Config as Conf

-- set this value "DB_METRIC_ENABLED" to enable this metric

incrementConnectionTimeOutMetric :: L.MonadFlow m => ConnectionTimeOutMetricHandle -> TimeoutCounter -> Text -> Text -> m ()
incrementConnectionTimeOutMetric handle metric dbName hostName = do
  L.runIO $ handle.connectTimeoutCounter (metric, dbName, hostName)

data ConnectionTimeOutMetricHandle = ConnectionTimeOutMetricHandle
  { connectTimeoutCounter :: (TimeoutCounter, Text, Text) -> IO ()
  }

data TimeoutCounter
  = ConnectionTimeout
  | ConnectionFailed
  | ConnectionDoesNotExist
  | ConnectionAlreadyExists
  | TransactionRollbacked
--   | SQLQueryError
  | UnrecognizedDBError
  | UnexpectedDBResult
  | RedisExceptionMessage

mkConnectionTimeOutHandle :: IO ConnectionTimeOutMetricHandle
mkConnectionTimeOutHandle = do
  metrics <- register collectionLock
  pure $ ConnectionTimeOutMetricHandle $ \case
    (ConnectionTimeout, dbName, hostName)   ->
      inc (metrics </> #connection_timeout) dbName hostName
    (ConnectionFailed, dbName, hostName)    ->
      inc (metrics </> #connection_failed) dbName hostName
    (ConnectionDoesNotExist, dbName, hostName)    ->
      inc (metrics </> #connection_doesnot_exist) dbName hostName
    (ConnectionAlreadyExists, dbName, hostName)    ->
      inc (metrics </> #connection_already_exists) dbName hostName
    (TransactionRollbacked, dbName, hostName)    ->
      inc (metrics </> #transaction_rollbacked) dbName hostName
    -- (SQLQueryError,dbName, hostName)    ->
    --   inc (metrics </> #sql_query_error) dbName hostName
    (UnrecognizedDBError, dbName, hostName)    ->
      inc (metrics </> #unrecognized_db_error) dbName hostName
    (UnexpectedDBResult, dbName, hostName)    ->
      inc (metrics </> #unexpected_db_result) dbName hostName
    (RedisExceptionMessage, dbName, hostName)    ->
      inc (metrics </> #redis_exception_msg) dbName hostName

connection_timeout = counter #connection_timeout
      .& lbl @"db_name" @Text
      .& lbl @"host_name" @Text
      .& build

connection_failed = counter #connection_failed
      .& lbl @"db_name" @Text
      .& lbl @"host_name" @Text
      .& build

connection_doesnot_exist = counter #connection_doesnot_exist
      .& lbl @"db_name" @Text
      .& lbl @"host_name" @Text
      .& build

connection_already_exists = counter #connection_already_exists
      .& lbl @"db_name" @Text
      .& lbl @"host_name" @Text
      .& build

-- sql_query_error = counter #sql_query_error
--       .& lbl @"db_name" @Text
--       .& lbl @"host_name" @Text
--       .& build

transaction_rollbacked = counter #transaction_rollbacked
      .& lbl @"db_name" @Text
      .& lbl @"host_name" @Text
      .& build

unrecognized_db_error = counter #unrecognized_db_error
      .& lbl @"db_name" @Text
      .& lbl @"host_name" @Text
      .& build


unexpected_db_result = counter #unexpected_db_result
      .& lbl @"db_name" @Text
      .& lbl @"host_name" @Text
      .& build

redis_exception_msg = counter #redis_exception_msg
      .& lbl @"db_name" @Text
      .& lbl @"host_name" @Text
      .& build

collectionLock =
     connection_timeout
  .> connection_failed
  .> connection_doesnot_exist
  .> connection_already_exists
--   .> sql_query_error
  .> transaction_rollbacked
  .> unrecognized_db_error
  .> unexpected_db_result
  .> redis_exception_msg
  .> MNil


------------------------------------------------------------

data EulerRedisCfg = EulerRedisCfg
  deriving stock (Generic, Typeable, Show, Eq)
  deriving anyclass (ToJSON, FromJSON)

instance T.OptionEntity EulerRedisCfg ConnectionTimeOutMetricHandle

---------------------------------------------------------

isDBMetricEnabled :: Bool
isDBMetricEnabled = fromMaybe False $ readMaybe =<< Conf.lookupEnvT "DB_METRIC_ENABLED"