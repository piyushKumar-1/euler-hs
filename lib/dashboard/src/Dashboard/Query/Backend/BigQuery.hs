{-# LANGUAGE OverloadedStrings #-}

module Dashboard.Query.Backend.BigQuery where

import Universum hiding ((^.))

import Control.Lens ((?~), (^.))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Utils as Aeson
import Data.ByteString.Lazy.Char8 as B hiding (zip)
import Data.Maybe (fromJust)
import qualified Data.Text as T
import Data.Time.Clock
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Network.Google (Env, LogLevel(..), getApplicationDefault, newEnvWith, newLogger, newManager, runGoogle, runResourceT, send, tlsManagerSettings)
import Network.Google.Auth.ApplicationDefault (fromJSONCredentials)
import Network.Google.BigQuery (QueryResponse)
import qualified Network.Google.BigQuery.Types as BQT
import Network.Google.Resource.BigQuery.Jobs.Query (jobsQuery)

import Dashboard.Query.Backend.BigQuery.SQL (printSQL)
import Dashboard.Query.Backend (QueryBackend, runQuery)
import Dashboard.Metrics.Prometheus
import qualified Dashboard.Query.Config as QT
import qualified Dashboard.Query.Types as QT
import qualified Prometheus as P

data BigQueryBackend = BigQueryBackend { project :: Text
                                       , env     :: Env '["https://www.googleapis.com/auth/bigquery"]
                                       }

data BackendConfig = forall qb. QueryBackend qb =>
  BackendConfig
    { qbe  :: qb
    , qcfg :: QT.QueryConfiguration
    }

newBigQueryBackend :: Text -> Maybe Text -> IO BigQueryBackend
newBigQueryBackend project jsonCreds = do
  manager <- newManager tlsManagerSettings
  creds   <- case readCreds jsonCreds of
                  Just c  -> return c
                  Nothing -> getApplicationDefault manager
  logger  <- newLogger Debug stderr
  env     <- newEnvWith creds logger manager

  return $ BigQueryBackend project env

  where
    readCreds j = rightToMaybe . fromJSONCredentials . B.fromStrict . encodeUtf8 =<< j

instance QueryBackend BigQueryBackend where
  runQuery (BigQueryBackend project env) queryConf query = do
    let (sql, qp) = printSQL dateIsString query
    let bqRequest = BQT.queryRequest
                      & (BQT.qrQuery ?~ sql)
                      . (BQT.qrUseLegacySQL .~ False)
                      . (BQT.qrParameterMode ?~ "POSITIONAL")
                      . (BQT.qrQueryParameters .~ qp)
    let job       = jobsQuery bqRequest project
    response     <- P.observeDuration bqResponseTime $ runResourceT . runGoogle env . send $ job
    let (qr, qs)  = toQueryResult queryConf query response

    _ <- P.observe queryBytesProcessed $ fromIntegral $ QT.totalBytesProcessed qs
    _ <- P.addCounter totalBytesProcessed $ fromIntegral $ QT.totalBytesProcessed qs
    _ <- P.setGauge queryTotalRows $ fromIntegral $ QT.totalRows qs
    return qr

    where
      dateIsString =
        let tc        = fromJust $ QT.lookupTable (QT.table query) queryConf
            dateField = QT.field . QT.interval $ query
            fieldType = fromJust $ QT.lookupField dateField tc
        in fieldType == QT.StringType

toQueryResult :: QT.QueryConfiguration -> QT.Query -> QueryResponse -> (QT.QueryResult, QT.QueryStats)
toQueryResult queryConf query queryResponse = (QT.QueryResult $ rowToResult <$> rowValues, queryStats)
  where
    -- FIXME: Write as idiomatic lens code
    rows            = queryResponse ^. BQT.qRows
    rowCells        = fmap (^. BQT.trF) rows
    rowValues       = fmap (^. BQT.tcV) <$> rowCells
    tBytesProcessed = fromJust $ queryResponse ^. BQT.qTotalBytesProcessed
    totalRows       = fromJust $ queryResponse ^. BQT.qTotalRows
    queryStats      = QT.QueryStats tBytesProcessed totalRows

    rowToResult r =
      case r of
           (t:vs) ->
             -- Interval is 'start + delta' if there is a step, else just 'end'
             -- from the query
             let startUtc  = parseTimestamp . fromJust $ t
                 start     = QT.Timestamp startUtc
                 step      = fmap ((`quot` 1000) . QT.unMs) . QT.step . QT.interval $ query
                 end       = case step of
                                  Just step' -> QT.Timestamp $ fromIntegral step' `addUTCTime` startUtc
                                  Nothing    -> QT.stop . QT.interval $ query
                 -- The table name is validated, so this is okay
                 tableConf = fromJust $ QT.lookupTable (QT.table query) queryConf
                 types     = queryTypes tableConf (QT.selection query)
             in
               QT.QueryResultRow start end (jsonToQValue <$> types `zip` vs)

           _      -> error "Got a row without values"

    -- FIXME:
    -- This does mean that 'SELECT * ...' queries will error out, for now. This
    -- should be okay for now. We can relax this restriction by using the table
    -- configuration, _if_ we are certain that the order of columns in the
    -- TableConfiguration is the same as what the database returns.
    queryTypes tc (QT.Selection s) = queryType tc <$> s
    queryType _ (Just QT.Count, _)       = QT.IntType
    queryType _ (Just QT.Average, _)     = QT.FloatType
    -- Sum will be IntType or FloatType depending on the input field
    queryType tc (_, QT.Field fieldName) = fromJust $ QT.lookupField fieldName tc
    queryType _ (_, QT.All)              = error "Cannot provide values on 'All'"

    parseTimestamp (Aeson.String s) =
      maybe (error $ "Could not parse timestamp: " <> s)
            posixSecondsToUTCTime
            (Aeson.decodeV . encodeUtf8 $ s)
    parseTimestamp v = error $ "Invalid timestamp value: " <> show v

    -- Deal with nulls in the db
    jsonToQValue (fieldType, Nothing) =
      case fieldType of
           QT.IntType      -> QT.IntValue 0
           QT.FloatType    -> QT.FloatValue 0.0
           QT.StringType   -> QT.StringValue "Unknown"
           QT.DateTimeType -> error "Unexpected date/time null"
    -- BigQuery returns all actual values as strings in JSON
    jsonToQValue (fieldType, Just (Aeson.String s)) =
      fromMaybe (error $ "Could not parse value: " <> s) v
      where
        s' = encodeUtf8 s
        v  = case fieldType of
                 QT.IntType      -> QT.IntValue <$> Aeson.decodeV s'
                 QT.FloatType    -> QT.FloatValue <$> Aeson.decodeV s'
                 QT.StringType   -> Just . QT.StringValue . T.unpack $ s
                 QT.DateTimeType -> error "Unexpected date/time value"
    -- We don't expect to get here
    jsonToQValue v = error $ "Invalid value: " <> show v
