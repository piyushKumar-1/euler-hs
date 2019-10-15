{-# LANGUAGE OverloadedStrings #-}

module Dashboard.Query.Backend.BigQuery where

import Universum hiding ((^.))

import Control.Lens ((?~), (^.))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Utils as Aeson
import Data.List (lookup)
import Data.Maybe (fromJust)
import qualified Data.Text as T
import Data.Time.Clock
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import qualified Network.Google.BigQuery.Types as BQT
import Network.Google.Resource.BigQuery.Jobs.Query (jobsQuery)
import Network.Google (Env, LogLevel(..), getApplicationDefault, newEnvWith, newLogger, newManager, runGoogle, runResourceT, send, tlsManagerSettings)
import Network.Google.BigQuery (QueryResponse)

import Dashboard.Query.Backend.BigQuery.SQL (printSQL)
import Dashboard.Query.Backend (QueryBackend, runQuery)
import qualified Dashboard.Query.Types as QT

data BigQueryBackend = BigQueryBackend { project :: Text
                                       , env     :: Env '["https://www.googleapis.com/auth/bigquery"]
                                       }

newBigQueryBackend :: Text -> IO BigQueryBackend
newBigQueryBackend project = do
  manager <- newManager tlsManagerSettings
  creds   <- getApplicationDefault manager
  logger  <- newLogger Debug stderr
  env     <- newEnvWith creds logger manager
  return $ BigQueryBackend project env

instance QueryBackend BigQueryBackend where
  runQuery (BigQueryBackend project env) queryConf query = do
    let sql       = printSQL dateIsString query
    let bqRequest = BQT.queryRequest
                      & (BQT.qrQuery ?~ sql)
                      . (BQT.qrUseLegacySQL .~ False)
    let job       = jobsQuery bqRequest project
    let response  = runResourceT . runGoogle env . send $ job
    toQueryResult queryConf query <$> response

    where
      dateIsString =
        let (QT.QueryConfiguration qc) = queryConf
            (QT.TableConfiguration tc) = fromJust $ lookup (QT.table query) qc
            dateField = QT.field . QT.interval $ query
            fieldType = fromJust $ lookup dateField tc
        in fieldType == QT.StringType

toQueryResult :: QT.QueryConfiguration -> QT.Query -> QueryResponse -> QT.QueryResult
toQueryResult queryConf query queryResponse = QT.QueryResult $ rowToResult <$> rowValues
  where
    -- FIXME: Write as idiomatic lens code
    rows      = queryResponse ^. BQT.qRows
    rowCells  = fmap (^. BQT.trF) rows
    rowValues = fmap (^. BQT.tcV) <$> rowCells

    rowToResult r =
      case r of
           (t:vs) ->
             -- Interval is 'start + delta' if there is a step, else just 'end'
             -- from the query
             let startUtc = parseTimestamp . fromJust $ t
                 start    = QT.Timestamp startUtc
                 step     = fmap ((`quot` 1000) . QT.unMs) . QT.step . QT.interval $ query
                 end      = case step of
                                 Just step' -> QT.Timestamp $ fromIntegral step' `addUTCTime` startUtc
                                 Nothing    -> QT.stop . QT.interval $ query
                 -- The table name is validated, so this is okay
                 (QT.QueryConfiguration qc) = queryConf
                 (QT.TableConfiguration tc) = fromJust $ lookup (QT.table query) qc
                 types    = fmap snd tc
             in
               QT.QueryResultRow start end (jsonToQValue <$> types `zip` vs)

           _      -> error "Got a row without values"

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
