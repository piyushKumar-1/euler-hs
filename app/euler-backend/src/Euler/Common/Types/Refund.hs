{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE UndecidableInstances #-}

module Euler.Common.Types.Refund where

import EulerHS.Prelude

import Database.Beam.Backend.SQL
import Database.Beam.Postgres
import Database.Beam.Sqlite
import Database.Beam.MySQL

import qualified Data.Text as T


data RefundStatus = FAILURE | MANUAL_REVIEW | PENDING | SUCCESS | TXN_FAILURE
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Generic, ToJSON, FromJSON)

instance HasSqlValueSyntax be String => HasSqlValueSyntax be RefundStatus where
  sqlValueSyntax = autoSqlValueSyntax

instance FromBackendRow Postgres RefundStatus where
  fromBackendRow = read . T.unpack <$> fromBackendRow

instance FromBackendRow Sqlite RefundStatus where
  fromBackendRow = read . T.unpack <$> fromBackendRow

instance FromBackendRow MySQL RefundStatus where
  fromBackendRow = read . T.unpack <$> fromBackendRow