{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE UndecidableInstances #-}

module Euler.Common.Types.Transaction where

import EulerHS.Prelude

import Database.Beam.Backend.SQL
import Database.Beam.Postgres
import Database.Beam.Sqlite
import Database.Beam.MySQL

import qualified Data.Text as T


data AuthType
  = ATMPIN
  | THREE_DS
  | OTP
  | VISA_CHECKOUT
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Generic, ToJSON, FromJSON)

instance HasSqlValueSyntax be String => HasSqlValueSyntax be AuthType where
  sqlValueSyntax = autoSqlValueSyntax

instance FromBackendRow Postgres AuthType where
  fromBackendRow = read . T.unpack <$> fromBackendRow

instance FromBackendRow Sqlite AuthType where
  fromBackendRow = read . T.unpack <$> fromBackendRow

instance FromBackendRow MySQL AuthType where
  fromBackendRow = read . T.unpack <$> fromBackendRow
