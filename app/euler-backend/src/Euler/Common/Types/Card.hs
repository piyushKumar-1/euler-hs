{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE UndecidableInstances #-}

module Euler.Common.Types.Card where

import EulerHS.Prelude

import Database.Beam.Backend.SQL
import Database.Beam.Postgres
import Database.Beam.Sqlite
import Database.Beam.MySQL

import qualified Data.Text as T


data CardType
  = CREDIT
  | DEBIT
  | PREPAID
  | NB
  | WALLET
  | PAYLATER
  | UPI
  | ATM_CARD
  | REWARD
  | CONSUMER_FINANCE
  | CASH
  | BLANK
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Generic, ToJSON, FromJSON)

instance HasSqlValueSyntax be String => HasSqlValueSyntax be CardType where
  sqlValueSyntax = autoSqlValueSyntax

instance FromBackendRow Postgres CardType where
  fromBackendRow = read . T.unpack <$> fromBackendRow

instance FromBackendRow Sqlite CardType where
  fromBackendRow = read . T.unpack <$> fromBackendRow

instance FromBackendRow MySQL CardType where
  fromBackendRow = read . T.unpack <$> fromBackendRow

