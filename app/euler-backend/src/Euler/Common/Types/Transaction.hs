{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE UndecidableInstances #-}

module Euler.Common.Types.Transaction where

import EulerHS.Prelude
import Data.Data (Data)

import Database.Beam.Backend.SQL
import Database.Beam.Postgres
import Database.Beam.Sqlite
import Database.Beam.MySQL

import qualified Data.Text as T


-- EHS: rework these types. They could be wrong (not checked for transaction)
data PaymentMethodType
  = WALLET  -- ^ Wallet
  | UPI     -- ^ UPI (UPI application)
  | NB      -- ^ Network Banking
  | CARD    -- ^ Credit / Debit Card
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Generic, ToJSON, FromJSON, Data, Typeable)

data TxnType
  = UPI_COLLECT
  | UPI_PAY
  | BHARAT_PAY
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Generic, ToJSON, FromJSON)

------------------

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
