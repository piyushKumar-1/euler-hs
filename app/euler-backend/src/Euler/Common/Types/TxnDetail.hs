{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE UndecidableInstances #-}

module Euler.Common.Types.TxnDetail where

import EulerHS.Prelude

import Database.Beam.Backend.SQL
import Database.Beam.Postgres
import Database.Beam.Sqlite
import Database.Beam.MySQL

import qualified Data.Text as T



data TxnMode = TEST | PROD
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Generic, ToJSON, FromJSON)

data TxnType =  AUTH_AND_SETTLE | REFUND | CANCEL_AUTH | AUTH
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Generic, ToJSON, FromJSON)

data TxnStatus = STARTED | AUTHENTICATION_FAILED | JUSPAY_DECLINED | PENDING_VBV | VBV_SUCCESSFUL | AUTHORIZED | AUTHORIZATION_FAILED | CHARGED | AUTHORIZING | COD_INITIATED | VOIDED | VOID_INITIATED | NOP | CAPTURE_INITIATED | CAPTURE_FAILED | VOID_FAILED
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Generic, ToJSON, FromJSON)

instance HasSqlValueSyntax be String => HasSqlValueSyntax be TxnStatus where
  sqlValueSyntax = autoSqlValueSyntax

instance FromBackendRow Postgres TxnStatus where
  fromBackendRow = read . T.unpack <$> fromBackendRow

instance FromBackendRow Sqlite TxnStatus where
  fromBackendRow = read . T.unpack <$> fromBackendRow

instance FromBackendRow MySQL TxnStatus where
  fromBackendRow = read . T.unpack <$> fromBackendRow

data TxnObjectType = SUBSCRIPTION_REGISTER | SUBSCRIPTION_PAYMENT | ORDER_PAYMENT
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Generic, ToJSON, FromJSON)
