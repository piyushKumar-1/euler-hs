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


txnStatusToInt :: TxnStatus -> Int
txnStatusToInt AUTHENTICATION_FAILED = 26
txnStatusToInt AUTHORIZATION_FAILED = 27
txnStatusToInt AUTHORIZING = 28
txnStatusToInt CHARGED = 21
txnStatusToInt JUSPAY_DECLINED = 22
txnStatusToInt PENDING_VBV = 23
txnStatusToInt STARTED = 20
txnStatusToInt VBV_SUCCESSFUL = 24
txnStatusToInt AUTHORIZED = 25
txnStatusToInt COD_INITIATED = 29
txnStatusToInt VOIDED = 31
txnStatusToInt VOID_INITIATED = 32
txnStatusToInt NOP = -1
txnStatusToInt CAPTURE_INITIATED = 33
txnStatusToInt CAPTURE_FAILED = 34
txnStatusToInt VOID_FAILED = 35
-- txnStatusToInt NEW = 10
-- txnStatusToInt _ = -1

type TxnDetailId = Text
