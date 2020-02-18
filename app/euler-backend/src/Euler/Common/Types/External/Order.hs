{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE UndecidableInstances #-}

module Euler.Common.Types.External.Order where

import EulerHS.Prelude

import Database.Beam.Backend.SQL
import Database.Beam.Postgres
import Database.Beam.Sqlite
import Database.Beam.MySQL
import Web.FormUrlEncoded
import Web.Internal.HttpApiData

import qualified Data.Text as T (pack, unpack)
import qualified Prelude as P (show)
import qualified Text.Read as TR (readEither)

-- EHS: Type for API and DB.
-- Temporary, split into separate types for API and DB
-- and move into the appropriate namespaces.
-- EHS: too generic names for domain data type (NEW, SUCCESS etc.)
-- Should be reworked.
data OrderStatus
  = NEW
  | SUCCESS
  | NOT_FOUND
  | ERROR
  | JUSPAY_DECLINED
  | PENDING_AUTHENTICATION
  | AUTHENTICATION_FAILED
  | AUTHORIZATION_FAILED
  | AUTHORIZING
  | AUTHORIZED
  | CREATED
  | COD_INITIATED
  | VOIDED
  | VOID_INITIATED
  deriving (Show, Read, Eq, Ord, Generic, ToJSON, FromJSON)

 -- :set -XUndecidableInstances

instance HasSqlValueSyntax be String => HasSqlValueSyntax be OrderStatus where
  sqlValueSyntax = autoSqlValueSyntax

instance FromBackendRow Postgres OrderStatus where
  fromBackendRow = read . T.unpack <$> fromBackendRow

instance FromBackendRow Sqlite OrderStatus where
  fromBackendRow = read . T.unpack <$> fromBackendRow

instance FromBackendRow MySQL OrderStatus where
  fromBackendRow = read . T.unpack <$> fromBackendRow


orderStatusToInt :: OrderStatus -> Int
orderStatusToInt CREATED = 1
orderStatusToInt NEW = 10
orderStatusToInt SUCCESS = 30
orderStatusToInt NOT_FOUND = 40
orderStatusToInt ERROR = -1
orderStatusToInt JUSPAY_DECLINED = 23
orderStatusToInt PENDING_AUTHENTICATION = 15
orderStatusToInt AUTHENTICATION_FAILED = 26
orderStatusToInt AUTHORIZATION_FAILED = 27
orderStatusToInt AUTHORIZING = 28
orderStatusToInt AUTHORIZED = 30
orderStatusToInt VOIDED = 31
orderStatusToInt VOID_INITIATED = 32
-- orderStatusToInt AUTHORIZATION_FAILURE = -1
orderStatusToInt COD_INITIATED = 29
