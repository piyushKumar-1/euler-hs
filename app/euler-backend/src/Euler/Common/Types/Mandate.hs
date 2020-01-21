{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE UndecidableInstances #-}

module Euler.Common.Types.Mandate where

import EulerHS.Prelude

import Database.Beam.Backend.SQL
import Database.Beam.Postgres
import Database.Beam.Sqlite
import Database.Beam.MySQL
import qualified Data.Text as T

-- from src/Types/Storage/EC/Mandate/Types.purs
data PaymentMethodType = WALLET | UPI | NB | CARD | PAYLATER | CONSUMER_FINANCE | REWARD | CASH | UNKNOWN -- Foreign
  deriving (Show, Read, Eq, Ord, Generic, ToJSON, FromJSON)

instance HasSqlValueSyntax be String => HasSqlValueSyntax be PaymentMethodType where
  sqlValueSyntax = autoSqlValueSyntax

instance FromBackendRow Postgres PaymentMethodType where
  fromBackendRow = read . T.unpack <$> fromBackendRow

instance FromBackendRow Sqlite PaymentMethodType where
  fromBackendRow = read . T.unpack <$> fromBackendRow

instance FromBackendRow MySQL PaymentMethodType where
  fromBackendRow = read . T.unpack <$> fromBackendRow

data MandateStatus = CREATED | ACTIVE | PAUSED | REVOKED | FAILURE | PENDING
  deriving (Show, Read, Eq, Ord, Generic, ToJSON, FromJSON)


instance HasSqlValueSyntax be String => HasSqlValueSyntax be MandateStatus where
  sqlValueSyntax = autoSqlValueSyntax

instance FromBackendRow Postgres MandateStatus where
  fromBackendRow = read . T.unpack <$> fromBackendRow

instance FromBackendRow Sqlite MandateStatus where
  fromBackendRow = read . T.unpack <$> fromBackendRow

instance FromBackendRow MySQL MandateStatus where
  fromBackendRow = read . T.unpack <$> fromBackendRow