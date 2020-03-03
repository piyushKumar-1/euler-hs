{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE UndecidableInstances  #-}

module Euler.Common.Types.External.Mandate where

import EulerHS.Prelude
import Data.Data (Data)

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
-- EHS: too generic names for domain data type
data MandateFeature
  = DISABLED
  | REQUIRED
  | OPTIONAL
  deriving (Show, Read, Eq, Ord, Generic, Data, Typeable, ToJSON, FromJSON, ToForm, FromForm)

instance ToHttpApiData MandateFeature where
  toQueryParam = T.pack . P.show                -- EHS: why not just `show`??

instance FromHttpApiData MandateFeature where
  parseQueryParam p = bimap T.pack id $ TR.readEither $ T.unpack p

instance HasSqlValueSyntax be String => HasSqlValueSyntax be MandateFeature where
  sqlValueSyntax = autoSqlValueSyntax

instance FromBackendRow Postgres MandateFeature where
  fromBackendRow = read . T.unpack <$> fromBackendRow

instance FromBackendRow Sqlite MandateFeature where
  fromBackendRow = read . T.unpack <$> fromBackendRow

instance FromBackendRow MySQL MandateFeature where
  fromBackendRow = read . T.unpack <$> fromBackendRow

-- EHS: Type for API and DB.
-- Temporary, split into separate types for API and DB
-- and move into the appropriate namespaces.


data MandateStatus
  = CREATED
  | ACTIVE
  | PAUSED
  | REVOKED
  | FAILURE
  | PENDING
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Generic, ToJSON, FromJSON, Data, Typeable)

instance HasSqlValueSyntax be String => HasSqlValueSyntax be MandateStatus where
  sqlValueSyntax = autoSqlValueSyntax

instance FromBackendRow Postgres MandateStatus where
  fromBackendRow = read . T.unpack <$> fromBackendRow

instance FromBackendRow Sqlite MandateStatus where
  fromBackendRow = read . T.unpack <$> fromBackendRow

instance FromBackendRow MySQL MandateStatus where
  fromBackendRow = read . T.unpack <$> fromBackendRow


data MandateType = MANDATE | EMANDATE
  deriving (Show, Read, Eq, Ord, Generic, Data, Typeable, ToJSON, FromJSON)

instance HasSqlValueSyntax be String => HasSqlValueSyntax be MandateType where
  sqlValueSyntax = autoSqlValueSyntax

instance FromBackendRow Postgres MandateType where
  fromBackendRow = read . T.unpack <$> fromBackendRow

instance FromBackendRow Sqlite MandateType where
  fromBackendRow = read . T.unpack <$> fromBackendRow

instance FromBackendRow MySQL MandateType where
  fromBackendRow = read . T.unpack <$> fromBackendRow
