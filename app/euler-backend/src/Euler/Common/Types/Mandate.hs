{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE UndecidableInstances  #-}

module Euler.Common.Types.Mandate where

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

import qualified Euler.Common.Types.Order as O

-- EHS: Type for API and DB.
-- Temporary, split into separate types for API and DB
-- and move into the appropriate namespaces.

data MandateFeature
  = DISABLED
  | REQUIRED
  | OPTIONAL
  deriving (Show, Read, Eq, Ord, Generic, ToJSON, FromJSON, ToForm, FromForm)

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

toDBMandate :: O.OrderMandate -> MandateFeature
toDBMandate O.MandateDisabled     = DISABLED
toDBMandate (O.MandateRequired _) = REQUIRED
toDBMandate (O.MandateOptional _) = OPTIONAL
