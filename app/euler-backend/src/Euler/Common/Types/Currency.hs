{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Euler.Common.Types.Currency where

import EulerHS.Prelude

import           Data.Data
import           Database.Beam.Backend.SQL
import           Database.Beam.Postgres
import           Database.Beam.Sqlite
import           Database.Beam.MySQL
import           Web.FormUrlEncoded
import           Web.Internal.HttpApiData

import qualified Data.Text as T (pack, unpack)
import qualified Prelude as P (show)
import qualified Text.Read as TR (readEither)

data Currency
  = INR
  | USD
  | GBP
  | EUR
  deriving (Data, Show, Read, Eq, Ord, Enum, Bounded, Generic, ToJSON, FromJSON)

instance ToHttpApiData Currency where
  toQueryParam = T.pack . P.show

instance FromHttpApiData Currency where
  parseQueryParam p = bimap T.pack id $ TR.readEither $ T.unpack p

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Currency where
  sqlValueSyntax = autoSqlValueSyntax

instance FromBackendRow Postgres Currency where
  fromBackendRow = read . T.unpack <$> fromBackendRow

instance FromBackendRow Sqlite Currency where
  fromBackendRow = read . T.unpack <$> fromBackendRow

instance FromBackendRow MySQL Currency where
  fromBackendRow = read . T.unpack <$> fromBackendRow