{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE UndecidableInstances #-}

module Euler.Common.Types.EulerAccountDetails where

import EulerHS.Prelude

import Control.Exception (throw)
import           Data.Aeson (decodeStrict')
import           Data.Aeson.Text (encodeToLazyText)
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL

import Database.Beam.Backend.SQL.Row

import Database.Beam.Backend.SQL
import Database.Beam.Postgres
import Database.Beam.Sqlite
import Database.Beam.MySQL


data EulerAccountDetails = EulerAccountDetails
  { merchantId  :: Text
  , checksumKey :: Text
  }
  deriving (Generic, Eq, Ord, Show, Read, ToJSON, FromJSON)

err :: BeamRowReadError
err = BeamRowReadError
  { brreColumn = Nothing
  , brreError = ColumnErrorInternal "Can't parse EulerAccountDetails"
  }

instance HasSqlValueSyntax be Text => HasSqlValueSyntax be EulerAccountDetails where
  sqlValueSyntax = sqlValueSyntax . TL.toStrict . encodeToLazyText

instance FromBackendRow Postgres EulerAccountDetails where
  fromBackendRow = maybe (throw err) id . decodeStrict' . TE.encodeUtf8 <$> fromBackendRow

instance FromBackendRow Sqlite EulerAccountDetails where
  fromBackendRow = maybe (throw err) id . decodeStrict' . TE.encodeUtf8 <$> fromBackendRow

instance FromBackendRow MySQL EulerAccountDetails where
  fromBackendRow = maybe (throw err) id . decodeStrict' . TE.encodeUtf8 <$> fromBackendRow



