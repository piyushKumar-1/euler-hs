{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module EulerHS.Core.Types.DB where

import EulerHS.Prelude

import qualified Database.SQLite.Simple as SQLite
import qualified Database.Beam.Sqlite as BS
-- import qualified Database.Beam.Postgres as BP

data MockedSqlConn  = MockedSqlConn String
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

data MockedKVDBConn = MockedKVDBConn String
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

type DbBackend  = BS.Sqlite
type DbBackendM = BS.SqliteM

data SqlConn
  = MockedSql MockedSqlConn
  | SQLiteConn SQLite.Connection
  -- | PostgresConn BP.Connection


data KVDBConn
  = MockedKVDB MockedKVDBConn
  -- | Redis SimpleConn

type DBName = String

data DBConfig
  = SQLiteConfig DBName
  -- | PostgresConfig ConnectInfo
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

-- TODO: more informative typed error.
data DBErrorType
  = ConnectionFailed
  | SomeError
  deriving (Show, Eq, Ord, Enum, Bounded, Generic, ToJSON, FromJSON)

data DBError
  = DBError DBErrorType Text
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

type DBResult a = Either DBError a

-- data ConnectInfo
--   = ConnectInfo
--     { connectHost :: String
--     , connectPort :: Word16
--     , connectUser :: String
--     , connectPassword :: String
--     , connectDatabase :: String
--     } deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

-- toBeamPostgresConnectInfo :: ConnectInfo -> BP.ConnectInfo
-- toBeamPostgresConnectInfo (ConnectInfo {..}) = BP.ConnectInfo {..}

--- OR

-- newtype ConnectInfo' a
--   = ConnectInfo' a
--   deriving (Show, Eq, Ord, Generic)

-- unpackCI :: ConnectInfo' a -> a
-- unpackCI (ConnectInfo' a) = a

-- packCI :: a -> ConnectInfo' a
-- packCI a = ConnectInfo' a

-- mapCI :: (a -> b) -> ConnectInfo' a -> ConnectInfo' b
-- mapCI f = packCI . f . unpackCI

-- type ConnectInfo = ConnectInfo' BP.ConnectInfo

-- instance ToJSON ConnectInfo where
--   toJSON = outer . mapCI inner
--     where
--       outer :: ConnectInfo' A.Value -> A.Value
--       outer = A.genericToJSON A.defaultOptions

--       inner :: BP.ConnectInfo -> A.Value
--       inner = A.genericToJSON A.defaultOptions

-- instance FromJSON ConnectInfo where
--   parseJSON = fmap packCI . inner . unpackCI <=< outer
--     where
--       outer :: A.Value -> A.Parser (ConnectInfo' A.Value)
--       outer = A.genericParseJSON A.defaultOptions

--       inner :: A.Value -> A.Parser BP.ConnectInfo
--       inner = A.genericParseJSON A.defaultOptions
