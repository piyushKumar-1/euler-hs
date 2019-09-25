{-# LANGUAGE DeriveAnyClass #-}

module EulerHS.Core.Types.DB where

import EulerHS.Prelude

import qualified Database.SQLite.Simple as SQLite
import qualified Database.Beam.Postgres as BP

data MockedSqlConn  = MockedSqlConn String
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

data MockedKVDBConn = MockedKVDBConn String
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

data SqlConn
  = MockedSql MockedSqlConn
  | SQLiteConn SQLite.Connection
  | PostgresConn BP.Connection


data KVDBConn
  = MockedKVDB MockedKVDBConn
  -- | Redis SimpleConn

type DBName = String

data DBConfig
  = SQLiteConfig DBName
  | PostgresConfig BP.ConnectInfo
  deriving (Show, Eq, Generic)
  -- Ord, ToJSON, FromJSON)
  -- no such instances for BP.ConnectInfo

-- TODO: more informative typed error.
data DBErrorType
  = ConnectionFailed
  | SomeError
  deriving (Show, Eq, Ord, Enum, Bounded, Generic, ToJSON, FromJSON)

data DBError
  = DBError DBErrorType Text
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

type DBResult a = Either DBError a
