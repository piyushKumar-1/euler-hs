{-# LANGUAGE DeriveAnyClass #-}

module EulerHS.Core.Types.DB where

import EulerHS.Prelude

import qualified Database.SQLite.Simple as SQLite

data MockedSqlConn  = MockedSqlConn String
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

data MockedKVDBConn = MockedKVDBConn String
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

data SqlConn
  = MockedSql MockedSqlConn
  | SQLiteConn SQLite.Connection
  | PostgresConn


data KVDBConn
  = MockedKVDB MockedKVDBConn
  -- | Redis SimpleConn


type DBName = String

data DBConfig
  = SQLiteConfig DBName
  | PostgresConfig
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

-- TODO: more informative typed error.
data DBErrorType
  = ConnectionFailed
  deriving (Show, Eq, Ord, Enum, Bounded, Generic, ToJSON, FromJSON)

data DBError = DBError DBErrorType Text
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

type DBResult a = Either DBError a
