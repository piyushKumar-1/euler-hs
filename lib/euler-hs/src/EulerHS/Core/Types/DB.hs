{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module EulerHS.Core.Types.DB where

import EulerHS.Prelude

import qualified Database.SQLite.Simple as SQLite
import qualified Database.Beam.Sqlite as BS
import qualified Database.Beam as B
import qualified Database.Beam.Backend.SQL as B
import qualified Database.Beam.Sqlite.Connection as SQLite
import qualified Database.Beam.Postgres as BP

-- data MockedSqlConn  = MockedSqlConn String
--   deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

data MockedKVDBConn = MockedKVDBConn String
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

class (B.BeamSqlBackend be, B.MonadBeam be beM) => BeamRuntime be beM
  | be -> beM, beM -> be where
  rtSelectReturningList :: B.FromBackendRow be a => B.SqlSelect be a -> beM [a]
  rtSelectReturningOne :: B.FromBackendRow be a => B.SqlSelect be a -> beM (Maybe a)
  rtInsert :: B.SqlInsert be table -> beM ()
  rtUpdate :: B.SqlUpdate be table -> beM ()
  rtDelete :: B.SqlDelete be table -> beM ()

-- TODO: move somewhere (it's implementation)
instance BeamRuntime BS.Sqlite BS.SqliteM where
  rtSelectReturningList = B.runSelectReturningList
  rtSelectReturningOne = B.runSelectReturningOne
  rtInsert = B.runInsert
  rtUpdate = B.runUpdate
  rtDelete = B.runDelete

-- TODO: move somewhere (it's implementation)
instance BeamRuntime BP.Postgres BP.Pg where
  rtSelectReturningList = B.runSelectReturningList
  rtSelectReturningOne = B.runSelectReturningOne
  rtInsert = B.runInsert
  rtUpdate = B.runUpdate
  rtDelete = B.runDelete

class BeamRunner beM where
  getBeamDebugRunner :: SqlConn beM -> beM a -> ((String -> IO ()) -> IO a)

-- TODO: move somewhere (it's implementation)
instance BeamRunner BS.SqliteM where
  getBeamDebugRunner (SQLiteConn conn) beM = \logger -> SQLite.runBeamSqliteDebug logger conn beM
  getBeamDebugRunner _ _ = \_ -> error "Invalid connection"

-- TODO: move somewhere (it's implementation)
instance BeamRunner BP.Pg where
  getBeamDebugRunner (PostgresConn conn) beM = \logger -> BP.runBeamPostgresDebug logger conn beM
  getBeamDebugRunner _ _ = \_ -> error "Invalid connection"



data SqlConn beM
  = MockedConn
  | SQLiteConn SQLite.Connection
  | PostgresConn BP.Connection

data DBConfig beM
  = SQLiteConfig DBName
  | MockConfig
  | PostgresConf PostgresConfig
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

mkSQLiteConfig :: DBName -> DBConfig BS.SqliteM
mkSQLiteConfig = SQLiteConfig

mkPostgresConfig :: PostgresConfig -> DBConfig BP.Pg
mkPostgresConfig = PostgresConf

data KVDBConn
  = MockedKVDB MockedKVDBConn
  -- | Redis SimpleConn

type DBName = String

-- TODO: more informative typed error.
data DBErrorType
  = ConnectionFailed
  | SomeError
  deriving (Show, Eq, Ord, Enum, Bounded, Generic, ToJSON, FromJSON)

data DBError
  = DBError DBErrorType Text
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

type DBResult a = Either DBError a

data PostgresConfig = PostgresConfig
  { connectHost :: String
  , connectPort :: Word16
  , connectUser :: String
  , connectPassword :: String
  , connectDatabase :: String
  } deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

toBeamPostgresConnectInfo :: PostgresConfig -> BP.ConnectInfo
toBeamPostgresConnectInfo (PostgresConfig {..}) = BP.ConnectInfo {..}
