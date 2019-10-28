{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RecordWildCards            #-}

module EulerHS.Core.Types.DB where

import           EulerHS.Prelude

import qualified Database.Beam                   as B
import qualified Database.Beam.Backend.SQL       as B
import qualified Database.Beam.MySQL             as BM
import qualified Database.Beam.Postgres          as BP
import qualified Database.Beam.Sqlite            as BS
import qualified Database.Beam.Sqlite.Connection as SQLite
import qualified Data.Pool                       as DP
import qualified Database.MySQL.Base             as MySQL
import qualified Database.SQLite.Simple          as SQLite
import           Data.Time.Clock                    (NominalDiffTime)

import           EulerHS.Core.Types.MySQL        (MySQLConfig, MySQLPoolConfig, createMySQLConn)
import           EulerHS.Core.Types.Postgres     (PostgresConfig, createPostgresConn)



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

instance BeamRuntime BM.MySQL BM.MySQLM where
  rtSelectReturningList = B.runSelectReturningList
  rtSelectReturningOne = B.runSelectReturningOne
  rtInsert = B.runInsert
  rtUpdate = B.runUpdate
  rtDelete = B.runDelete

class BeamRunner beM where
  getBeamDebugRunner :: SqlConn beM -> beM a -> ((String -> IO ()) -> IO a)

-- TODO: move somewhere (it's implementation)
instance BeamRunner BS.SqliteM where
  getBeamDebugRunner (SQLiteConn _ conn) beM = \logger -> SQLite.runBeamSqliteDebug logger conn beM
  getBeamDebugRunner (SQLitePool _ pool) beM = \logger -> DP.withResource pool
    $ \connection -> SQLite.runBeamSqliteDebug logger connection beM
  getBeamDebugRunner _ _ = \_ -> error "Invalid connection"   -- TODO: more informative error

-- TODO: move somewhere (it's implementation)
instance BeamRunner BP.Pg where
  getBeamDebugRunner (PostgresConn _ conn) beM = \logger -> BP.runBeamPostgresDebug logger conn beM
  getBeamDebugRunner (PostgresPool _ pool) beM = \logger -> DP.withResource pool
    $ \connection -> BP.runBeamPostgresDebug logger connection beM
  getBeamDebugRunner _ _ = \_ -> error "Invalid connection"   -- TODO: more informative error

instance BeamRunner BM.MySQLM where
  getBeamDebugRunner (MySQLConn _ conn) beM = \logger -> BM.runBeamMySQLDebug logger conn beM
  getBeamDebugRunner (MySQLPool _ pool) beM = \logger -> DP.withResource pool
    $ \connection -> BM.runBeamMySQLDebug logger connection beM
  getBeamDebugRunner _ _ = \_ -> error "Invalid connection"   -- TODO: more informative error
-- ###

data NativeSqlConn
  = NativePGConn BP.Connection
  | NativeMySQLConn MySQL.Connection
  | NativeSQLiteConn SQLite.Connection
  | NativePGPool (DP.Pool BP.Connection)
  | NativeMySQLPool (DP.Pool MySQL.Connection)
  | NativeSQLitePool (DP.Pool SQLite.Connection)
  | NativeMockedConn

bemToNative :: SqlConn beM -> NativeSqlConn
bemToNative (MockedConn _) = NativeMockedConn
bemToNative (SQLiteConn _ conn) = NativeSQLiteConn conn
bemToNative (PostgresConn _ conn) = NativePGConn conn
bemToNative (MySQLConn _ conn) = NativeMySQLConn conn
bemToNative (PostgresPool _ conn) = NativePGPool conn
bemToNative (MySQLPool _ conn) = NativeMySQLPool conn
bemToNative (SQLitePool _ conn) = NativeSQLitePool conn


mkSqlConn :: DBConfig beM -> IO (SqlConn beM)
mkSqlConn (PostgresPoolConf connTag PoolConfig {..} cfg) =  PostgresPool connTag
  <$> DP.createPool (createPostgresConn cfg) BP.close stripes keepAlive resourcesPerStripe

mkSqlConn (MySQLPoolConf connTag PoolConfig {..} cfg) =  MySQLPool connTag
  <$> DP.createPool (createMySQLConn cfg) MySQL.close stripes keepAlive resourcesPerStripe

mkSqlConn (SQLitePoolConf connTag PoolConfig {..} dbname) =  SQLitePool connTag
  <$> DP.createPool (SQLite.open dbname) SQLite.close stripes keepAlive resourcesPerStripe

mkSqlConn (SQLiteConf connTag dbname) =  SQLiteConn connTag <$> SQLite.open dbname

mkSqlConn (PostgresConf connTag cfg) =  PostgresConn connTag <$> createPostgresConn cfg

mkSqlConn (MySQLConf connTag cfg) =  MySQLConn connTag <$> createMySQLConn cfg

mkSqlConn (MockConfig connTag) = pure $ MockedConn connTag



type ConnTag = Text

type SQliteDBname = String

data SqlConn beM
  = MockedConn ConnTag
  | SQLiteConn ConnTag SQLite.Connection
  | PostgresConn ConnTag BP.Connection
  | MySQLConn ConnTag MySQL.Connection
  | PostgresPool ConnTag (DP.Pool BP.Connection)
  | MySQLPool ConnTag (DP.Pool MySQL.Connection)
  | SQLitePool ConnTag (DP.Pool SQLite.Connection)
  deriving (Generic)

data DBConfig beM
  = MockConfig ConnTag
  | SQLiteConf ConnTag SQliteDBname
  | PostgresConf ConnTag PostgresConfig
  | PostgresPoolConf ConnTag PoolConfig PostgresConfig
  | MySQLConf ConnTag MySQLConfig
  | MySQLPoolConf ConnTag PoolConfig MySQLConfig
  | SQLitePoolConf ConnTag PoolConfig SQliteDBname
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)


data PoolConfig = PoolConfig
  { stripes :: Int
  , keepAlive :: NominalDiffTime
  , resourcesPerStripe :: Int
  } deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

mkSQLiteConfig :: ConnTag -> SQliteDBname -> DBConfig BS.SqliteM
mkSQLiteConfig = SQLiteConf

mkSQLitePoolConfig :: ConnTag -> PoolConfig -> SQliteDBname -> DBConfig BS.SqliteM
mkSQLitePoolConfig = SQLitePoolConf

mkPostgresConfig :: ConnTag -> PostgresConfig -> DBConfig BP.Pg
mkPostgresConfig = PostgresConf

mkPostgresPoolConfig :: ConnTag -> PoolConfig -> PostgresConfig -> DBConfig BP.Pg
mkPostgresPoolConfig = PostgresPoolConf

mkMySQLConfig :: ConnTag -> MySQLConfig -> DBConfig BM.MySQLM
mkMySQLConfig = MySQLConf

mkMySQLPoolConfig :: ConnTag -> PoolConfig -> MySQLConfig -> DBConfig BM.MySQLM
mkMySQLPoolConfig = MySQLPoolConf



-- TODO: more informative typed error.
data DBErrorType
  = ConnectionFailed
  | ConnectionAlreadyExists
  | SomeError
  deriving (Show, Eq, Ord, Enum, Bounded, Generic, ToJSON, FromJSON)

data DBError
  = DBError DBErrorType Text
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

type DBResult a = Either DBError a

