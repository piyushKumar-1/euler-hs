{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RecordWildCards            #-}

module EulerHS.Core.Types.DB
  (
    -- * Core DB
    -- ** Types
    BeamRuntime(..)
  , BeamRunner(..)
  , NativeSqlConn(..)
  , ConnTag
  , SQliteDBname
  , SqlConn(..)
  , DBConfig
  , PoolConfig(..)
  , DBErrorType(..)
  , DBError(..)
  , DBResult
  -- ** Methods
  , bemToNative
  , mkSqlConn
  , mkSQLiteConfig
  , mkSQLitePoolConfig
  , mkPostgresConfig
  , mkPostgresPoolConfig
  , mkMySQLConfig
  , mkMySQLPoolConfig
  -- ** Helpers
  , nativeToBem
  ) where

import           EulerHS.Prelude

import qualified Data.Pool                       as DP
import           Data.Time.Clock                 (NominalDiffTime)
import qualified Database.Beam                   as B
import qualified Database.Beam.Backend.SQL       as B
import qualified Database.Beam.MySQL             as BM
import qualified Database.Beam.Postgres          as BP
import qualified Database.Beam.Sqlite            as BS
import qualified Database.Beam.Sqlite.Connection as SQLite
import qualified Database.MySQL.Base             as MySQL
import qualified Database.PostgreSQL.Simple      as PGS
import qualified Database.SQLite.Simple          as SQLiteS
import qualified Database.SQLite.Simple          as SQLite

import           EulerHS.Core.Types.MySQL        (MySQLConfig, createMySQLConn)
import           EulerHS.Core.Types.Postgres     (PostgresConfig,
                                                  createPostgresConn)



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


instance BeamRunner BS.SqliteM where
  getBeamDebugRunner (SQLitePool _ pool) beM = \logger -> DP.withResource pool
    $ \connection -> do
        let begin    = beginTransactionSQLite      logger connection
        let commit   = commitTransactionSQLite     logger connection
        let rollback = rollbackTransactionSQLite   logger connection
        bracketOnError begin (const rollback) $ const $ do
          res <- SQLite.runBeamSqliteDebug logger connection beM
          commit
          return res

  getBeamDebugRunner _ _ = \_ -> error "Not a SQLite connection"

executeWithLogSQLite :: (String -> IO ()) -> SQLiteS.Connection -> SQLiteS.Query -> IO ()
executeWithLogSQLite log conn q = SQLiteS.execute_ conn q >> (log $ show q)

beginTransactionSQLite :: (String -> IO ()) -> SQLiteS.Connection -> IO ()
beginTransactionSQLite    log conn = do
  executeWithLogSQLite log conn "PRAGMA busy_timeout = 60000"
  executeWithLogSQLite log conn "BEGIN TRANSACTION"

commitTransactionSQLite :: (String -> IO ()) -> SQLiteS.Connection -> IO ()
commitTransactionSQLite   log conn = executeWithLogSQLite log conn "COMMIT TRANSACTION"

rollbackTransactionSQLite :: (String -> IO ()) -> SQLiteS.Connection -> IO ()
rollbackTransactionSQLite log conn = executeWithLogSQLite log conn "ROLLBACK TRANSACTION"


instance BeamRunner BP.Pg where
  getBeamDebugRunner (PostgresPool _ pool) beM = \logger -> DP.withResource pool
    $ \connection -> do
        let begin    = PGS.begin connection
        let commit   = PGS.commit connection
        let rollback = PGS.rollback connection
        bracketOnError begin (const rollback) $ const $ do
          res <- BP.runBeamPostgresDebug logger connection beM
          commit
          return res

  getBeamDebugRunner _ _ = \_ -> error "Not a Postgres connection"

instance BeamRunner BM.MySQLM where
  getBeamDebugRunner (MySQLPool _ pool) beM = \logger -> DP.withResource pool
    $ \connection -> do
        let begin    = pure ()
        let commit   = MySQL.commit connection
        let rollback = MySQL.rollback connection
        bracketOnError begin (const rollback) $ const $ do
          res <- BM.runBeamMySQLDebug logger connection beM
          commit
          return res

  getBeamDebugRunner _ _ = \_ -> error "Not a MySQL connection"

-- | Representation of native DB connections that we store in FlowRuntime
data NativeSqlConn
  = NativePGPool (DP.Pool BP.Connection)         -- ^ 'Pool' with Postgres connections
  | NativeMySQLPool (DP.Pool MySQL.Connection)   -- ^ 'Pool' with MySQL connections
  | NativeSQLitePool (DP.Pool SQLite.Connection) -- ^ 'Pool' with SQLite connections
  | NativeMockedConn

-- | Transform 'SqlConn' to 'NativeSqlConn'
bemToNative :: SqlConn beM -> NativeSqlConn
bemToNative (MockedConn _)        = NativeMockedConn
bemToNative (PostgresPool _ conn) = NativePGPool conn
bemToNative (MySQLPool _ conn)    = NativeMySQLPool conn
bemToNative (SQLitePool _ conn)   = NativeSQLitePool conn

-- | Create 'SqlConn' from 'DBConfig'
mkSqlConn :: DBConfig beM -> IO (SqlConn beM)
mkSqlConn (PostgresPoolConf connTag cfg PoolConfig {..}) =  PostgresPool connTag
  <$> DP.createPool (createPostgresConn cfg) BP.close stripes keepAlive resourcesPerStripe

mkSqlConn (MySQLPoolConf connTag cfg PoolConfig {..}) =  MySQLPool connTag
  <$> DP.createPool (createMySQLConn cfg) MySQL.close stripes keepAlive resourcesPerStripe

mkSqlConn (SQLitePoolConf connTag dbname PoolConfig {..}) =  SQLitePool connTag
  <$> DP.createPool (SQLite.open dbname) SQLite.close stripes keepAlive resourcesPerStripe

mkSqlConn (MockConfig connTag) = pure $ MockedConn connTag


-- | Tag for SQL connections
type ConnTag = Text

-- | Represents path to the SQLite DB
type SQliteDBname = String

-- | Represents SQL connection that we use in flow.
--   Parametrised by BEAM monad corresponding to the certain DB (MySQL, Postgres, SQLite)
data SqlConn beM
  = MockedConn ConnTag
  | PostgresPool ConnTag (DP.Pool BP.Connection)
  -- ^ 'Pool' with Postgres connections
  | MySQLPool ConnTag (DP.Pool MySQL.Connection)
  -- ^ 'Pool' with MySQL connections
  | SQLitePool ConnTag (DP.Pool SQLite.Connection)
  -- ^ 'Pool' with SQLite connections
  deriving (Generic)


-- | Represents DB configurations
data DBConfig beM
  = MockConfig ConnTag
  | PostgresPoolConf ConnTag PostgresConfig PoolConfig
  -- ^ config for 'Pool' with Postgres connections
  | MySQLPoolConf ConnTag MySQLConfig PoolConfig
  -- ^ config for 'Pool' with MySQL connections
  | SQLitePoolConf ConnTag SQliteDBname PoolConfig
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

-- | Represents 'Pool' parameters
data PoolConfig = PoolConfig
  { stripes            :: Int
  -- ^ a number of sub-pools
  , keepAlive          :: NominalDiffTime
  -- ^ the amount of time the connection will be stored
  , resourcesPerStripe :: Int
  -- ^ maximum number of connections to be stored in each sub-pool
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

defaultPoolConfig :: PoolConfig
defaultPoolConfig = PoolConfig
  { stripes = 1
  , keepAlive = 100
  , resourcesPerStripe = 1
  }

-- | Create SQLite 'DBConfig'
mkSQLiteConfig :: ConnTag -> SQliteDBname -> DBConfig BS.SqliteM
mkSQLiteConfig connTag dbName = SQLitePoolConf connTag dbName defaultPoolConfig

-- | Create SQLite 'Pool' 'DBConfig'
mkSQLitePoolConfig :: ConnTag -> SQliteDBname -> PoolConfig -> DBConfig BS.SqliteM
mkSQLitePoolConfig = SQLitePoolConf

-- | Create Postgres 'DBConfig'
mkPostgresConfig :: ConnTag -> PostgresConfig -> DBConfig BP.Pg
mkPostgresConfig connTag dbName = PostgresPoolConf connTag dbName defaultPoolConfig

-- | Create Postgres 'Pool' 'DBConfig'
mkPostgresPoolConfig :: ConnTag -> PostgresConfig -> PoolConfig -> DBConfig BP.Pg
mkPostgresPoolConfig = PostgresPoolConf

-- | Create MySQL 'DBConfig'
mkMySQLConfig :: ConnTag -> MySQLConfig -> DBConfig BM.MySQLM
mkMySQLConfig connTag dbName = MySQLPoolConf connTag dbName defaultPoolConfig

-- | Create MySQL 'Pool' 'DBConfig'
mkMySQLPoolConfig :: ConnTag -> MySQLConfig -> PoolConfig -> DBConfig BM.MySQLM
mkMySQLPoolConfig = MySQLPoolConf



-- TODO: more informative typed error.
-- | Represents failures that may occur while working with the database
data DBErrorType
  = ConnectionFailed
  | ConnectionAlreadyExists
  | ConnectionDoesNotExist
  | SomeError
  deriving (Show, Eq, Ord, Enum, Bounded, Generic, ToJSON, FromJSON)

-- | Represents DB error
data DBError
  = DBError DBErrorType Text
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

-- | Represents resulting type for DB actions
type DBResult a = Either DBError a


-- | Transforms 'NativeSqlConn' to 'SqlConn'
nativeToBem :: ConnTag -> NativeSqlConn -> SqlConn beM
nativeToBem connTag NativeMockedConn        = MockedConn connTag
nativeToBem connTag (NativePGPool conn)     = PostgresPool connTag conn
nativeToBem connTag (NativeMySQLPool conn)  = MySQLPool connTag conn
nativeToBem connTag (NativeSQLitePool conn) = SQLitePool connTag conn
