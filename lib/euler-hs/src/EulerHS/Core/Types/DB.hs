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
  ) where

import           EulerHS.Prelude

import qualified Database.Beam                   as B
import qualified Database.Beam.Backend.SQL       as B
import qualified Database.Beam.MySQL             as BM
import qualified Database.Beam.Postgres          as BP
import qualified Database.Beam.Sqlite            as BS
import qualified Database.Beam.Sqlite.Connection as SQLite
import qualified Database.SQLite.Simple          as SQLiteS
import qualified Data.Pool                       as DP
import qualified Database.MySQL.Base             as MySQL
import qualified Database.SQLite.Simple          as SQLite
import qualified Database.PostgreSQL.Simple      as PGS
import           Data.Time.Clock                    (NominalDiffTime)

import           EulerHS.Core.Types.MySQL        (MySQLConfig, createMySQLConn)
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


instance BeamRunner BS.SqliteM where
  getBeamDebugRunner (SQLiteConn _ conn) beM = \logger -> do
    let begin    = beginTransactionSQLite      logger conn
    let commit   = commitTransactionSQLite     logger conn
    let rollback = rollbackTransactionSQLite   logger conn
    bracketOnError begin (const rollback) $ const $ do
      res <- SQLite.runBeamSqliteDebug logger conn beM
      commit
      return res


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
  getBeamDebugRunner (PostgresConn _ conn) beM = \logger -> do
    let begin    = PGS.begin conn
    let commit   = PGS.commit conn
    let rollback = PGS.rollback conn
    bracketOnError begin (const rollback) $ const $ do
      res <- BP.runBeamPostgresDebug logger conn beM
      commit
      return res

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
  getBeamDebugRunner (MySQLConn _ conn) beM = \logger -> do
    let begin    = pure ()              -- Seems no begin transaction in MySQL
    let commit   = MySQL.commit conn
    let rollback = MySQL.rollback conn
    bracketOnError begin (const rollback) $ const $ do
      res <- BM.runBeamMySQLDebug logger conn beM
      commit
      return res


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
  = NativePGConn BP.Connection                   -- ^ Postgres connection
  | NativeMySQLConn MySQL.Connection             -- ^ MySQL connection
  | NativeSQLiteConn SQLite.Connection           -- ^ SQLite connection
  | NativePGPool (DP.Pool BP.Connection)         -- ^ 'Pool' with Postgres connections
  | NativeMySQLPool (DP.Pool MySQL.Connection)   -- ^ 'Pool' with MySQL connections
  | NativeSQLitePool (DP.Pool SQLite.Connection) -- ^ 'Pool' with SQLite connections
  | NativeMockedConn

-- | Transform 'SqlConn' to 'NativeSqlConn'
bemToNative :: SqlConn beM -> NativeSqlConn
bemToNative (MockedConn _) = NativeMockedConn
bemToNative (SQLiteConn _ conn) = NativeSQLiteConn conn
bemToNative (PostgresConn _ conn) = NativePGConn conn
bemToNative (MySQLConn _ conn) = NativeMySQLConn conn
bemToNative (PostgresPool _ conn) = NativePGPool conn
bemToNative (MySQLPool _ conn) = NativeMySQLPool conn
bemToNative (SQLitePool _ conn) = NativeSQLitePool conn

-- | Create 'SqlConn' from 'DBConfig'
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


-- | Tag for SQL connections
type ConnTag = Text

-- | Represents path to the SQLite DB
type SQliteDBname = String

-- | Represents SQL connection that we use in flow.
--   Parametrised by BEAM monad corresponding to the certain DB (MySQL, Postgres, SQLite)
data SqlConn beM
  = MockedConn ConnTag
  | SQLiteConn ConnTag SQLite.Connection
  -- ^ SQLite connection
  | PostgresConn ConnTag BP.Connection
  -- ^ Postgres connection
  | MySQLConn ConnTag MySQL.Connection
  -- ^ MySQL connection
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
  | SQLiteConf ConnTag SQliteDBname
  -- ^ config for SQLite connection
  | PostgresConf ConnTag PostgresConfig
  -- ^ config for Postgres connection
  | PostgresPoolConf ConnTag PoolConfig PostgresConfig
  -- ^ config for 'Pool' with Postgres connections
  | MySQLConf ConnTag MySQLConfig
  -- ^ config for MySQL connection
  | MySQLPoolConf ConnTag PoolConfig MySQLConfig
  -- ^ config for 'Pool' with MySQL connections
  | SQLitePoolConf ConnTag PoolConfig SQliteDBname
  -- ^ config for 'Pool' with SQLite connections
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

-- | Represents 'Pool' parameters
data PoolConfig = PoolConfig
  { stripes :: Int
  -- ^ a number of sub-pools
  , keepAlive :: NominalDiffTime
  -- ^ the amount of time the connection will be stored
  , resourcesPerStripe :: Int
  -- ^ maximum number of connections to be stored in each sub-pool
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

-- | Create SQLite 'DBConfig'
mkSQLiteConfig :: ConnTag -> SQliteDBname -> DBConfig BS.SqliteM
mkSQLiteConfig = SQLiteConf

-- | Create SQLite 'Pool' 'DBConfig'
mkSQLitePoolConfig :: ConnTag -> PoolConfig -> SQliteDBname -> DBConfig BS.SqliteM
mkSQLitePoolConfig = SQLitePoolConf

-- | Create Postgres 'DBConfig'
mkPostgresConfig :: ConnTag -> PostgresConfig -> DBConfig BP.Pg
mkPostgresConfig = PostgresConf

-- | Create Postgres 'Pool' 'DBConfig'
mkPostgresPoolConfig :: ConnTag -> PoolConfig -> PostgresConfig -> DBConfig BP.Pg
mkPostgresPoolConfig = PostgresPoolConf

-- | Create MySQL 'DBConfig'
mkMySQLConfig :: ConnTag -> MySQLConfig -> DBConfig BM.MySQLM
mkMySQLConfig = MySQLConf

-- | Create MySQL 'Pool' 'DBConfig'
mkMySQLPoolConfig :: ConnTag -> PoolConfig -> MySQLConfig -> DBConfig BM.MySQLM
mkMySQLPoolConfig = MySQLPoolConf



-- TODO: more informative typed error.
-- | Represents failures that may occur while working with the database
data DBErrorType
  = ConnectionFailed
  | ConnectionAlreadyExists
  | SomeError
  deriving (Show, Eq, Ord, Enum, Bounded, Generic, ToJSON, FromJSON)

-- | Represents DB error
data DBError
  = DBError DBErrorType Text
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

-- | Represents resulting type for DB actions
type DBResult a = Either DBError a

