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
  , NativeSqlPool(..)
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
  , withTransaction
  ) where

import           EulerHS.Prelude

import qualified Data.Pool                                as DP
import           Data.Time.Clock                         (NominalDiffTime)
import qualified Database.Beam                            as B
import qualified Database.Beam.Backend.SQL.BeamExtensions as B
import qualified Database.Beam.Backend.SQL                as B
import qualified Database.Beam.MySQL                      as BM
import qualified Database.Beam.Postgres                   as BP
import qualified Database.Beam.Sqlite                     as BS
import qualified Database.Beam.Sqlite.Connection          as SQLite
import qualified Database.MySQL.Base                      as MySQL
import qualified Database.PostgreSQL.Simple               as PGS
import qualified Database.SQLite.Simple                   as SQLite

import           EulerHS.Core.Types.MySQL        (MySQLConfig, createMySQLConn)
import           EulerHS.Core.Types.Postgres     (PostgresConfig,
                                                  createPostgresConn)



class (B.BeamSqlBackend be, B.MonadBeam be beM) => BeamRuntime be beM
  | be -> beM, beM -> be where
  rtSelectReturningList :: B.FromBackendRow be a => B.SqlSelect be a -> beM [a]
  rtSelectReturningOne  :: B.FromBackendRow be a => B.SqlSelect be a -> beM (Maybe a)
  rtInsert              :: B.SqlInsert be table -> beM ()
  rtInsertReturningList :: forall table . (B.Beamable table, B.FromBackendRow be (table Identity)) => B.SqlInsert be table -> beM [table Identity]
  rtUpdate              :: B.SqlUpdate be table -> beM ()
  rtDelete              :: B.SqlDelete be table -> beM ()

-- TODO: move somewhere (it's implementation)
instance BeamRuntime BS.Sqlite BS.SqliteM where
  rtSelectReturningList = B.runSelectReturningList
  rtSelectReturningOne = B.runSelectReturningOne
  rtInsert = B.runInsert
  rtInsertReturningList = B.runInsertReturningList
  rtUpdate = B.runUpdate
  rtDelete = B.runDelete

-- TODO: move somewhere (it's implementation)
instance BeamRuntime BP.Postgres BP.Pg where
  rtSelectReturningList = B.runSelectReturningList
  rtSelectReturningOne = B.runSelectReturningOne
  rtInsert = B.runInsert
  rtInsertReturningList = B.runInsertReturningList
  rtUpdate = B.runUpdate
  rtDelete = B.runDelete

instance BeamRuntime BM.MySQL BM.MySQLM where
  rtSelectReturningList = B.runSelectReturningList
  rtSelectReturningOne = B.runSelectReturningOne
  rtInsert = B.runInsert
  rtInsertReturningList = B.runInsertReturningList
  rtUpdate = B.runUpdate
  rtDelete = B.runDelete

class BeamRunner beM where
  getBeamDebugRunner :: NativeSqlConn -> beM a -> ((String -> IO ()) -> IO a)


instance BeamRunner BS.SqliteM where
  getBeamDebugRunner (NativeSQLiteConn conn) beM =
    \logger -> SQLite.runBeamSqliteDebug logger conn beM
  getBeamDebugRunner _ _ = \_ -> error "Not a SQLite connection"

beginTransactionSQLite :: SQLite.Connection -> IO ()
beginTransactionSQLite conn = do
  SQLite.execute_ conn "PRAGMA busy_timeout = 60000"
  SQLite.execute_ conn "BEGIN TRANSACTION"

commitTransactionSQLite :: SQLite.Connection -> IO ()
commitTransactionSQLite conn = SQLite.execute_ conn "COMMIT TRANSACTION"

rollbackTransactionSQLite :: SQLite.Connection -> IO ()
rollbackTransactionSQLite conn = SQLite.execute_ conn "ROLLBACK TRANSACTION"


instance BeamRunner BP.Pg where
  getBeamDebugRunner (NativePGConn conn) beM =
    \logger -> BP.runBeamPostgresDebug logger conn beM
  getBeamDebugRunner _ _ = \_ -> error "Not a Postgres connection"

instance BeamRunner BM.MySQLM where
  getBeamDebugRunner (NativeMySQLConn conn) beM =
    \logger -> BM.runBeamMySQLDebug logger conn beM
  getBeamDebugRunner _ _ = \_ -> error "Not a MySQL connection"

withTransaction :: SqlConn beM -> (NativeSqlConn -> IO a) -> IO (DBResult a)
withTransaction p actF =
  withNativeConnection p $ \nativeConn -> do
    eRes <- try @_ @SomeException $ bracketOnError (begin nativeConn) (const (rollback nativeConn)) $ const $ do
      res <- actF nativeConn
      commit nativeConn
      return res
    case eRes of
      Left e -> pure $ Left $ DBError TransactionRollbacked $ show e
      Right res -> pure $ Right res
  where
    withNativeConnection (PostgresPool _ pool) f = DP.withResource pool $ \conn -> f (NativePGConn conn)
    withNativeConnection (MySQLPool _ pool)    f = DP.withResource pool $ \conn -> f (NativeMySQLConn conn)
    withNativeConnection (SQLitePool _ pool)   f = DP.withResource pool $ \conn -> f (NativeSQLiteConn conn)
    withNativeConnection _                     f = error "Connection is not supported."
    begin     (NativePGConn conn)       = PGS.begin conn
    begin     (NativeMySQLConn conn)    = pure ()
    begin     (NativeSQLiteConn conn)   = beginTransactionSQLite conn
    commit    (NativePGConn conn)       = PGS.commit conn
    commit    (NativeMySQLConn conn)    = MySQL.commit conn
    commit    (NativeSQLiteConn conn)   = commitTransactionSQLite conn
    rollback  (NativePGConn conn)       = PGS.rollback conn
    rollback  (NativeMySQLConn conn)    = MySQL.rollback conn
    rollback  (NativeSQLiteConn conn)   = rollbackTransactionSQLite conn

-- | Representation of native DB pools that we store in FlowRuntime
data NativeSqlPool
  = NativePGPool (DP.Pool BP.Connection)         -- ^ 'Pool' with Postgres connections
  | NativeMySQLPool (DP.Pool MySQL.Connection)   -- ^ 'Pool' with MySQL connections
  | NativeSQLitePool (DP.Pool SQLite.Connection) -- ^ 'Pool' with SQLite connections
  | NativeMockedPool

-- | Representation of native DB connections that we use in implementation.
data NativeSqlConn
  = NativePGConn BP.Connection
  | NativeMySQLConn MySQL.Connection
  | NativeSQLiteConn SQLite.Connection

-- | Transform 'SqlConn' to 'NativeSqlPool'
bemToNative :: SqlConn beM -> NativeSqlPool
bemToNative (MockedPool _)        = NativeMockedPool
bemToNative (PostgresPool _ pool) = NativePGPool pool
bemToNative (MySQLPool _ pool)    = NativeMySQLPool pool
bemToNative (SQLitePool _ pool)   = NativeSQLitePool pool

-- | Create 'SqlConn' from 'DBConfig'
mkSqlConn :: DBConfig beM -> IO (SqlConn beM)
mkSqlConn (PostgresPoolConf connTag cfg PoolConfig {..}) =  PostgresPool connTag
  <$> DP.createPool (createPostgresConn cfg) BP.close stripes keepAlive resourcesPerStripe

mkSqlConn (MySQLPoolConf connTag cfg PoolConfig {..}) =  MySQLPool connTag
  <$> DP.createPool (createMySQLConn cfg) MySQL.close stripes keepAlive resourcesPerStripe

mkSqlConn (SQLitePoolConf connTag dbname PoolConfig {..}) =  SQLitePool connTag
  <$> DP.createPool (SQLite.open dbname) SQLite.close stripes keepAlive resourcesPerStripe

mkSqlConn (MockConfig connTag) = pure $ MockedPool connTag


-- | Tag for SQL connections
type ConnTag = Text

-- | Represents path to the SQLite DB
type SQliteDBname = String

-- | Represents SQL connection that we use in flow.
--   Parametrised by BEAM monad corresponding to the certain DB (MySQL, Postgres, SQLite)
data SqlConn beM
  = MockedPool ConnTag
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
  -- ^ config for 'Pool' with SQlite connections
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
  | TransactionRollbacked
  | SomeError
  deriving (Show, Eq, Ord, Enum, Bounded, Generic, ToJSON, FromJSON)

-- | Represents DB error
data DBError
  = DBError DBErrorType Text
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

-- | Represents resulting type for DB actions
type DBResult a = Either DBError a


-- | Transforms 'NativeSqlPool' to 'SqlConn'
nativeToBem :: ConnTag -> NativeSqlPool -> SqlConn beM
nativeToBem connTag NativeMockedPool        = MockedPool connTag
nativeToBem connTag (NativePGPool conn)     = PostgresPool connTag conn
nativeToBem connTag (NativeMySQLPool conn)  = MySQLPool connTag conn
nativeToBem connTag (NativeSQLitePool conn) = SQLitePool connTag conn
