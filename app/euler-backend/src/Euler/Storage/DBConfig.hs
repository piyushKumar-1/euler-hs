module Euler.Storage.DBConfig
  ( eulerDB
  , ecDB
  , mysqlDBC
  ) where

import EulerHS.Prelude hiding (id, show)
import EulerHS.Language
import EulerHS.Language as L
import EulerHS.Types
import qualified EulerHS.Types as T
import Data.Text as T
import qualified Prelude as P (show)
import Servant.Server

import qualified Database.Beam.MySQL  as BM
import qualified Database.Beam.Sqlite as BS



ecDB = mysqlDBC


eulerDB = mysqlDBC


poolConfig :: PoolConfig
poolConfig = T.PoolConfig
  { stripes = 1
  , keepAlive = 10
  , resourcesPerStripe = 150
  }


sqL :: DBConfig BS.SqliteM
sqL = mkSQLitePoolConfig "sqlite" "./app/euler-backend/test/Euler/TestData/test.db" poolConfig


mySQLCfg :: MySQLConfig
mySQLCfg = MySQLConfig
  { connectHost     = "localhost"
  , connectPort     = 3306
  , connectUser     = "cloud"
  , connectPassword = "scape"
  , connectDatabase = "jdb"
  , connectOptions  = [CharsetName "utf8"]
  , connectPath     = ""
  , connectSSL      = Nothing
  }


mysqlDBC :: DBConfig BM.MySQLM
mysqlDBC = mkMySQLPoolConfig "eulerMysqlDB" mySQLCfg poolConfig