module Euler.Storage.DBConfig
  ( eulerDB
  , ecDB
  , mysqlDBC
  ) where

import EulerHS.Prelude hiding (id, show)

import qualified EulerHS.Types as T

import qualified Database.Beam.MySQL  as BM

ecDB :: T.DBConfig BM.MySQLM
ecDB = mysqlDBC


eulerDB :: T.DBConfig BM.MySQLM
eulerDB = mysqlDBC


poolConfig :: T.PoolConfig
poolConfig = T.PoolConfig
  { stripes = 1
  , keepAlive = 10
  , resourcesPerStripe = 150
  }


mySQLCfg :: T.MySQLConfig
mySQLCfg = T.MySQLConfig
  { connectHost     = "localhost"
  , connectPort     = 3306
  , connectUser     = "cloud"
  , connectPassword = "scape"
  , connectDatabase = "jdb"
  , connectOptions  = [T.CharsetName "utf8"]
  , connectPath     = ""
  , connectSSL      = Nothing
  }


mysqlDBC :: T.DBConfig BM.MySQLM
mysqlDBC = T.mkMySQLPoolConfig "eulerMysqlDB" mySQLCfg poolConfig
