module Euler.Storage.DBConfig
  ( eulerDB
  , ecDB
  , getConn
  , inSQLITEconn
  ) where

import EulerHS.Prelude hiding (id, show)
import EulerHS.Language
import EulerHS.Language as L
import EulerHS.Types
import qualified EulerHS.Types as T
import Data.Text as T
import qualified Prelude as P (show)
import Servant.Server


ecDB = sqL -- mysqlDBC --

eulerDB = sqL -- mysqlDBC --

poolConfig :: PoolConfig
poolConfig = T.PoolConfig
  { stripes = 1
  , keepAlive = 10
  , resourcesPerStripe = 50
  }

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

mysqlDBC = mkMySQLPoolConfig "eulerMysqlDB" mySQLCfg poolConfig

connSQLITEorFail :: T.DBConfig beM -> Flow (T.SqlConn beM)
connSQLITEorFail cfg = L.initSqlDBConnection cfg >>= \case
  Left e     -> error $ T.pack $  P.show e
  Right conn -> pure conn

inSQLITEconn :: Flow ()
inSQLITEconn= do
  _ <- connSQLITEorFail $ sqL
  pure ()

getConn :: T.DBConfig beM -> Flow (T.SqlConn beM)
getConn cfg = do
  conn <- getSqlDBConnection cfg
  case conn of
    Right c -> pure c
    Left err -> do
      logError @String "SqlDB" $ toText $ P.show err
      throwException err500