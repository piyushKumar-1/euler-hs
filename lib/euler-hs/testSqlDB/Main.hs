module Main where

import           EulerHS.Prelude
import           Test.Hspec


import qualified SQLDB.Tests.SQLiteDBSpec as SQLiteDB
import qualified SQLDB.Tests.SQLiteDBPoolSpec as SQLiteDBP
import qualified SQLDB.Tests.PostgresDBSpec as PGDB
import qualified SQLDB.Tests.PostgresDBPoolSpec as PGDBP
import qualified SQLDB.Tests.MySQLDBSpec as MySQL
import qualified SQLDB.Tests.MySQLDBPoolSpec as MySQLP

main = hspec $ do
  SQLiteDB.spec
  SQLiteDBP.spec
 -- PGDB.spec
 -- PGDBP.spec
 -- MySQL.spec
 -- MySQLP.spec
