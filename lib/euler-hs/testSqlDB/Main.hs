module Main where

import           EulerHS.Prelude
import           Test.Hspec


import qualified SQLDB.Tests.SQLiteDBSpec as SQLiteDB
import qualified SQLDB.Tests.PostgresDBSpec as PGDB
--import qualified SQLDB.Tests.PostgresDBPoolSpec as PGDBP
import qualified SQLDB.Tests.MySQLDBSpec as MySQL
--import qualified SQLDB.Tests.MySQLDBPoolSpec as MySQLP
-- import qualified SQLDB.Tests.RunDb2Spec as RD2

main = hspec $ do
  SQLiteDB.spec
   -- RD2.spec
  -- PGDB.spec
  --PGDBP.spec
  -- MySQL.spec
  --MySQLP.spec
