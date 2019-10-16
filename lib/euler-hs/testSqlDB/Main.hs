module Main where

import           EulerHS.Prelude
import           Test.Hspec


import qualified SQLDB.Tests.SQLiteDBSpec as SQLiteDB
import qualified SQLDB.Tests.PostgresDBSpec as PGDB
import qualified SQLDB.Tests.MySQLDBSpec as MySQL

main = hspec $ do
    SQLiteDB.spec
    PGDB.spec
    MySQL.spec
  