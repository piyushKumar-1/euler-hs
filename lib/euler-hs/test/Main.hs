module Main where

import           EulerHS.Prelude
import           Test.Hspec

import qualified EulerHS.Tests.Framework.FlowSpec     as Framework
import qualified EulerHS.Tests.Framework.KvdbSpec     as Kvdb
import qualified EulerHS.Tests.Framework.SQLiteDBSpec as SQLiteDB

main = hspec $ do
  Framework.spec
  SQLiteDB.spec
  Kvdb.spec
