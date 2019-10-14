module Main where

import           EulerHS.Prelude
import           Test.Hspec

import qualified EulerHS.Tests.Framework.FlowSpec     as Framework
import qualified EulerHS.Tests.Framework.KvdbSpec     as Kvdb
import qualified EulerHS.Tests.Framework.SQLiteDBSpec as SQLiteDB
import qualified EulerHS.Tests.Framework.ArtSpec      as Art

main = hspec $ do
  Framework.spec
  SQLiteDB.spec
  Kvdb.spec
  Art.spec
