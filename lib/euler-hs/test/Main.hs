module Main where

import EulerHS.Prelude
import           Test.Hspec

import qualified EulerHS.Tests.Framework.FlowSpec as Framework
import qualified EulerHS.Tests.Framework.SqlDBSpec as SqlDB
import qualified EulerHS.Tests.Framework.KvdbSpec as Kvdb

main = hspec $ do
  Framework.spec
  SqlDB.spec
  Kvdb.spec
