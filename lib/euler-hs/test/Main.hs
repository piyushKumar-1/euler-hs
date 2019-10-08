module Main where

import EulerHS.Prelude
import           Test.Hspec

import qualified EulerHS.Tests.Framework.FlowSpec as Framework
import qualified EulerHS.Tests.Framework.SqlDBSpec as SqlDB

main = hspec $ do
  Framework.spec
  SqlDB.spec
