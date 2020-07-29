module Main where

import           EulerHS.Prelude
import           Test.Hspec

import qualified EulerHS.Tests.Framework.CachedDBSpec as CachedSqlDBQuery
import qualified EulerHS.Tests.Framework.ArtSpec as Art
import qualified EulerHS.Tests.Framework.FlowSpec as Framework
import qualified EulerHS.Tests.Framework.KVDBArtSpec as KVDB
import qualified EulerHS.Tests.Framework.PubSubSpec as PubSub
import qualified EulerHS.Tests.Framework.SQLArtSpec as SQL


main = hspec $ do
  Framework.spec
  Art.spec
  CachedSqlDBQuery.spec
  KVDB.spec
  SQL.spec
  PubSub.spec
