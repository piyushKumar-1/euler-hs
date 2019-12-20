module SQLDB.Tests.PostgresDBPoolSpec where

import           EulerHS.Prelude

import           EulerHS.Interpreters
import           EulerHS.Language
import           EulerHS.Runtime (withFlowRuntime)
import           EulerHS.Types hiding (error)
import qualified EulerHS.Types as T

import           SQLDB.TestData.Connections (connectOrFail)
import           SQLDB.TestData.Scenarios.Postgres
import           SQLDB.TestData.Types

import           Test.Hspec hiding (runIO)


pgCfg' = T.PostgresConfig
  { connectHost = "localhost" --String
  , connectPort = 5432 --Word16
  , connectUser = "postgres" -- String
  , connectPassword = "postgres" -- String
  , connectDatabase = "testdb" --  String
  }

poolConfig = T.PoolConfig
  { stripes = 1
  , keepAlive = 10
  , resourcesPerStripe = 50
  }

pgCfg = mkPostgresPoolConfig "eulerPGDB" pgCfg' poolConfig


spec :: Spec
spec =
  around (withFlowRuntime Nothing) $

    describe "EulerHS Postgres DB Pool tests" $ do
      it "Unique Constraint Violation" $ \rt -> do
        eRes <- runFlow rt $ uniqueConstraintViolationDbScript pgCfg
        eRes `shouldBe` (Left (DBError SomeError "SqlError {sqlState = \"23505\", sqlExecStatus = FatalError, sqlErrorMsg = \"duplicate key value violates unique constraint \\\"users_pk\\\"\", sqlErrorDetail = \"Key (id)=(1) already exists.\", sqlErrorHint = \"\"}"))

      it "Select one, row not found" $ \rt -> do
        eRes <- runFlow rt $ selectUnknownDbScript pgCfg
        eRes `shouldBe` (Right Nothing)

      it "Select one, row found" $ \rt -> do
        eRes <- runFlow rt $ selectOneDbScript pgCfg
        eRes `shouldSatisfy` (someUser "John" "Doe")

      it "Update / Select, row found & changed" $ \rt -> do
        eRes <- runFlow rt $ updateAndSelectDbScript pgCfg
        eRes `shouldSatisfy` (someUser "Leo" "San")
