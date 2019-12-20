module SQLDB.Tests.MySQLDBPoolSpec where

import           EulerHS.Prelude

import           EulerHS.Interpreters
import           EulerHS.Runtime (withFlowRuntime)
import           EulerHS.Types hiding (error)
import qualified EulerHS.Types as T

import           SQLDB.TestData.Connections (connectOrFail)
import           SQLDB.TestData.Scenarios.MySQL
import           SQLDB.TestData.Types

import           Test.Hspec hiding (runIO)


-- Configeuration

poolConfig = T.PoolConfig
  { stripes = 1
  , keepAlive = 10
  , resourcesPerStripe = 50
  }

mySQLCfg :: MySQLConfig
mySQLCfg = MySQLConfig
  { connectHost     = "localhost"
  , connectPort     = 3306
  , connectUser     = "test"
  , connectPassword = "test"
  , connectDatabase = "test"
  , connectOptions  = [CharsetName "utf8"]
  , connectPath     = ""
  , connectSSL      = Nothing
  }

mysqlConfig = mkMySQLPoolConfig "eulerMysqlDB" mySQLCfg poolConfig


-- Tests

spec :: Spec
spec =
  around (withFlowRuntime Nothing) $

    describe "EulerHS MySQL DB Pool tests" $ do
      it "Unique Constraint Violation" $ \rt -> do
        eRes <- runFlow rt $ uniqueConstraintViolationDbScript mysqlConfig
        eRes `shouldBe` (Left (DBError SomeError "ConnectionError {errFunction = \"query\", errNumber = 1062, errMessage = \"Duplicate entry '1' for key 'PRIMARY'\"}"))

      it "Select one, row not found" $ \rt -> do
        eRes <- runFlow rt $ selectUnknownDbScript mysqlConfig
        eRes `shouldBe` (Right Nothing)

      it "Select one, row found" $ \rt -> do
        eRes <- runFlow rt $ selectOneDbScript mysqlConfig
        eRes `shouldSatisfy` (someUser "John" "Doe")

      it "Update / Select, row found & changed" $ \rt -> do
        eRes <- runFlow rt $ updateAndSelectDbScript mysqlConfig
        eRes `shouldSatisfy` (someUser "Leo" "San")
