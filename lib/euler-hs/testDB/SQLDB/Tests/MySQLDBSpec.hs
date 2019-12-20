module SQLDB.Tests.MySQLDBSpec where

import EulerHS.Prelude

import EulerHS.Interpreters
import EulerHS.Runtime (withFlowRuntime)
import EulerHS.Types hiding (error)

import SQLDB.TestData.Connections (connectOrFail)
import SQLDB.TestData.Scenarios.MySQL
import SQLDB.TestData.Types

import Test.Hspec hiding (runIO)


-- Configurations

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

mysqlConfig = mkMySQLConfig "eulerMysqlDB" mySQLCfg


-- Tests

spec :: Spec
spec =
  around (withFlowRuntime Nothing) $

    describe "EulerHS MySQL DB tests" $ do
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

      it "Insert returning should return list of rows" $ \rt -> do
        eRes <- runFlow rt $ insertReturningScript mysqlConfig

        case eRes of
          Left  _  -> expectationFailure "Left DBResult"
          Right us -> do
            length us `shouldBe` 2
            let u1 = us !! 0
            let u2 = us !! 1

            _userFirstName u1 `shouldBe` "John"
            _userLastName  u1 `shouldBe` "Doe"

            _userFirstName u2 `shouldBe` "Doe"
            _userLastName  u2 `shouldBe` "John"

