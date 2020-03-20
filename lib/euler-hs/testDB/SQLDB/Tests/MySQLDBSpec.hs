module SQLDB.Tests.MySQLDBSpec where

import EulerHS.Prelude

import EulerHS.Interpreters
import EulerHS.Runtime (withFlowRuntime, FlowRuntime)
import EulerHS.Types hiding (error)

import SQLDB.TestData.Connections (connectOrFail)
import SQLDB.TestData.Scenarios.MySQL
import SQLDB.TestData.Types

import Test.Hspec hiding (runIO)

import qualified EulerHS.Types as T
import           EulerHS.Language
import           System.Process
import           Database.MySQL.Base
import qualified Database.Beam.MySQL as BM

import EulerHS.Extra.Test



mySQLCfg :: T.MySQLConfig
mySQLCfg = T.MySQLConfig
  { connectHost     = "127.0.0.1"
  , connectPort     = 3306
  , connectUser     = "cloud"
  , connectPassword = "scape"
  , connectDatabase = "euler_test_db"
  , connectOptions  = [T.CharsetName "utf8"]
  , connectPath     = ""
  , connectSSL      = Nothing
  }

mySQLRootCfg :: T.MySQLConfig
mySQLRootCfg =
    T.MySQLConfig
      { connectUser     = "root"
      , connectPassword = "root"
      , connectDatabase = ""
      , ..
      }
  where
    T.MySQLConfig {..} = mySQLCfg

mkMysqlConfig = T.mkMySQLConfig "eulerMysqlDB"

poolConfig = T.PoolConfig
  { stripes = 1
  , keepAlive = 10
  , resourcesPerStripe = 50
  }

mkMysqlPoolConfig mySQLCfg = mkMySQLPoolConfig "eulerMysqlDB" mySQLCfg poolConfig

spec :: Spec
spec = do
  let test dbCfg = do
        it "Unique Constraint Violation" $ \rt -> do
          eRes <- runFlow rt $ uniqueConstraintViolationDbScript dbCfg
          eRes `shouldBe`
            ( Left $ DBError
                ( SQLError $ MysqlError $
                    MysqlSqlError
                      { errFunction = "query"
                      , errNumber   = 1062
                      , errMessage  = "Duplicate entry '2' for key 'PRIMARY'"
                      }
                )
                "ConnectionError {errFunction = \"query\", errNumber = 1062, errMessage = \"Duplicate entry '2' for key 'PRIMARY'\"}"
            )

        it "Select one, row not found" $ \rt -> do
          eRes <- runFlow rt $ selectUnknownDbScript dbCfg
          eRes `shouldBe` (Right Nothing)

        it "Select one, row found" $ \rt -> do
          eRes <- runFlow rt $ selectOneDbScript dbCfg
          eRes `shouldSatisfy` (someUser "John" "Doe")

        it "Update / Select, row found & changed" $ \rt -> do
          eRes <- runFlow rt $ updateAndSelectDbScript dbCfg
          eRes `shouldSatisfy` (someUser "Leo" "San")

        it "Insert returning should return list of rows" $ \rt -> do
          eRes <- runFlow rt $ insertReturningScript dbCfg

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

  let prepare msCfgToDbCfg =
        prepareMysqlDB "testDB/SQLDB/TestData/MySQLDBSpec.sql"
        mySQLRootCfg
        mySQLCfg
        msCfgToDbCfg
        (withFlowRuntime Nothing)

  around (prepare mkMysqlConfig) $
    describe "EulerHS MySQL DB tests" $ test $ mkMysqlConfig mySQLCfg

  around (prepare mkMysqlPoolConfig) $
    describe "EulerHS MySQL DB tests. Pool" $ test $ mkMysqlPoolConfig mySQLCfg


