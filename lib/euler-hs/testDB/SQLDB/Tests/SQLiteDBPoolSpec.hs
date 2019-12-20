{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}

module SQLDB.Tests.SQLiteDBPoolSpec where


import           EulerHS.Prelude

import           EulerHS.Interpreters
import           EulerHS.Language
import qualified EulerHS.Language as L
import qualified EulerHS.Runtime as R
import           EulerHS.Types hiding (error)
import qualified EulerHS.Types as T

import           SQLDB.TestData.Connections
import           SQLDB.TestData.Scenarios.SQLite
import           SQLDB.TestData.Types

import qualified Database.Beam.Sqlite as BS
import           Test.Hspec hiding (runIO)


poolConfig :: T.PoolConfig
poolConfig = T.PoolConfig
  { stripes = 1
  , keepAlive = 10
  , resourcesPerStripe = 50
  }

sqliteCfg :: T.DBConfig BS.SqliteM
sqliteCfg = T.mkSQLitePoolConfig "eulerSQliteDB" testDBName poolConfig

spec :: Spec
spec =
  around (withEmptyDB insertTestValues sqliteCfg) $

    describe "EulerHS SQLite DB Pool tests" $ do
      it "Double connection initialization should fail" $ \rt -> do
        eRes <- runFlow rt $ do
          eConn1 <- L.initSqlDBConnection sqliteCfg
          eConn2 <- L.initSqlDBConnection sqliteCfg
          case (eConn1, eConn2) of
            (Left err, _) -> pure $ Left $ "Failed to connect 1st time: " <> show err
            (_, Left (T.DBError T.ConnectionAlreadyExists msg))
              | msg == "Connection for eulerSQliteDB already created." -> pure $ Right ()
            (_, Left err) -> pure $ Left $ "Unexpected error type on 2nd connect: " <> show err
        eRes `shouldBe` Right ()

      it "Get uninialized connection should fail" $ \rt -> do
        eRes <- runFlow rt $ do
          eConn <- L.getSqlDBConnection sqliteCfg
          case eConn of
            Left (T.DBError T.ConnectionDoesNotExist msg)
              | msg == "Connection for eulerSQliteDB does not exists." -> pure $ Right ()
            Left err -> pure $ Left $ "Unexpected error: " <> show err
            Right _ -> pure $ Left "Unexpected connection success"
        eRes `shouldBe` Right ()

      it "Init and get connection should succeed" $ \rt -> do
        eRes <- runFlow rt $ do
          eConn1 <- L.initSqlDBConnection sqliteCfg
          eConn2 <- L.getSqlDBConnection sqliteCfg
          case (eConn1, eConn2) of
            (Left err, _) -> pure $ Left $ "Failed to connect: " <> show err
            (_, Left err) -> pure $ Left $ "Unexpected error on get connection: " <> show err
            _             -> pure $ Right ()
        eRes `shouldBe` Right ()

      it "getOrInitSqlConn should succeed" $ \rt -> do
        eRes <- runFlow rt $ do
          eConn <- L.getOrInitSqlConn sqliteCfg
          case eConn of
            Left err -> pure $ Left $ "Failed to connect: " <> show err
            _        -> pure $ Right ()
        eRes `shouldBe` Right ()

      it "Prepared connection should be available" $ \rt -> do
        void $ runFlow rt $ do
          eConn <- L.initSqlDBConnection sqliteCfg
          when (isLeft eConn) $ error "Failed to prepare connection."
        void $ runFlow rt $ do
          eConn <- L.getSqlDBConnection sqliteCfg
          when (isLeft eConn) $ error "Failed to get prepared connection."

      it "Unique Constraint Violation" $ \rt -> do
        eRes <- runFlow rt (uniqueConstraintViolationDbScript sqliteCfg)
        eRes `shouldBe` (Left (DBError SomeError "SQLite3 returned ErrorConstraint while attempting to perform step: UNIQUE constraint failed: users.id"))

      it "Select one, row not found" $ \rt -> do
        eRes <- runFlow rt (selectUnknownDbScript sqliteCfg)
        eRes `shouldBe` (Right Nothing)

      it "Select one, row found" $ \rt -> do
        eRes <- runFlow rt (selectOneDbScript sqliteCfg)
        eRes `shouldSatisfy` (someUser "John" "Doe")

      it "Update / Select, row found & changed" $ \rt -> do
        eRes <- runFlow rt (updateAndSelectDbScript sqliteCfg)
        eRes `shouldSatisfy` (someUser "Leo" "San")
