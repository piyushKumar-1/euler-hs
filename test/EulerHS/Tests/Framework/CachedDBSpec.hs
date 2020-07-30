{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

module EulerHS.Tests.Framework.CachedDBSpec where

import           Test.Hspec

import           Data.Aeson as A
import           Data.Aeson.Encode.Pretty
import qualified Database.Beam as B
import qualified Database.Beam.Backend.SQL as B
import qualified Database.Beam.Query as B
import           Database.Beam.Sqlite.Connection (Sqlite, SqliteM)
import           Named
import           Sequelize

import EulerHS.CachedSqlDBQuery
import EulerHS.Interpreters as I
import EulerHS.Language as L
import EulerHS.Prelude
import EulerHS.Runtime
import EulerHS.Tests.Framework.Common
import EulerHS.Tests.Framework.DBSetup
import EulerHS.Types as T


redisCfg = T.mkKVDBConfig "eulerKVDB" T.defaultKVDBConnConfig

testKey :: Text
testKey = "testKey"

-- TODO: Start and clean up redis with each test

spec :: Spec
spec = do
  around (withEmptyDB) $
    
    describe "Cached sequelize layer" $ do

      it "findOne returns Nothing for empty table" $ \rt -> do
        res <- runFlow rt $ do
          _ <- L.initKVDBConnection redisCfg
          conn <- connectOrFail sqliteCfg
          findOne conn (Just testKey) []
        (res :: Either DBError (Maybe User)) `shouldBe` Right Nothing

      it "findOne returns first row from table" $ \rt -> do
        res <- runFlow rt $ do
          _ <- L.initKVDBConnection redisCfg
          conn <- connectOrFail sqliteCfg
          L.runDB conn $ L.insertRows $
            B.insert (users userDB) $ B.insertValues [User 1 "Bill" "Gates"]
          findOne conn (Just testKey) []
        res `shouldBe` Right (Just (User 1 "Bill" "Gates"))

      it "findOne successfully reads `Nothing` from cache" $ \rt -> do
        -- Test with `Nothing`
        res <- runFlow rt $ do
          _ <- L.initKVDBConnection redisCfg
          conn <- connectOrFail sqliteCfg
          -- This read should write `Nothing` to the cache
          _ <- findOne conn (Just testKey) []
                :: Flow (Either DBError (Maybe User))
          -- Read `Nothing` from the cache
          findOne conn (Just testKey) []
        (res :: Either DBError (Maybe User)) `shouldBe` Right Nothing
        -- Also test with a value (Just ...)

      it "findOne reads (Just result) from cache" $ \rt -> do
        res <- runFlow rt $ do
          _ <- L.initKVDBConnection redisCfg
          conn <- connectOrFail sqliteCfg
          L.runDB conn $ L.insertRows $
            B.insert (users userDB) $ B.insertValues [User 1 "Bill" "Gates"]
          _ <- findOne conn (Just testKey) []
                :: Flow (Either DBError (Maybe User))
          -- Delete value to ensure the cache is used
          L.runDB conn $ L.deleteRows $
            B.delete (users userDB) (\u -> _userGUID u B.==. 1)
          findOne conn (Just testKey) []
        res `shouldBe` Right (Just (User 1 "Bill" "Gates"))

      it "findAll finds all values in the database" $ \rt -> do
        res <- runFlow rt $ do
          redisConn <- L.initKVDBConnection redisCfg
          conn <- connectOrFail sqliteCfg
          L.runDB conn $ L.insertRows $
            B.insert (users userDB) $ B.insertValues [User 1 "Bill" "Gates"]
          L.runDB conn $ L.insertRows $
            B.insert (users userDB) $ B.insertValues [User 2 "Steve" "Jobs"]
          _ <- findAll conn (Just testKey) []
                :: Flow (Either DBError [User])
          findAll conn (Just testKey) []
        res `shouldSatisfy` \case
          Right xs -> User 1 "Bill" "Gates" `elem` xs
                      && User 2 "Steve" "Jobs" `elem` xs
          Left _ -> False

      it "findAll successfully reads `[]` from cache" $ \rt -> do
        -- Test with `Nothing`
        -- TODO: Delete value from database to ensure the cache is used
        res <- runFlow rt $ do
          _ <- L.initKVDBConnection redisCfg
          conn <- connectOrFail sqliteCfg
          -- This read should write `Nothing` to the cache
          _ <- findAll conn (Just testKey) []
                :: Flow (Either DBError [User])
          -- Read `Nothing` from the cache
          findAll conn (Just testKey) []
        (res :: Either DBError [User]) `shouldBe` Right []
        -- Also test with a value (Just ...)

      it "findAll reads nonempty list from cache after writing to it" $ \rt -> do
        res <- runFlow rt $ do
          _ <- L.initKVDBConnection redisCfg
          conn <- connectOrFail sqliteCfg
          L.runDB conn $ L.insertRows $
            B.insert (users userDB) $ B.insertValues [User 1 "Bill" "Gates"]
          L.runDB conn $ L.insertRows $
            B.insert (users userDB) $ B.insertValues [User 2 "Steve" "Jobs"]
          something <- findAll conn (Just testKey) []
                :: Flow (Either DBError [User])
          -- Delete everything to ensure the cache is used
          L.runDB conn $ L.deleteRows $
            B.delete (users userDB) (\u -> _userGUID u B.<. 3)
          findAll conn (Just testKey) []
        res `shouldSatisfy` \case
          Right xs -> User 1 "Bill" "Gates" `elem` xs
                      && User 2 "Steve" "Jobs" `elem` xs
          Left _ -> False
