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

spec :: Spec
spec = do
  around (withEmptyDB) $
    
    describe "Cached sequelize layer" $ do

      it "findOne returns Nothing for empty table" $ \rt -> do
        res <- runFlow rt $ do
          conn <- connectOrFail sqliteCfg
          let where_ = []
          findOne conn (Just "testKey") where_
        (res :: Either DBError (Maybe User)) `shouldBe` Right Nothing

      it "findOne returns first row from table" $ \rt -> do
        res <- runFlow rt $ do
          conn <- connectOrFail sqliteCfg
          L.runDB conn $ L.insertRows $
            B.insert (users userDB) $ B.insertValues [User 1 "Bill" "Gates"]
          findOne conn (Just "testKey") []
        res `shouldBe` Right (Just (User 1 "Bill" "Gates"))

      it "findOne successfully reads cache after writing to it" $ \rt -> do
        -- Test with `Nothing`
        res <- runFlow rt $ do
          conn <- connectOrFail sqliteCfg
          -- This read should write `Nothing` to the cache
          _ <- findOne conn (Just "testKey") []
                :: Flow (Either DBError (Maybe User))
          -- Read `Nothing` from the cache
          findOne conn (Just "testKey") []
        (res :: Either DBError (Maybe User)) `shouldBe` Right Nothing
        -- Also test with a value (Just ...)
        res2 <- runFlow rt $ do
          conn <- connectOrFail sqliteCfg
          L.runDB conn $ L.insertRows $
            B.insert (users userDB) $ B.insertValues [User 1 "Bill" "Gates"]
          _ <- findOne conn (Just "testKey") []
                :: Flow (Either DBError (Maybe User))
          findOne conn (Just "testKey") []
        res2 `shouldBe` Right (Just (User 1 "Bill" "Gates"))
    
      -- Next: findOne actually finds something
      -- then test caching
