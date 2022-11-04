{-# LANGUAGE OverloadedStrings #-}
module KV.SckvSpec where

import           EulerHS.Prelude

import           KV.FlowHelper
import           KV.Sctest
import qualified EulerHS.CachedSqlDBQuery as DB
import           Test.Hspec
import           Sequelize (Clause(..), Term(..))
import           KV.Mesh
import qualified EulerHS.Language as L
import qualified Data.Text as Text

{-
Things to test against insert
1. The value should be present in the KV againt the right primary id KEY 
2. Testing auto increment 
3. All the secondary keys should be present in KV and should be pointing to the primary key
4. The insert command should be present in the redis stream

Things to test againt find
1. Right value should be fetched with primary key 
2. Right value should be fetched with secondary keys (including composite key)
3. Right value should be fetched with secondary keys for a query which requires extra filtering of data in the application
4. 
-}


spec :: Spec
spec = flowSpec $ do
    itFlow "Should Add value to KV and fetch is successfully" $ do
        dbConf <- getEulerDbConf
        currentSC <- dummyServiceConfig
        eitherVal <- DB.createReturning dbConf meshConfig currentSC Nothing
        case eitherVal of
          Left err -> error $ "Unable to insert : " <> (show err)
          Right val -> do 
            res <- join . hush <$> DB.findOne' dbConf meshConfig Nothing [Is name (Eq $ currentSC.name)]
            asserting $ res `shouldBe` (Just val)
-- Assuming name column has unique constraint
    itFlow "Should Reject duplicate entry to KV" $ do
        dbConf <- getEulerDbConf
        currentSC <- dummyServiceConfig
        eitherVal <- DB.createReturning dbConf meshConfig currentSC Nothing
        case eitherVal of
          Left err -> error $ "Unable to insert : " <> (show err)
          Right _ -> do 
            eitherDuplicate <- DB.createReturning dbConf meshConfig currentSC Nothing
            asserting $ isLeft eitherDuplicate `shouldBe` True

dummyServiceConfig :: L.Flow ServiceConfiguration
dummyServiceConfig = do
  randomName <- Text.take 5 <$> L.generateGUID
  pure $ ServiceConfiguration
    { id = 0
    , version = 0
    , name = "KV_TEST" <> randomName
    , value = Just "VALUE"
    }
