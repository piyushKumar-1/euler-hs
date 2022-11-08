{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module KV.SckvSpec where

import           EulerHS.Prelude hiding(id)

import           KV.FlowHelper
import           KV.TestSchema.ServiceConfiguration
import qualified EulerHS.CachedSqlDBQuery as DB
import           Test.Hspec
import           Sequelize (Clause(..), Term(..))
import           KV.TestSchema.Mesh
import qualified EulerHS.Language as L
import qualified Data.Text as Text
import EulerHS.KVConnector.Types hiding(kvRedis)
import qualified EulerHS.Types as T
import           Database.Beam.MySQL (MySQLM)
import           KV.TestHelper

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

spec :: HasCallStack => Spec
spec = flowSpec $ do
    itFlow "Should fetch a created entry using secondary key" $ do
        withServiceConfig $ (\serviceConfig dbConf -> do
            eitherSC <- DB.findOne' dbConf meshConfig Nothing [Is name (Eq $ serviceConfig.name)]
            when (isLeft eitherSC) $ error $ show eitherSC
            asserting $ (join $ hush eitherSC) `shouldBe` (Just serviceConfig)
          )
    itFlow "Should add primary key and secondary keys to redis on insert command" $ do
        withServiceConfig $ (\serviceConfig _dbConf -> do
            let pKey = getLookupKeyByPKey serviceConfig
            let secKeys = getSecondaryLookupKeys serviceConfig
            (valueFromPrimaryKey :: Maybe ServiceConfiguration) <- getValueFromPrimaryKey pKey
            valueFromSecondaryKeys <- (snd . partialHead) <$> getValueFromSecondaryKeys secKeys
            asserting $ valueFromPrimaryKey `shouldBe` valueFromSecondaryKeys
          )
    itFlow "Should fetch a created entry using primary key" $ do
        withServiceConfig $ (\serviceConfig dbConf -> do
            eitherSC <- DB.findOne' dbConf meshConfig Nothing [Is id (Eq $ serviceConfig.id)]
            when (isLeft eitherSC) $ error $ show eitherSC
            asserting $ (join $ hush eitherSC) `shouldBe` (Just serviceConfig)
          )
    xitFlow "Should reject creation of duplicate entry based on the unique key" $ do
        -- Assuming name column of service config table has a unique key constraint
        withServiceConfig $ (\serviceConfig dbConf -> do
            eitherEntry <- DB.createReturning dbConf meshConfig serviceConfig Nothing
            asserting $ (isLeft eitherEntry) `shouldBe` True
          )

dummyServiceConfig :: L.Flow ServiceConfiguration
dummyServiceConfig = do
  randomName <- Text.take 5 <$> L.generateGUID
  pure $ ServiceConfiguration
    { id = 0
    , version = 0
    , name = "KV_TEST" <> randomName
    , value = Just "VALUE"
    }

withServiceConfig :: (ServiceConfiguration -> T.DBConfig MySQLM -> L.Flow a) -> L.Flow a
withServiceConfig act = do
  dbConf <- getEulerDbConf
  sc <- dummyServiceConfig
  fmap fst $ generalBracket
    (DB.createReturning dbConf meshConfig sc Nothing)
    (\_ _ -> deleteServiceConfigValueFromKV sc)
    (\eitherSC -> either (\err -> error $ show err) (\serviceConfig -> act serviceConfig dbConf) eitherSC)