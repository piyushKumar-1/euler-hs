{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module KV.SckvSpec where

import           EulerHS.Prelude

import           KV.FlowHelper
import           KV.TestSchema.Sctest
import qualified EulerHS.CachedSqlDBQuery as DB
import           Test.Hspec
import           Sequelize (Clause(..), Term(..))
import           KV.TestSchema.Mesh
import qualified EulerHS.Language as L
import qualified Data.Text as Text
import EulerHS.KVConnector.Types hiding(kvRedis)
-- import qualified Data.Aeson as A
-- import Prelude (partialHead)
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
-- KV_TESTae4b6\\\
-- [\"]

spec :: HasCallStack => Spec
spec = flowSpec $ do
    itFlow "Should fetch a created SC using secondary key" $ do
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
            valueFromSecondaryKeys<- (snd . partialHead) <$> getValueFromSecondaryKeys secKeys
            asserting $ valueFromPrimaryKey `shouldBe` valueFromSecondaryKeys
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