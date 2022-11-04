{-# LANGUAGE OverloadedStrings #-}
module KV.SckvSpec where

import           EulerHS.Prelude

import           KV.FlowHelper
import           KV.Sctest
import qualified EulerHS.CachedSqlDBQuery as DB
import           Test.Hspec
import           Sequelize (Clause(..), Term(..))
import           KV.Mesh


spec :: Spec
spec = flowSpec $ do
    itFlow "Should Add value to KV" $ do
        dbConf <- getEulerDbConf
        eitherVal <- DB.createReturning dbConf meshConfig dummyServiceConfig Nothing
        case eitherVal of
          Left err -> error $ "Unable to insert : " <> (show err)
          Right val -> do 
            res <- join . hush <$> DB.findOne' dbConf meshConfig Nothing [Is name (Eq $ "KV_TEST")]
            asserting $ res `shouldBe` (Just val)

dummyServiceConfig :: ServiceConfiguration
dummyServiceConfig = 
  ServiceConfiguration
    { id = 0
    , version = 0
    , name = "KV_TEST"
    , value = Just "VALUE"
    }
