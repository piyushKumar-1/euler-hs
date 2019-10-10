module EulerHS.Tests.Framework.KvdbSpec where

import           EulerHS.Prelude   hiding (getOption, get)
import           Test.Hspec        hiding (runIO)
import           EulerHS.Types
import           EulerHS.Interpreters
import           EulerHS.Language as L
import           EulerHS.Runtime (withFlowRuntime, FlowRuntime(..), Connection(Redis))
import           Database.Redis (checkedConnect, defaultConnectInfo)
import           Data.Map (insert)

withRedisConnection :: (FlowRuntime -> IO()) -> IO()
withRedisConnection next =
  withFlowRuntime Nothing $ \rt -> do
    connections     <- takeMVar $ _connections rt
    redisConnection <- checkedConnect defaultConnectInfo
    let newConnections = insert "redis" (Redis redisConnection) $ connections
    putMVar (_connections rt) newConnections
    next rt

spec :: Spec
spec = around withRedisConnection $ do
  describe "RunKVDB tests" $ do
    it "get a correct key" $ \rt -> do
      result <- runFlow rt $ L.runKVDB $ do
        set "aaa" "bbb"
        res <- get "aaa"
        del ["aaa"]
        pure res
      result `shouldBe` Right (Just "bbb")

    it "get a wrong key" $ \rt -> do
      result <- runFlow rt $ L.runKVDB $ do
        set "aaa" "bbb"
        res <- get "aaac"
        del ["aaa"]
        pure res
      result `shouldBe` Right Nothing

    it "delete existing keys" $ \rt -> do
      result <- runFlow rt $ L.runKVDB $ do
        set "aaa" "bbb"
        set "ccc" "ddd"
        del ["aaa", "ccc"]
      result `shouldBe` Right 2

    it "delete keys (w/ no keys)" $ \rt -> do
      result <- runFlow rt $ L.runKVDB $ do
        del []
      result `shouldBe` Right 0

    it "delete missing keys" $ \rt -> do
      result <- runFlow rt $ L.runKVDB $ do
        del ["zzz", "yyy"]
      result `shouldBe` Right 0
