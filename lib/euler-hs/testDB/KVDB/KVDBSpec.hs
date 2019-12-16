module KVDB.KVDBSpec where

import           Test.Hspec hiding (runIO)

import           EulerHS.Prelude
import           EulerHS.Interpreters
import qualified EulerHS.Language          as L
import           EulerHS.Runtime
import qualified EulerHS.Types             as T


redisCfg = T.mkKVDBConfig "eulerKVDB" T.defaultKVDBConnConfig

spec :: Spec
spec =
  around (withFlowRuntime Nothing) $

    describe "EulerHS Redis DB tests" $ do

      it "Double connection initialization should fail" $ \rt -> do
        eRes <- runFlow rt $ do
          eConn1 <- L.initKVDBConnection redisCfg
          eConn2 <- L.initKVDBConnection redisCfg
          case (eConn1, eConn2) of
            (Left err, _) -> pure $ Left $ "Failed to connect 1st time: " <> show err
            (_, Left (T.ExceptionMessage msg))
              | msg == "Connection for eulerKVDB already created." -> pure $ Right ()
            (_, Left err) -> pure $ Left $ "Unexpected error type on 2nd connect: " <> show err
        eRes `shouldBe` Right ()

      it "Get uninialized connection should fail" $ \rt -> do
        eRes <- runFlow rt $ do
          eConn <- L.getKVDBConnection redisCfg
          case eConn of
            Left (T.ExceptionMessage msg)
              | msg == "Connection for eulerKVDB does not exists." -> pure $ Right ()
            Left err -> pure $ Left $ "Unexpected error: " <> show err
            Right _ -> pure $ Left "Unexpected connection success"
        eRes `shouldBe` Right ()

      it "Init and get connection should succeed" $ \rt -> do
        eRes <- runFlow rt $ do
          eConn1 <- L.initKVDBConnection redisCfg
          eConn2 <- L.getKVDBConnection redisCfg
          case (eConn1, eConn2) of
            (Left err, _) -> pure $ Left $ "Failed to connect: " <> show err
            (_, Left err) -> pure $ Left $ "Unexpected error on get connection: " <> show err
            _             -> pure $ Right ()
        eRes `shouldBe` Right ()

      it "Init and double get connection should succeed" $ \rt -> do
        eRes <- runFlow rt $ do
          eConn1 <- L.initKVDBConnection redisCfg
          eConn2 <- L.getKVDBConnection redisCfg
          eConn3 <- L.getKVDBConnection redisCfg
          case (eConn1, eConn2, eConn3) of
            (Left err, _, _) -> pure $ Left $ "Failed to connect: " <> show err
            (_, Left err, _) -> pure $ Left $ "Unexpected error on 1st get connection: " <> show err
            (_, _, Left err) -> pure $ Left $ "Unexpected error on 2nd get connection: " <> show err
            _                -> pure $ Right ()
        eRes `shouldBe` Right ()

      it "getOrInitRedisConn should succeed" $ \rt -> do
        eRes <- runFlow rt $ do
          eConn <- L.getOrInitRedisConn redisCfg
          case eConn of
            Left err -> pure $ Left $ "Failed to connect: " <> show err
            _        -> pure $ Right ()
        eRes `shouldBe` Right ()

      it "Prepared connection should be available" $ \rt -> do
        void $ runFlow rt $ do
          eConn <- L.initKVDBConnection redisCfg
          when (isLeft eConn) $ error "Failed to prepare connection."
        void $ runFlow rt $ do
          eConn <- L.getKVDBConnection redisCfg
          when (isLeft eConn) $ error "Failed to get prepared connection."
