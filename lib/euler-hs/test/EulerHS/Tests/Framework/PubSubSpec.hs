module EulerHS.Tests.Framework.PubSubSpec
  ( spec
  ) where

import EulerHS.Prelude

import Test.Hspec

import Data.Aeson
import EulerHS.Language as L
import EulerHS.Tests.Framework.Common
import EulerHS.Types as T

-- runWithRedisConn
-- replayRecording

spec :: Spec
spec = do
  describe "Publish/Subscribe subsystem tests" $ do
    it "Callback receives messages from channel it subscribed to" $ do
      let testMsg = "Hello, Tests"
      let testCh  = "test"
      (targetMVar, watch, _) <- emptyMVarWithWatchDog 1

      result <- replayRecording rr1 $ do
        subscribe [Channel testCh] $ \msg -> L.runIO $
          putMVar targetMVar msg

        publish (Channel testCh) $ Payload testMsg

        L.runIO watch
      result `shouldBe` Just testMsg

    it "Pub/Sub works the same way if run in fork" $ do
      let testMsg = "Hello, Tests"
      let testCh  = "test"
      (targetMVar, watch, _) <- emptyMVarWithWatchDog 1

      waitSubscribe <- newEmptyMVar
      result <- replayRecording rr2 $ do
        L.forkFlow "Fork" $ do
          void $ subscribe [Channel testCh] $ \msg -> L.runIO $
            putMVar targetMVar msg

          void $ L.runIO $ putMVar waitSubscribe ()

        void $ L.runIO $ takeMVar waitSubscribe

        publish (Channel testCh) $ Payload testMsg

        L.runIO watch

      result `shouldBe` Just testMsg

    it "Callback does not receive messages from channel after unsubscribe (subscribe method)" $ do
      let testMsg = "Hello, Tests"
      let testCh  = "test"
      (targetMVar, watch, _) <- emptyMVarWithWatchDog 1

      result <- replayRecording rr3 $ do
        unsubscribe <- subscribe [Channel testCh] $ \msg -> L.runIO $
          putMVar targetMVar msg

        unsubscribe

        publish (Channel testCh) $ Payload testMsg

        L.runIO watch

      result `shouldBe` Nothing

    it "Callback receives messages from channel it subscribed to, if pattern matches" $ do
      let testMsg  = "Hello, Tests"
      let testCh0  = "0test"
      let testCh1  = "1test"
      let testPatt = "?test"
      (targetMVar, watch, reset) <- emptyMVarWithWatchDog 1

      result <- replayRecording rr4 $ do
        void $ psubscribe [ChannelPattern testPatt] $ \ch msg -> L.runIO $
          putMVar targetMVar (ch, msg)

        L.publish (Channel testCh0) $ Payload testMsg
        result0 <- L.runIO $ watch <* reset

        L.publish (Channel testCh1) $ Payload testMsg
        result1 <- L.runIO $ watch <* reset

        pure (result0, result1)

      result `shouldBe`
        ( Just (testCh0, testMsg)
        , Just (testCh1, testMsg)
        )

    it "Callback does not receive messages from channel after unsubscribe (psubscribe method)" $ do
      let testMsg  = "Hello, Tests"
      let testCh   = "ptest"
      let testPatt = "?test"
      (targetMVar, watch, _) <- emptyMVarWithWatchDog 1

      result <- replayRecording rr5 $ do
        unsubscribe <- psubscribe [ChannelPattern testPatt] $ \ch msg -> L.runIO $
          putMVar targetMVar (ch, msg)

        unsubscribe

        publish (Channel testCh) $ Payload testMsg

        L.runIO watch

      result `shouldBe` Nothing

    it "Callback receive messages from all subscribed channels" $ do
      let testMsg0 = "Hello, Tests_0"
      let testMsg1 = "Hello, Tests_1"
      let testCh0  = "test_0"
      let testCh1  = "test_1"
      (targetMVar, watch, reset) <- emptyMVarWithWatchDog 1

      result <- replayRecording rr6 $ do
        void $ L.subscribe [Channel testCh0, Channel testCh1] $ \msg -> L.runIO $
          putMVar targetMVar msg

        L.publish (Channel testCh0) $ Payload testMsg0
        result0 <- L.runIO $ watch <* reset

        L.publish (Channel testCh1) $ Payload testMsg1
        result1 <- L.runIO $ watch <* reset

        pure (result0, result1)

      result `shouldBe` (Just testMsg0, Just testMsg1)

    it "Unsubscribe unsubscribes from all subscribed channels" $ do
      let testMsg0 = "Hello, Tests_0"
      let testMsg1 = "Hello, Tests_1"
      let testCh0  = "test_0"
      let testCh1  = "test_1"
      (targetMVar, watch, reset) <- emptyMVarWithWatchDog 1

      result <- replayRecording rr7 $ do
        unsubscribe <- L.subscribe [Channel testCh0, Channel testCh1] $ \msg -> L.runIO $
          putMVar targetMVar msg

        unsubscribe

        L.publish (Channel testCh0) $ Payload testMsg0
        result0 <- L.runIO $ watch <* reset

        L.publish (Channel testCh1) $ Payload testMsg1
        result1 <- L.runIO $ watch <* reset

        pure (result0, result1)

      result `shouldBe` (Nothing, Nothing)


-- Callback receives messages from channel it subscribed to
rr1 :: ResultRecording
rr1 = fromJust $ decode $ "{\"recording\":[{\"_entryName\":\"SubscribeEntry\",\"_entryIndex\":0,\"_entryPayload\":{\"jsonChannels\":[{\"utf8\":\"test\",\"b64\":\"dGVzdA==\"}]},\"_entryReplayMode\":\"Normal\"},{\"_entryName\":\"PublishEntry\",\"_entryIndex\":1,\"_entryPayload\":{\"jsonChannel\":{\"utf8\":\"test\",\"b64\":\"dGVzdA==\"},\"jsonPayload\":{\"utf8\":\"Hello, Tests\",\"b64\":\"SGVsbG8sIFRlc3Rz\"},\"jsonResult\":{\"Right\":1}},\"_entryReplayMode\":\"Normal\"},{\"_entryName\":\"RunIOEntry\",\"_entryIndex\":2,\"_entryPayload\":{\"description\":\"\", \"jsonResult\":{\"utf8\":\"Hello, Tests\",\"b64\":\"SGVsbG8sIFRlc3Rz\"}},\"_entryReplayMode\":\"Normal\"}],\"forkedRecordings\":{}}"

-- Pub/Sub works the same way if run in fork
rr2 :: ResultRecording
rr2 = fromJust $ decode $ "{\"recording\":[{\"_entryName\":\"GenerateGUIDEntry\",\"_entryIndex\":0,\"_entryPayload\":{\"guid\":\"d20e4eb4-d3bd-48ad-9b1a-73f26c033316\"},\"_entryReplayMode\":\"Normal\"},{\"_entryName\":\"LogMessageEntry\",\"_entryIndex\":1,\"_entryPayload\":{\"tag\":\"\\\"ForkFlow\\\"\",\"msg\":\"Flow forked. Description: Fork GUID: d20e4eb4-d3bd-48ad-9b1a-73f26c033316\",\"level\":\"Info\"},\"_entryReplayMode\":\"Normal\"},{\"_entryName\":\"ForkEntry\",\"_entryIndex\":2,\"_entryPayload\":{\"guid\":\"d20e4eb4-d3bd-48ad-9b1a-73f26c033316\",\"description\":\"Fork\"},\"_entryReplayMode\":\"Normal\"},{\"_entryName\":\"RunIOEntry\",\"_entryIndex\":3,\"_entryPayload\":{\"description\":\"\", \"jsonResult\":[]},\"_entryReplayMode\":\"Normal\"},{\"_entryName\":\"PublishEntry\",\"_entryIndex\":4,\"_entryPayload\":{\"jsonChannel\":{\"utf8\":\"test\",\"b64\":\"dGVzdA==\"},\"jsonPayload\":{\"utf8\":\"Hello, Tests\",\"b64\":\"SGVsbG8sIFRlc3Rz\"},\"jsonResult\":{\"Right\":1}},\"_entryReplayMode\":\"Normal\"},{\"_entryName\":\"RunIOEntry\",\"_entryIndex\":5,\"_entryPayload\":{\"description\":\"\", \"jsonResult\":{\"utf8\":\"Hello, Tests\",\"b64\":\"SGVsbG8sIFRlc3Rz\"}},\"_entryReplayMode\":\"Normal\"}],\"forkedRecordings\":{\"d20e4eb4-d3bd-48ad-9b1a-73f26c033316\":{\"recording\":[{\"_entryName\":\"SubscribeEntry\",\"_entryIndex\":0,\"_entryPayload\":{\"jsonChannels\":[{\"utf8\":\"test\",\"b64\":\"dGVzdA==\"}]},\"_entryReplayMode\":\"Normal\"},{\"_entryName\":\"RunIOEntry\",\"_entryIndex\":1,\"_entryPayload\":{\"description\":\"\", \"jsonResult\":[]},\"_entryReplayMode\":\"Normal\"}],\"forkedRecordings\":{}}}}"

-- Callback does not receive messages from channel after unsubscribe (subscribe method)
rr3 :: ResultRecording
rr3 = fromJust $ decode $ "{\"recording\":[{\"_entryName\":\"SubscribeEntry\",\"_entryIndex\":0,\"_entryPayload\":{\"jsonChannels\":[{\"utf8\":\"test\",\"b64\":\"dGVzdA==\"}]},\"_entryReplayMode\":\"Normal\"},{\"_entryName\":\"RunIOEntry\",\"_entryIndex\":1,\"_entryPayload\":{\"description\":\"subscribe\", \"jsonResult\":[]},\"_entryReplayMode\":\"Normal\"},{\"_entryName\":\"PublishEntry\",\"_entryIndex\":2,\"_entryPayload\":{\"jsonChannel\":{\"utf8\":\"test\",\"b64\":\"dGVzdA==\"},\"jsonPayload\":{\"utf8\":\"Hello, Tests\",\"b64\":\"SGVsbG8sIFRlc3Rz\"},\"jsonResult\":{\"Right\":1}},\"_entryReplayMode\":\"Normal\"},{\"_entryName\":\"RunIOEntry\",\"_entryIndex\":3,\"_entryPayload\":{\"description\":\"\", \"jsonResult\":null},\"_entryReplayMode\":\"Normal\"}],\"forkedRecordings\":{}}"

-- Callback receives messages from channel it subscribed to, if pattern matches
rr4 :: ResultRecording
rr4 = fromJust $ decode $ "{\"recording\":[{\"_entryName\":\"PSubscribeEntry\",\"_entryIndex\":0,\"_entryPayload\":{\"jsonPatterns\":[{\"utf8\":\"?test\",\"b64\":\"P3Rlc3Q=\"}]},\"_entryReplayMode\":\"Normal\"},{\"_entryName\":\"PublishEntry\",\"_entryIndex\":1,\"_entryPayload\":{\"jsonChannel\":{\"utf8\":\"0test\",\"b64\":\"MHRlc3Q=\"},\"jsonPayload\":{\"utf8\":\"Hello, Tests\",\"b64\":\"SGVsbG8sIFRlc3Rz\"},\"jsonResult\":{\"Right\":1}},\"_entryReplayMode\":\"Normal\"},{\"_entryName\":\"RunIOEntry\",\"_entryIndex\":2,\"_entryPayload\":{\"description\":\"\", \"jsonResult\":[{\"utf8\":\"0test\",\"b64\":\"MHRlc3Q=\"},{\"utf8\":\"Hello, Tests\",\"b64\":\"SGVsbG8sIFRlc3Rz\"}]},\"_entryReplayMode\":\"Normal\"},{\"_entryName\":\"PublishEntry\",\"_entryIndex\":3,\"_entryPayload\":{\"jsonChannel\":{\"utf8\":\"1test\",\"b64\":\"MXRlc3Q=\"},\"jsonPayload\":{\"utf8\":\"Hello, Tests\",\"b64\":\"SGVsbG8sIFRlc3Rz\"},\"jsonResult\":{\"Right\":1}},\"_entryReplayMode\":\"Normal\"},{\"_entryName\":\"RunIOEntry\",\"_entryIndex\":4,\"_entryPayload\":{\"description\":\"\", \"jsonResult\":[{\"utf8\":\"1test\",\"b64\":\"MXRlc3Q=\"},{\"utf8\":\"Hello, Tests\",\"b64\":\"SGVsbG8sIFRlc3Rz\"}]},\"_entryReplayMode\":\"Normal\"}],\"forkedRecordings\":{}}"

-- Callback does not receive messages from channel after unsubscribe (psubscribe method)
rr5 :: ResultRecording
rr5 = fromJust $ decode $ "{\"recording\":[{\"_entryName\":\"PSubscribeEntry\",\"_entryIndex\":0,\"_entryPayload\":{\"jsonPatterns\":[{\"utf8\":\"?test\",\"b64\":\"P3Rlc3Q=\"}]},\"_entryReplayMode\":\"Normal\"},{\"_entryName\":\"RunIOEntry\",\"_entryIndex\":1,\"_entryPayload\":{\"description\":\"psubscribe\", \"jsonResult\":[]},\"_entryReplayMode\":\"Normal\"},{\"_entryName\":\"PublishEntry\",\"_entryIndex\":2,\"_entryPayload\":{\"jsonChannel\":{\"utf8\":\"ptest\",\"b64\":\"cHRlc3Q=\"},\"jsonPayload\":{\"utf8\":\"Hello, Tests\",\"b64\":\"SGVsbG8sIFRlc3Rz\"},\"jsonResult\":{\"Right\":1}},\"_entryReplayMode\":\"Normal\"},{\"_entryName\":\"RunIOEntry\",\"_entryIndex\":3,\"_entryPayload\":{\"description\":\"\", \"jsonResult\":null},\"_entryReplayMode\":\"Normal\"}],\"forkedRecordings\":{}}"

-- Callback receive messages from all subscribed channels
rr6 :: ResultRecording
rr6 = fromJust $ decode $ "{\"recording\":[{\"_entryName\":\"SubscribeEntry\",\"_entryIndex\":0,\"_entryPayload\":{\"jsonChannels\":[{\"utf8\":\"test_0\",\"b64\":\"dGVzdF8w\"},{\"utf8\":\"test_1\",\"b64\":\"dGVzdF8x\"}]},\"_entryReplayMode\":\"Normal\"},{\"_entryName\":\"PublishEntry\",\"_entryIndex\":1,\"_entryPayload\":{\"jsonChannel\":{\"utf8\":\"test_0\",\"b64\":\"dGVzdF8w\"},\"jsonPayload\":{\"utf8\":\"Hello, Tests_0\",\"b64\":\"SGVsbG8sIFRlc3RzXzA=\"},\"jsonResult\":{\"Right\":1}},\"_entryReplayMode\":\"Normal\"},{\"_entryName\":\"RunIOEntry\",\"_entryIndex\":2,\"_entryPayload\":{\"description\":\"\", \"jsonResult\":{\"utf8\":\"Hello, Tests_0\",\"b64\":\"SGVsbG8sIFRlc3RzXzA=\"}},\"_entryReplayMode\":\"Normal\"},{\"_entryName\":\"PublishEntry\",\"_entryIndex\":3,\"_entryPayload\":{\"jsonChannel\":{\"utf8\":\"test_1\",\"b64\":\"dGVzdF8x\"},\"jsonPayload\":{\"utf8\":\"Hello, Tests_1\",\"b64\":\"SGVsbG8sIFRlc3RzXzE=\"},\"jsonResult\":{\"Right\":1}},\"_entryReplayMode\":\"Normal\"},{\"_entryName\":\"RunIOEntry\",\"_entryIndex\":4,\"_entryPayload\":{\"description\":\"\", \"jsonResult\":{\"utf8\":\"Hello, Tests_1\",\"b64\":\"SGVsbG8sIFRlc3RzXzE=\"}},\"_entryReplayMode\":\"Normal\"}],\"forkedRecordings\":{}}"

-- Unsubscribe unsubscribes from all subscribed channels
rr7 :: ResultRecording
rr7 = fromJust $ decode $ "{\"recording\":[{\"_entryName\":\"SubscribeEntry\",\"_entryIndex\":0,\"_entryPayload\":{\"jsonChannels\":[{\"utf8\":\"test_0\",\"b64\":\"dGVzdF8w\"},{\"utf8\":\"test_1\",\"b64\":\"dGVzdF8x\"}]},\"_entryReplayMode\":\"Normal\"},{\"_entryName\":\"RunIOEntry\",\"_entryIndex\":1,\"_entryPayload\":{\"description\":\"subscribe\", \"jsonResult\":[]},\"_entryReplayMode\":\"Normal\"},{\"_entryName\":\"PublishEntry\",\"_entryIndex\":2,\"_entryPayload\":{\"jsonChannel\":{\"utf8\":\"test_0\",\"b64\":\"dGVzdF8w\"},\"jsonPayload\":{\"utf8\":\"Hello, Tests_0\",\"b64\":\"SGVsbG8sIFRlc3RzXzA=\"},\"jsonResult\":{\"Right\":1}},\"_entryReplayMode\":\"Normal\"},{\"_entryName\":\"RunIOEntry\",\"_entryIndex\":3,\"_entryPayload\":{\"description\":\"\", \"jsonResult\":null},\"_entryReplayMode\":\"Normal\"},{\"_entryName\":\"PublishEntry\",\"_entryIndex\":4,\"_entryPayload\":{\"jsonChannel\":{\"utf8\":\"test_1\",\"b64\":\"dGVzdF8x\"},\"jsonPayload\":{\"utf8\":\"Hello, Tests_1\",\"b64\":\"SGVsbG8sIFRlc3RzXzE=\"},\"jsonResult\":{\"Right\":0}},\"_entryReplayMode\":\"Normal\"},{\"_entryName\":\"RunIOEntry\",\"_entryIndex\":5,\"_entryPayload\":{\"description\":\"\", \"jsonResult\":null},\"_entryReplayMode\":\"Normal\"}],\"forkedRecordings\":{}}"
