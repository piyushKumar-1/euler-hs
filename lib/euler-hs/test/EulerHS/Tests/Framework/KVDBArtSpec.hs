module EulerHS.Tests.Framework.KVDBArtSpec
  ( spec
  ) where

import           EulerHS.Prelude

import           Data.Aeson as A
import           Data.Aeson.Encode.Pretty
import qualified Data.Map as Map
import           Database.Redis (checkedConnect, defaultConnectInfo)
import           Test.Hspec

import           EulerHS.Language as L
import           EulerHS.Runtime
import           EulerHS.Tests.Framework.Common
import           EulerHS.Types as T




-- prints replay in JSON format to console
runWithRedisConn :: (Show b, Eq b) => a -> Flow b -> IO b
runWithRedisConn _ flow = do
  (recording, recResult) <- runFlowRecording initRedis flow
  print $ encode $ recording
  pure recResult
  where
    initRedis = \rt -> do
      realRedisConnection <- NativeKVDB <$> checkedConnect defaultConnectInfo
      connections         <- takeMVar $ _kvdbConnections rt
      putMVar (_kvdbConnections rt) $
        Map.insert "redis" realRedisConnection connections
      pure rt


spec :: Spec
spec = do
  describe "ART KVDB tests" $ do
    it "get a correct key" $ do
      result <- replayRecording getKey $ L.runKVDB $ do
        L.set "aaa" "bbb"
        res <- L.get "aaa"
        L.del ["aaa"]
        pure res
      result `shouldBe` Right (Just "bbb")

    it "get a wrong key" $ do
      result <- replayRecording getWrongKey $ L.runKVDB $ do
        L.set "aaa" "bbb"
        res <- L.get "aaac"
        L.del ["aaa"]
        pure res
      result `shouldBe` Right Nothing

    it "delete existing keys" $ do
      result <- replayRecording deleteExisting $ L.runKVDB $ do
        L.set "aaa" "bbb"
        L.set "ccc" "ddd"
        L.del ["aaa", "ccc"]
      result `shouldBe` Right 2

    it "delete keys (w/ no keys)" $ do
      result <- replayRecording deleteKeysNoKeys $ L.runKVDB $ do
        L.del []
      result `shouldBe` Right 0

    it "delete missing keys" $ do
      result <- replayRecording deleteMissing $ L.runKVDB $ do
        L.del ["zzz", "yyy"]
      result `shouldBe` Right 0

    it "get a correct key from transaction" $ do
      result <- replayRecording getCorrectFromTx $ L.runKVDB $ L.multiExec $ do
        L.setTx "aaa" "bbb"
        res <- L.getTx "aaa"
        L.delTx ["aaa"]
        pure res
      result `shouldBe` Right (T.TxSuccess (Just "bbb"))

    it "get incorrect key from transaction" $ do
      result <- replayRecording getIncorrectFromTx $ L.runKVDB $ L.multiExec $ do
        res <- L.getTx "aaababababa"
        pure res
      result `shouldBe` Right (T.TxSuccess Nothing)

    it "setex sets value" $ do
      let hour = 60 * 60
      result <- replayRecording setExGetKey $ L.runKVDB $ do
        L.setex "aaaex" hour "bbbex"
        res <- L.get "aaaex"
        L.del ["aaaex"]
        pure res
      result `shouldBe` Right (Just "bbbex")

    it "setex ttl works" $ do
      result <- replayRecording setExTtl $ do
        L.runKVDB $ L.setex "aaaex" 1 "bbbex"
        L.runIO $ threadDelay (2 * 10 ^ 6)
        L.runKVDB $ do
          res <- L.get "aaaex"
          L.del ["aaaex"]
          pure res
      result `shouldBe` Right Nothing



getKey :: ResultRecording
getKey = fromJust $ decode "{\"recording\":[{\"_entryName\":\"SetEntry\",\"_entryIndex\":0,\"_entryPayload\":{\"jsonValue\":[98,98,98],\"jsonResult\":{\"Right\":{\"tag\":\"Ok\"}},\"jsonKey\":[97,97,97]},\"_entryReplayMode\":\"Normal\"},{\"_entryName\":\"GetEntry\",\"_entryIndex\":1,\"_entryPayload\":{\"jsonResult\":{\"Right\":[98,98,98]},\"jsonKey\":[97,97,97]},\"_entryReplayMode\":\"Normal\"},{\"_entryName\":\"DelEntry\",\"_entryIndex\":2,\"_entryPayload\":{\"jsonResult\":{\"Right\":1},\"jsonKeys\":[[97,97,97]]},\"_entryReplayMode\":\"Normal\"}],\"forkedRecordings\":{}}"

getWrongKey :: ResultRecording
getWrongKey = fromJust $ decode "{\"recording\":[{\"_entryName\":\"SetEntry\",\"_entryIndex\":0,\"_entryPayload\":{\"jsonValue\":[98,98,98],\"jsonResult\":{\"Right\":{\"tag\":\"Ok\"}},\"jsonKey\":[97,97,97]},\"_entryReplayMode\":\"Normal\"},{\"_entryName\":\"GetEntry\",\"_entryIndex\":1,\"_entryPayload\":{\"jsonResult\":{\"Right\":null},\"jsonKey\":[97,97,97,99]},\"_entryReplayMode\":\"Normal\"},{\"_entryName\":\"DelEntry\",\"_entryIndex\":2,\"_entryPayload\":{\"jsonResult\":{\"Right\":1},\"jsonKeys\":[[97,97,97]]},\"_entryReplayMode\":\"Normal\"}],\"forkedRecordings\":{}}"

deleteExisting :: ResultRecording
deleteExisting = fromJust $ decode "{\"recording\":[{\"_entryName\":\"SetEntry\",\"_entryIndex\":0,\"_entryPayload\":{\"jsonValue\":[98,98,98],\"jsonResult\":{\"Right\":{\"tag\":\"Ok\"}},\"jsonKey\":[97,97,97]},\"_entryReplayMode\":\"Normal\"},{\"_entryName\":\"SetEntry\",\"_entryIndex\":1,\"_entryPayload\":{\"jsonValue\":[100,100,100],\"jsonResult\":{\"Right\":{\"tag\":\"Ok\"}},\"jsonKey\":[99,99,99]},\"_entryReplayMode\":\"Normal\"},{\"_entryName\":\"DelEntry\",\"_entryIndex\":2,\"_entryPayload\":{\"jsonResult\":{\"Right\":2},\"jsonKeys\":[[97,97,97],[99,99,99]]},\"_entryReplayMode\":\"Normal\"}],\"forkedRecordings\":{}}"

deleteKeysNoKeys :: ResultRecording
deleteKeysNoKeys = fromJust $ decode "{\"recording\":[{\"_entryName\":\"DelEntry\",\"_entryIndex\":0,\"_entryPayload\":{\"jsonResult\":{\"Right\":0},\"jsonKeys\":[]},\"_entryReplayMode\":\"Normal\"}],\"forkedRecordings\":{}}"

deleteMissing :: ResultRecording
deleteMissing = fromJust $ decode "{\"recording\":[{\"_entryName\":\"DelEntry\",\"_entryIndex\":0,\"_entryPayload\":{\"jsonResult\":{\"Right\":0},\"jsonKeys\":[[122,122,122],[121,121,121]]},\"_entryReplayMode\":\"Normal\"}],\"forkedRecordings\":{}}"

getCorrectFromTx :: ResultRecording
getCorrectFromTx = fromJust $ decode "{\"recording\":[{\"_entryName\":\"MultiExecEntry\",\"_entryIndex\":0,\"_entryPayload\":{\"jsonResult\":{\"Right\":{\"tag\":\"TxSuccess\",\"contents\":[98,98,98]}}},\"_entryReplayMode\":\"Normal\"}],\"forkedRecordings\":{}}"

getIncorrectFromTx :: ResultRecording
getIncorrectFromTx = fromJust $ decode "{\"recording\":[{\"_entryName\":\"MultiExecEntry\",\"_entryIndex\":0,\"_entryPayload\":{\"jsonResult\":{\"Right\":{\"tag\":\"TxSuccess\",\"contents\":null}}},\"_entryReplayMode\":\"Normal\"}],\"forkedRecordings\":{}}"

setExGetKey :: ResultRecording
setExGetKey = fromJust $ decode "{\"recording\":[{\"_entryName\":\"SetExEntry\",\"_entryIndex\":0,\"_entryPayload\":{\"jsonTtl\":3600,\"jsonValue\":[98,98,98,101,120],\"jsonResult\":{\"Right\":{\"tag\":\"Ok\"}},\"jsonKey\":[97,97,97,101,120]},\"_entryReplayMode\":\"Normal\"},{\"_entryName\":\"GetEntry\",\"_entryIndex\":1,\"_entryPayload\":{\"jsonResult\":{\"Right\":[98,98,98,101,120]},\"jsonKey\":[97,97,97,101,120]},\"_entryReplayMode\":\"Normal\"},{\"_entryName\":\"DelEntry\",\"_entryIndex\":2,\"_entryPayload\":{\"jsonResult\":{\"Right\":1},\"jsonKeys\":[[97,97,97,101,120]]},\"_entryReplayMode\":\"Normal\"}],\"forkedRecordings\":{}}"

setExTtl :: ResultRecording
setExTtl = fromJust $ decode "{\"recording\":[{\"_entryName\":\"SetExEntry\",\"_entryIndex\":0,\"_entryPayload\":{\"jsonTtl\":1,\"jsonValue\":[98,98,98,101,120],\"jsonResult\":{\"Right\":{\"tag\":\"Ok\"}},\"jsonKey\":[97,97,97,101,120]},\"_entryReplayMode\":\"Normal\"},{\"_entryName\":\"RunIOEntry\",\"_entryIndex\":1,\"_entryPayload\":{\"jsonResult\":[]},\"_entryReplayMode\":\"Normal\"},{\"_entryName\":\"GetEntry\",\"_entryIndex\":2,\"_entryPayload\":{\"jsonResult\":{\"Right\":null},\"jsonKey\":[97,97,97,101,120]},\"_entryReplayMode\":\"Normal\"},{\"_entryName\":\"DelEntry\",\"_entryIndex\":3,\"_entryPayload\":{\"jsonResult\":{\"Right\":0},\"jsonKeys\":[[97,97,97,101,120]]},\"_entryReplayMode\":\"Normal\"}],\"forkedRecordings\":{}}"
