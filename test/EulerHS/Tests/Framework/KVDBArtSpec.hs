module EulerHS.Tests.Framework.KVDBArtSpec
  ( spec
  ) where

import           EulerHS.Prelude

import           Data.Aeson as A
import           Test.Hspec

import           EulerHS.Language as L
import           EulerHS.Runtime
import           EulerHS.Tests.Framework.Common
import           EulerHS.Types as T
import qualified Database.Redis as R

connectInfo :: R.ConnectInfo
-- connectInfo = R.defaultConnectInfo -- fot recording
connectInfo = R.defaultConnectInfo {R.connectHost = "redis"} -- for replaying

runWithRedisConn_ :: ResultRecording -> Flow b -> IO b
-- runWithRedisConn_ = runWithRedisConn connectInfo -- for recording
runWithRedisConn_ = replayRecording

spec :: Spec
spec = do
  describe "ART KVDB tests" $ do
    it "get a correct key" $ do
      result <- runWithRedisConn_ getKey $ L.runKVDB "redis" $ do
        L.set "aaa" "bbb"
        res <- L.get "aaa"
        L.del ["aaa"]
        pure res
      result `shouldBe` Right (Just "bbb")

    it "get a wrong key" $ do
      result <- runWithRedisConn_ getWrongKey $ L.runKVDB "redis" $ do
        L.set "aaa" "bbb"
        res <- L.get "aaac"
        L.del ["aaa"]
        pure res
      result `shouldBe` Right Nothing

    it "delete existing keys" $ do
      result <- runWithRedisConn_ deleteExisting $ L.runKVDB "redis" $ do
        L.set "aaa" "bbb"
        L.set "ccc" "ddd"
        L.del ["aaa", "ccc"]
      result `shouldBe` Right 2

    it "delete keys (w/ no keys)" $ do
      result <- runWithRedisConn_ deleteKeysNoKeys $ L.runKVDB "redis" $ do
        L.del []
      result `shouldBe` Right 0

    it "delete missing keys" $ do
      result <- runWithRedisConn_ deleteMissing $ L.runKVDB "redis" $ do
        L.del ["zzz", "yyy"]
      result `shouldBe` Right 0

    it "get a correct key from transaction" $ do
      result <- runWithRedisConn_ getCorrectFromTx $ L.runKVDB "redis" $ L.multiExec $ do
        L.setTx "aaa" "bbb"
        res <- L.getTx "aaa"
        L.delTx ["aaa"]
        pure res
      result `shouldBe` Right (T.TxSuccess (Just "bbb"))

    it "get incorrect key from transaction" $ do
      result <- runWithRedisConn_ getIncorrectFromTx $ L.runKVDB "redis" $ L.multiExec $ do
        res <- L.getTx "aaababababa"
        pure res
      result `shouldBe` Right (T.TxSuccess Nothing)

    it "setex sets value" $ do
      let hour = 60 * 60
      result <- runWithRedisConn_ setExGetKey $ L.runKVDB "redis" $ do
        L.setex "aaaex" hour "bbbex"
        res <- L.get "aaaex"
        L.del ["aaaex"]
        pure res
      result `shouldBe` Right (Just "bbbex")

    it "setex ttl works" $ do
      result <- runWithRedisConn_ setExTtl $ do
        L.runKVDB "redis" $ L.setex "aaaex" 1 "bbbex"
        L.runIO $ threadDelay (2 * 10 ^ 6)
        L.runKVDB "redis" $ do
          res <- L.get "aaaex"
          L.del ["aaaex"]
          pure res
      result `shouldBe` Right Nothing


getKey :: ResultRecording
getKey = fromJust $ decode "{\"recording\":[{\"_entryName\":\"SetEntry\",\"_entryIndex\":0,\"_entryPayload\":{\"jsonValue\":{\"utf8\":\"bbb\",\"b64\":\"YmJi\"},\"jsonResult\":{\"Right\":{\"tag\":\"Ok\"}},\"jsonKey\":{\"utf8\":\"aaa\",\"b64\":\"YWFh\"}},\"_entryReplayMode\":\"Normal\"},{\"_entryName\":\"GetEntry\",\"_entryIndex\":1,\"_entryPayload\":{\"jsonResult\":{\"Right\":{\"utf8\":\"bbb\",\"b64\":\"YmJi\"}},\"jsonKey\":{\"utf8\":\"aaa\",\"b64\":\"YWFh\"}},\"_entryReplayMode\":\"Normal\"},{\"_entryName\":\"DelEntry\",\"_entryIndex\":2,\"_entryPayload\":{\"jsonResult\":{\"Right\":1},\"jsonKeys\":[{\"utf8\":\"aaa\",\"b64\":\"YWFh\"}]},\"_entryReplayMode\":\"Normal\"}],\"forkedRecordings\":{},\"safeRecordings\":{}}"

getWrongKey :: ResultRecording
getWrongKey = fromJust $ decode "{\"recording\":[{\"_entryName\":\"SetEntry\",\"_entryIndex\":0,\"_entryPayload\":{\"jsonValue\":{\"utf8\":\"bbb\",\"b64\":\"YmJi\"},\"jsonResult\":{\"Right\":{\"tag\":\"Ok\"}},\"jsonKey\":{\"utf8\":\"aaa\",\"b64\":\"YWFh\"}},\"_entryReplayMode\":\"Normal\"},{\"_entryName\":\"GetEntry\",\"_entryIndex\":1,\"_entryPayload\":{\"jsonResult\":{\"Right\":null},\"jsonKey\":{\"utf8\":\"aaac\",\"b64\":\"YWFhYw==\"}},\"_entryReplayMode\":\"Normal\"},{\"_entryName\":\"DelEntry\",\"_entryIndex\":2,\"_entryPayload\":{\"jsonResult\":{\"Right\":1},\"jsonKeys\":[{\"utf8\":\"aaa\",\"b64\":\"YWFh\"}]},\"_entryReplayMode\":\"Normal\"}],\"forkedRecordings\":{},\"safeRecordings\":{}}"

deleteExisting :: ResultRecording
deleteExisting = fromJust $ decode "{\"recording\":[{\"_entryName\":\"SetEntry\",\"_entryIndex\":0,\"_entryPayload\":{\"jsonValue\":{\"utf8\":\"bbb\",\"b64\":\"YmJi\"},\"jsonResult\":{\"Right\":{\"tag\":\"Ok\"}},\"jsonKey\":{\"utf8\":\"aaa\",\"b64\":\"YWFh\"}},\"_entryReplayMode\":\"Normal\"},{\"_entryName\":\"SetEntry\",\"_entryIndex\":1,\"_entryPayload\":{\"jsonValue\":{\"utf8\":\"ddd\",\"b64\":\"ZGRk\"},\"jsonResult\":{\"Right\":{\"tag\":\"Ok\"}},\"jsonKey\":{\"utf8\":\"ccc\",\"b64\":\"Y2Nj\"}},\"_entryReplayMode\":\"Normal\"},{\"_entryName\":\"DelEntry\",\"_entryIndex\":2,\"_entryPayload\":{\"jsonResult\":{\"Right\":2},\"jsonKeys\":[{\"utf8\":\"aaa\",\"b64\":\"YWFh\"},{\"utf8\":\"ccc\",\"b64\":\"Y2Nj\"}]},\"_entryReplayMode\":\"Normal\"}],\"forkedRecordings\":{},\"safeRecordings\":{}}"

deleteKeysNoKeys :: ResultRecording
deleteKeysNoKeys = fromJust $ decode "{\"recording\":[{\"_entryName\":\"DelEntry\",\"_entryIndex\":0,\"_entryPayload\":{\"jsonResult\":{\"Right\":0},\"jsonKeys\":[]},\"_entryReplayMode\":\"Normal\"}],\"forkedRecordings\":{},\"safeRecordings\":{}}"

deleteMissing :: ResultRecording
deleteMissing = fromJust $ decode "{\"recording\":[{\"_entryName\":\"DelEntry\",\"_entryIndex\":0,\"_entryPayload\":{\"jsonResult\":{\"Right\":0},\"jsonKeys\":[{\"utf8\":\"zzz\",\"b64\":\"enp6\"},{\"utf8\":\"yyy\",\"b64\":\"eXl5\"}]},\"_entryReplayMode\":\"Normal\"}],\"forkedRecordings\":{},\"safeRecordings\":{}}"

getCorrectFromTx :: ResultRecording
getCorrectFromTx = fromJust $ decode "{\"recording\":[{\"_entryName\":\"MultiExecEntry\",\"_entryIndex\":0,\"_entryPayload\":{\"jsonResult\":{\"Right\":{\"tag\":\"TxSuccess\",\"contents\":{\"utf8\":\"bbb\",\"b64\":\"YmJi\"}}}},\"_entryReplayMode\":\"Normal\"}],\"forkedRecordings\":{},\"safeRecordings\":{}}"

getIncorrectFromTx :: ResultRecording
getIncorrectFromTx = fromJust $ decode "{\"recording\":[{\"_entryName\":\"MultiExecEntry\",\"_entryIndex\":0,\"_entryPayload\":{\"jsonResult\":{\"Right\":{\"tag\":\"TxSuccess\",\"contents\":null}}},\"_entryReplayMode\":\"Normal\"}],\"forkedRecordings\":{},\"safeRecordings\":{}}"

setExGetKey :: ResultRecording
setExGetKey = fromJust $ decode "{\"recording\":[{\"_entryName\":\"SetExEntry\",\"_entryIndex\":0,\"_entryPayload\":{\"jsonTtl\":3600,\"jsonValue\":{\"utf8\":\"bbbex\",\"b64\":\"YmJiZXg=\"},\"jsonResult\":{\"Right\":{\"tag\":\"Ok\"}},\"jsonKey\":{\"utf8\":\"aaaex\",\"b64\":\"YWFhZXg=\"}},\"_entryReplayMode\":\"Normal\"},{\"_entryName\":\"GetEntry\",\"_entryIndex\":1,\"_entryPayload\":{\"jsonResult\":{\"Right\":{\"utf8\":\"bbbex\",\"b64\":\"YmJiZXg=\"}},\"jsonKey\":{\"utf8\":\"aaaex\",\"b64\":\"YWFhZXg=\"}},\"_entryReplayMode\":\"Normal\"},{\"_entryName\":\"DelEntry\",\"_entryIndex\":2,\"_entryPayload\":{\"jsonResult\":{\"Right\":1},\"jsonKeys\":[{\"utf8\":\"aaaex\",\"b64\":\"YWFhZXg=\"}]},\"_entryReplayMode\":\"Normal\"}],\"forkedRecordings\":{},\"safeRecordings\":{}}"

setExTtl :: ResultRecording
setExTtl = fromJust $ decode "{\"recording\":[{\"_entryName\":\"SetExEntry\",\"_entryIndex\":0,\"_entryPayload\":{\"jsonTtl\":1,\"jsonValue\":{\"utf8\":\"bbbex\",\"b64\":\"YmJiZXg=\"},\"jsonResult\":{\"Right\":{\"tag\":\"Ok\"}},\"jsonKey\":{\"utf8\":\"aaaex\",\"b64\":\"YWFhZXg=\"}},\"_entryReplayMode\":\"Normal\"},{\"_entryName\":\"RunIOEntry\",\"_entryIndex\":1,\"_entryPayload\":{\"jsonResult\":[],\"description\":\"\"},\"_entryReplayMode\":\"Normal\"},{\"_entryName\":\"GetEntry\",\"_entryIndex\":2,\"_entryPayload\":{\"jsonResult\":{\"Right\":null},\"jsonKey\":{\"utf8\":\"aaaex\",\"b64\":\"YWFhZXg=\"}},\"_entryReplayMode\":\"Normal\"},{\"_entryName\":\"DelEntry\",\"_entryIndex\":3,\"_entryPayload\":{\"jsonResult\":{\"Right\":0},\"jsonKeys\":[{\"utf8\":\"aaaex\",\"b64\":\"YWFhZXg=\"}]},\"_entryReplayMode\":\"Normal\"}],\"forkedRecordings\":{},\"safeRecordings\":{}}"