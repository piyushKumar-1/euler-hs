module EulerHS.Tests.Framework.KvdbSpec where

import           EulerHS.Prelude   hiding (getOption, get)
import           Test.Hspec        hiding (runIO)
import qualified Test.Hspec as HS
import           EulerHS.Types
import           EulerHS.Interpreters
import           EulerHS.Language as L
import           EulerHS.Runtime (withFlowRuntime, FlowRuntime(..)) -- , Connection(Redis))
import           Database.Redis (checkedConnect, defaultConnectInfo, TxResult(TxSuccess), Status(..))
import           Data.Map (insert)
import           Unsafe.Coerce

-- data KVDBMockedValues' = KVDBMockedValues'
--   { kvdbSet    :: [ RD.Status]
--   , kvdbGet    :: [ (Maybe ByteString)]
--   , kvdbExists :: [ Bool]
--   , kvdbDel    :: [ Integer]
--   , kvdbExpire :: [ Bool]
--   , kvdbIncr   :: [ Integer]
--   , kvdbHSet   :: [ Bool]
--   , kvdbHGet   :: [ (Maybe ByteString)]
--   } deriving (Generic, Typeable)

justBBB :: Any
justBBB = unsafeCoerce (Just "bbb" :: Maybe ByteString)

txnothing :: Any
txnothing = unsafeCoerce (Nothing)

mockedValues = KVDBMockedValues'
  { kvdbSet    = [ Ok, Ok, Ok, Ok]
  , kvdbGet    = [ Just "bbb", Nothing, Nothing, Nothing]
  , kvdbExists = [ True ]
  , kvdbDel    = [ 0,0,2, 0,0,4,5]
  , kvdbExpire = [ ]
  , kvdbIncr   = [ ]
  , kvdbHSet   = [ ]
  , kvdbHGet   = [ ]
  , kvdbTX     = [TxSuccess justBBB, TxSuccess txnothing]
  }

withRedisConnection :: MVar KVDBMockedValues' -> (FlowRuntime -> IO()) -> IO()
withRedisConnection mv next =
  withFlowRuntime Nothing $ \rt -> do
    mVals <- newMVar mockedValues
    connections     <- takeMVar $ _connections rt
    redisConnection <- checkedConnect defaultConnectInfo
    let newConnections = insert "redis" (Mocked mv) $ connections
    putMVar (_connections rt) newConnections
    next rt

spec :: Spec
spec = do
  mv <- HS.runIO $newMVar mockedValues
  around (withRedisConnection mv) $ do
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

      it "get a correct key from transaction" $ \rt -> do
        result <- runFlow rt $ L.runKVDB $ multiExec $ do
          setTx "aaa" "bbb"
          res <- getTx "aaa"
          delTx ["aaa"]
          pure res
        result `shouldBe` Right (TxSuccess (Just "bbb"))

      it "get incorrect key from transaction" $ \rt -> do
        result <- runFlow rt $ L.runKVDB $ multiExec $ do
          res <- getTx "aaababababa"
          pure res
        result `shouldBe` Right (TxSuccess Nothing)
