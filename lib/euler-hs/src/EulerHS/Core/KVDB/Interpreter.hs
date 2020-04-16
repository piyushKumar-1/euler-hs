{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module EulerHS.Core.KVDB.Interpreter
  (
    -- * KVDB Interpreter
    runKVDB
  ) where

import           EulerHS.Prelude

import qualified Database.Redis             as R
import qualified EulerHS.Core.KVDB.Language as L
import           EulerHS.Core.Types.KVDB
import qualified Data.Map                   as Map

import qualified EulerHS.Core.Playback.Machine as P
import qualified EulerHS.Core.KVDB.Entries     as E
import qualified EulerHS.Core.Types            as D


interpretKeyValueF
  :: (forall b . R.Redis (Either R.Reply b) -> IO (Either KVDBReply b))
  -> D.RunMode
  -> L.KeyValueF (Either KVDBReply) a
  -> IO a
interpretKeyValueF runRedis runMode (L.Set k v next) =
  fmap next $ P.withRunMode runMode (E.mkSetEntry k v) $
    fmap (second fromRdStatus) $ runRedis $ R.set k v

interpretKeyValueF runRedis runMode (L.SetEx k e v next) =
  fmap next $ P.withRunMode runMode (E.mkSetExEntry k e v) $
    fmap (second fromRdStatus) $ runRedis $ R.setex k e v

interpretKeyValueF runRedis runMode (L.Get k next) =
  fmap next $ P.withRunMode runMode (E.mkGetEntry k) $
      runRedis $ R.get k

interpretKeyValueF runRedis runMode (L.Exists k next) =
  fmap next $ P.withRunMode runMode (E.mkExistsEntry k) $
      runRedis $ R.exists k

interpretKeyValueF _ runMode (L.Del [] next) =
  fmap next $ P.withRunMode runMode (E.mkDelEntry []) $
      pure $ pure 0

interpretKeyValueF runRedis runMode (L.Del ks next) =
  fmap next $ P.withRunMode runMode (E.mkDelEntry ks) $
      runRedis $ R.del ks

interpretKeyValueF runRedis runMode (L.Expire k sec next) =
  fmap next $ P.withRunMode runMode (E.mkExpireEntry k sec) $
      runRedis $ R.expire k sec

interpretKeyValueF runRedis runMode (L.Incr k next) =
  fmap next $ P.withRunMode runMode (E.mkIncrEntry k) $
      runRedis $ R.incr k

interpretKeyValueF runRedis runMode (L.HSet k field value next) =
  fmap next $ P.withRunMode runMode (E.mkHSetEntry k field value) $
      runRedis $ R.hset k field value

interpretKeyValueF runRedis runMode (L.HGet k field next) =
  fmap next $ P.withRunMode runMode (E.mkHGetEntry k field) $
      runRedis $ R.hget k field


interpretKeyValueTxF :: L.KeyValueF R.Queued a -> R.RedisTx a
interpretKeyValueTxF (L.Set k v next) =
  fmap next $ fmap (fmap D.fromRdStatus) $ R.set k v

interpretKeyValueTxF (L.SetEx k e v next) =
  fmap next $ fmap (fmap D.fromRdStatus) $ R.setex k e v

interpretKeyValueTxF (L.Get k next) =
  fmap next $ R.get k

interpretKeyValueTxF (L.Exists k next) =
  fmap next $ R.exists k

interpretKeyValueTxF (L.Del [] next) =
  fmap next $ return $ pure 0

interpretKeyValueTxF (L.Del ks next) =
  fmap next $ R.del ks

interpretKeyValueTxF (L.Expire k sec next) =
  fmap next $ R.expire k sec

interpretKeyValueTxF (L.Incr k next) =
  fmap next $ R.incr k

interpretKeyValueTxF (L.HSet k field value next) =
  fmap next $ R.hset k field value

interpretKeyValueTxF (L.HGet k field next) =
  fmap next $ R.hget k field


interpretTransactionF
  :: (forall b. R.Redis (Either R.Reply b) -> IO (Either KVDBReply b))
  -> D.RunMode
  -> L.TransactionF a
  -> IO a
interpretTransactionF runRedis runMode (L.MultiExec dsl next) =
  fmap next $ P.withRunMode runMode E.mkMultiExecEntry $
    runRedis $ fmap (Right . fromRdTxResult) $ R.multiExec $ foldF interpretKeyValueTxF dsl


interpretDbF
  :: (forall b. R.Redis (Either R.Reply b) -> IO (Either KVDBReply b))
  -> D.RunMode
  -> L.KVDBF a
  -> IO a
interpretDbF runRedis runMode (L.KV f) = interpretKeyValueF    runRedis runMode f
interpretDbF runRedis runMode (L.TX f) = interpretTransactionF runRedis runMode f


runKVDB :: Text -> D.RunMode -> MVar (Map Text NativeKVDBConn) -> L.KVDB a -> IO (Either KVDBReply a)
runKVDB cName runMode kvdbConnMapMVar =
  fmap (join . first exceptionToKVDBReply) . try @_ @SomeException .
    foldF (interpretDbF runRedis runMode) . runExceptT
  where
    runRedis :: R.Redis (Either R.Reply a) -> IO (Either KVDBReply a)
    runRedis redisDsl = do
      connections <- readMVar kvdbConnMapMVar
      case Map.lookup cName connections of
        Nothing   -> pure $ Left $ KVDBError KVDBConnectionDoesNotExist "Can't find redis connection"
        Just conn ->
          case conn of
            NativeKVDB c         -> fmap (first hedisReplyToKVDBReply) $ R.runRedis c redisDsl
            NativeKVDBMockedConn -> pure $ Right $
              error "Result of runRedis with mocked connection should not ever be evaluated"
