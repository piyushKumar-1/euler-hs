module EulerHS.Core.KVDB.Interpreter(runKVDB) where

import           EulerHS.Prelude

import qualified Database.Redis             as R
import qualified EulerHS.Core.KVDB.Language as L
import           EulerHS.Core.Types.KVDB

interpretKeyValue_F :: (Applicative f, R.RedisCtx m f) => L.KeyValueF f a -> m a
interpretKeyValue_F (L.Set k v next) =
  fmap next $ R.set k v

interpretKeyValue_F (L.Get k next) =
  fmap next $ R.get k

interpretKeyValue_F (L.Exists k next) =
  fmap next $ R.exists k

interpretKeyValue_F (L.Del [] next) =
  fmap next $ return $ pure 0

interpretKeyValue_F (L.Del ks next) =
  fmap next $ R.del ks

interpretKeyValue_F (L.Expire k sec next) =
  fmap next $ R.expire k sec

interpretKeyValue_F (L.Incr k next) =
  fmap next $ R.incr k

interpretKeyValue_F (L.HSet k field value next) =
  fmap next $ R.hset k field value

interpretKeyValue_F (L.HGet k field next) =
  fmap next $ R.hget k field

interpretTransactionF :: L.TransactionF a -> R.Redis a
interpretTransactionF (L.MultiExec dsl next) =
  fmap (next . Right) $ R.multiExec $ foldF interpretKeyValueTxF dsl

interpretKeyValueF :: L.KeyValueF (Either R.Reply) a -> R.Redis a
interpretKeyValueF = interpretKeyValue_F

interpretKeyValueTxF :: L.KeyValueF R.Queued a -> R.RedisTx a
interpretKeyValueTxF = interpretKeyValue_F

interpretDbF :: L.KVDBF a -> R.Redis a
interpretDbF (L.KV f) = interpretKeyValueF f
interpretDbF (L.TX f) = interpretTransactionF f

runKVDB :: R.Connection -> L.KVDB a -> IO (Either KVDBReply a)
runKVDB conn =
  fmap (join . first exceptionToKVDBReply) . try @_ @SomeException .
    R.runRedis conn . fmap (first hedisReplyToKVDBReply) . foldF interpretDbF . runExceptT


