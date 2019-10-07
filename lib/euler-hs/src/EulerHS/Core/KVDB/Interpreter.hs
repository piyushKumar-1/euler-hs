module EulerHS.Core.KVDB.Interpreter where

import           EulerHS.Prelude

import qualified Database.Redis             as R
import qualified EulerHS.Core.KVDB.Language as L
import           EulerHS.Core.Types.KVDB
-- import           EulerHS.Prelude

interpretKVDBMethod :: L.KVDBMethod b -> ExceptT R.Reply R.Redis b
interpretKVDBMethod (L.Set k v next) =
  fmap next $ ExceptT $ R.set k v

interpretKVDBMethod (L.Get k next) =
  fmap next $ ExceptT $ R.get k

interpretKVDBMethod (L.Exists k next) =
  fmap next $ ExceptT $ R.exists k

interpretKVDBMethod (L.Del ks next) =
  fmap next $ ExceptT $ R.del ks

interpretKVDBMethod (L.Expire k sec next) =
  fmap next $ ExceptT $ R.expire k sec

interpretKVDBMethod (L.Incr k next) =
  fmap next $ ExceptT $ R.incr k

interpretKVDBMethod (L.HSet k field value next) =
  fmap next $ ExceptT $ R.hset k field value

interpretKVDBMethod (L.HGet k field next) =
  fmap next $ ExceptT $ R.hget k field

interpretKVDBMethod (L.Publish chan msg next) =
  fmap next $ ExceptT $ R.publish chan msg

------
-- interpretKVDBMethod _ (L.Subscribe chan next) = do
--   next <$> (pure $ RD.subscribe chan)
--
-- interpretKVDBMethod _ (L.Unsubscribe chan next) = do
--   next <$> (pure $ RD.unsubscribe chan)
--
-- interpretKVDBMethod connection (L.SubHandle sub callback next) = do
--   next <$> (RD.runRedis connection $ RD.pubSub sub callback)

runKVDB :: R.Connection -> L.KVDB a -> IO (Either KVDBReply a)
runKVDB conn = fmap (join . first exceptionToKVDBReply) . try @_ @SomeException .
  R.runRedis conn . fmap (first hedisReplyToKVDBReplyMono) . runExceptT . foldF interpretKVDBMethod
