module EulerHS.Core.KVDB.Interpreter where

import qualified Database.Redis             as RD
import qualified EulerHS.Core.KVDB.Language as L
import           EulerHS.Prelude

interpretKVDBMethod :: RD.Connection -> L.KVDBMethod b -> IO b

interpretKVDBMethod connection (L.Set k v next) = do
  next <$> (RD.runRedis connection $ RD.set k v)

interpretKVDBMethod connection (L.Get k next) = do
  next <$> (RD.runRedis connection $ RD.get k)

interpretKVDBMethod connection (L.Exists k next) = do
  next <$> (RD.runRedis connection $ RD.exists k)

interpretKVDBMethod connection (L.Del ks next) = do
  next <$> (RD.runRedis connection $ RD.del ks)

interpretKVDBMethod connection (L.Expire k sec next) = do
  next <$> (RD.runRedis connection $ RD.expire k sec)

interpretKVDBMethod connection (L.Incr k next) = do
  next <$> (RD.runRedis connection $ RD.incr k)

interpretKVDBMethod connection (L.HSet k field value next) = do
  next <$> (RD.runRedis connection $ RD.hset k field value)

interpretKVDBMethod connection (L.HGet k field next) = do
  next <$> (RD.runRedis connection $ RD.hget k field)

interpretKVDBMethod connection (L.Publish chan msg next) = do
  next <$> (RD.runRedis connection $ RD.publish chan msg)

interpretKVDBMethod _ (L.Subscribe chan next) = do
  next <$> (pure $ RD.subscribe chan)

interpretKVDBMethod _ (L.Unsubscribe chan next) = do
  next <$> (pure $ RD.unsubscribe chan)

interpretKVDBMethod connection (L.SubHandle sub callback next) = do
  next <$> (RD.runRedis connection $ RD.pubSub sub callback)

runKVDB :: RD.Connection -> L.KVDB b -> IO b
runKVDB conn = foldF (interpretKVDBMethod conn)
