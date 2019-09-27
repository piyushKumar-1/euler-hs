{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module EulerHS.Core.KVDB.Language ( module X, KVDB, KVDBAnswer, KVDBMethod(..)
                                       , set, get, exists, del, expire, incr, hset, hget
                                       , publish) where

import           Database.Redis  as X (Message (..), PubSub, Reply (..),
                                       Status (..))
import           EulerHS.Prelude hiding (get)

type KVDBAnswer res = Either X.Reply res

data KVDBMethod next where
  Set :: ByteString -> ByteString -> (KVDBAnswer X.Status -> next) -> KVDBMethod next
  Get :: ByteString -> (KVDBAnswer (Maybe ByteString) -> next) -> KVDBMethod next
  Exists :: ByteString -> (KVDBAnswer Bool -> next) -> KVDBMethod next
  Del :: [ByteString] -> (KVDBAnswer Integer -> next) -> KVDBMethod next
  Expire :: ByteString -> Integer -> (KVDBAnswer Bool -> next) -> KVDBMethod next
  Incr :: ByteString -> (KVDBAnswer Integer -> next) -> KVDBMethod next
  HSet :: ByteString -> ByteString -> ByteString -> (KVDBAnswer Bool -> next) -> KVDBMethod next
  HGet :: ByteString -> ByteString -> (KVDBAnswer (Maybe ByteString) -> next) -> KVDBMethod next
  Publish :: ByteString -> ByteString -> (KVDBAnswer Integer -> next) -> KVDBMethod next
  -- Subscribe :: [ByteString] -> (X.PubSub -> next) -> KVDBMethod next
  -- Unsubscribe :: [ByteString] -> (X.PubSub -> next) -> KVDBMethod next
  -- SubHandle :: X.PubSub -> (X.Message -> IO X.PubSub) -> (() -> next) -> KVDBMethod next

instance Functor KVDBMethod where
  fmap f (Set k value next)            = Set k value (f . next)
  fmap f (Get k next)                  = Get k (f . next)
  fmap f (Exists k next)               = Exists k (f . next)
  fmap f (Del ks next)                 = Del ks (f . next)
  fmap f (Expire k sec next)           = Expire k sec (f . next)
  fmap f (Incr k next)                 = Incr k (f . next)
  fmap f (HSet k field value next)     = HSet k field value (f . next)
  fmap f (HGet k field next)           = HGet k field (f . next)
  fmap f (Publish chan msg next)       = Publish chan msg (f . next)
  -- fmap f (Subscribe chan next)         = Subscribe chan (f . next)
  -- fmap f (Unsubscribe chan next)       = Unsubscribe chan (f . next)
  -- fmap f (SubHandle sub callback next) = SubHandle sub callback (f . next)

type KVDB = F KVDBMethod

set :: ByteString -> ByteString -> KVDB (KVDBAnswer X.Status)
set key value = liftFC $ Set key value id

get :: ByteString -> KVDB (KVDBAnswer (Maybe ByteString))
get key = liftFC $ Get key id

exists :: ByteString -> KVDB (KVDBAnswer Bool)
exists key = liftFC $ Exists key id

del :: [ByteString] -> KVDB (KVDBAnswer Integer)
del ks = liftFC $ Del ks id

expire :: ByteString -> Integer -> KVDB (KVDBAnswer Bool)
expire key sec = liftFC $ Expire key sec id

incr :: ByteString -> KVDB (KVDBAnswer Integer)
incr key = liftFC $ Incr key id

hset :: ByteString -> ByteString -> ByteString -> KVDB (KVDBAnswer Bool)
hset key field value = liftFC $ HSet key field value id

hget :: ByteString -> ByteString -> KVDB (KVDBAnswer (Maybe ByteString))
hget key field = liftFC $ HGet key field id

publish :: ByteString -> ByteString -> KVDB (KVDBAnswer Integer)
publish chan msg = liftFC $ Publish chan msg id

-- subscribe :: [ByteString] -> KVDB X.PubSub
-- subscribe chan = liftFC $ Subscribe chan id
--
-- unsubscribe :: [ByteString] -> KVDB X.PubSub
-- unsubscribe chan = liftFC $ Unsubscribe chan id
--
-- subHandle :: X.PubSub -> (X.Message -> IO X.PubSub) -> KVDB ()
-- subHandle sub callback = liftFC $ SubHandle sub callback id
