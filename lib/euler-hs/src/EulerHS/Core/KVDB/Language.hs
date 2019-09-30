{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module EulerHS.Core.KVDB.Language ( module X, KVDB, KVDBAnswer, KVDBMethod(..)
                                       , KVDBKey, KVDBValue, KVDBDuration, KVDBField
                                       , KVDBChannel, KVDBMessage
                                       , set, get, exists, del, expire, incr, hset, hget
                                       , publish) where

import           Database.Redis  as X (Message (..), PubSub, Reply (..),
                                       Status (..))
import           EulerHS.Prelude hiding (get)

type KVDBAnswer res = Either X.Reply res

type KVDBKey = ByteString
type KVDBValue = ByteString
type KVDBDuration = Integer
type KVDBField = ByteString
type KVDBChannel = ByteString
type KVDBMessage = ByteString

data KVDBMethod next where
  Set :: KVDBKey -> KVDBValue -> (KVDBAnswer X.Status -> next) -> KVDBMethod next
  Get :: KVDBKey -> (KVDBAnswer (Maybe ByteString) -> next) -> KVDBMethod next
  Exists :: KVDBKey -> (KVDBAnswer Bool -> next) -> KVDBMethod next
  Del :: [KVDBKey] -> (KVDBAnswer Integer -> next) -> KVDBMethod next
  Expire :: KVDBKey -> KVDBDuration -> (KVDBAnswer Bool -> next) -> KVDBMethod next
  Incr :: KVDBKey -> (KVDBAnswer Integer -> next) -> KVDBMethod next
  HSet :: KVDBKey -> KVDBField -> KVDBValue -> (KVDBAnswer Bool -> next) -> KVDBMethod next
  HGet :: KVDBKey -> KVDBField -> (KVDBAnswer (Maybe ByteString) -> next) -> KVDBMethod next
  Publish :: KVDBChannel -> KVDBMessage -> (KVDBAnswer Integer -> next) -> KVDBMethod next
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

set :: KVDBKey -> KVDBValue -> KVDB (KVDBAnswer X.Status)
set key value = liftFC $ Set key value id

get :: KVDBKey -> KVDB (KVDBAnswer (Maybe ByteString))
get key = liftFC $ Get key id

exists :: KVDBKey -> KVDB (KVDBAnswer Bool)
exists key = liftFC $ Exists key id

del :: [KVDBKey] -> KVDB (KVDBAnswer Integer)
del ks = liftFC $ Del ks id

expire :: KVDBKey -> KVDBDuration -> KVDB (KVDBAnswer Bool)
expire key sec = liftFC $ Expire key sec id

incr :: KVDBKey -> KVDB (KVDBAnswer Integer)
incr key = liftFC $ Incr key id

hset :: KVDBKey -> KVDBField -> KVDBValue -> KVDB (KVDBAnswer Bool)
hset key field value = liftFC $ HSet key field value id

hget :: KVDBKey -> KVDBField -> KVDB (KVDBAnswer (Maybe ByteString))
hget key field = liftFC $ HGet key field id

publish :: KVDBChannel -> KVDBMessage -> KVDB (KVDBAnswer Integer)
publish chan msg = liftFC $ Publish chan msg id

-- subscribe :: [ByteString] -> KVDB X.PubSub
-- subscribe chan = liftFC $ Subscribe chan id
--
-- unsubscribe :: [ByteString] -> KVDB X.PubSub
-- unsubscribe chan = liftFC $ Unsubscribe chan id
--
-- subHandle :: X.PubSub -> (X.Message -> IO X.PubSub) -> KVDB ()
-- subHandle sub callback = liftFC $ SubHandle sub callback id
