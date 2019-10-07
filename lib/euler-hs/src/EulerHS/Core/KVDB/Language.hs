{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module EulerHS.Core.KVDB.Language
  ( KVDB
  , KVDBAnswer
  , KVDBMethod(..)
  , KVDBKey, KVDBValue, KVDBDuration, KVDBField , KVDBChannel, KVDBMessage
  , set, get, exists, del, expire, incr, hset, hget , publish
  ) where

import            EulerHS.Prelude hiding (get)
import qualified  Database.Redis  as R
import            EulerHS.Core.Types.KVDB

type KVDBAnswer res = Either KVDBReply res

type KVDBKey = ByteString
type KVDBValue = ByteString
type KVDBDuration = Integer
type KVDBField = ByteString
type KVDBChannel = ByteString
type KVDBMessage = ByteString

data KVDBMethod next where
  Set :: KVDBKey -> KVDBValue -> (R.Status -> next) -> KVDBMethod next
  Get :: KVDBKey -> (Maybe ByteString -> next) -> KVDBMethod next
  Exists :: KVDBKey -> (Bool -> next) -> KVDBMethod next
  Del :: [KVDBKey] -> (Integer -> next) -> KVDBMethod next
  Expire :: KVDBKey -> KVDBDuration -> (Bool -> next) -> KVDBMethod next
  Incr :: KVDBKey -> (Integer -> next) -> KVDBMethod next
  HSet :: KVDBKey -> KVDBField -> KVDBValue -> (Bool -> next) -> KVDBMethod next
  HGet :: KVDBKey -> KVDBField -> (Maybe ByteString -> next) -> KVDBMethod next
  Publish :: KVDBChannel -> KVDBMessage -> (Integer -> next) -> KVDBMethod next
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

set :: KVDBKey -> KVDBValue -> KVDB R.Status
set key value = liftFC $ Set key value id

get :: KVDBKey -> KVDB (Maybe ByteString)
get key = liftFC $ Get key id

exists :: KVDBKey -> KVDB Bool
exists key = liftFC $ Exists key id

del :: [KVDBKey] -> KVDB Integer
del ks = liftFC $ Del ks id

expire :: KVDBKey -> KVDBDuration -> KVDB Bool
expire key sec = liftFC $ Expire key sec id

incr :: KVDBKey -> KVDB Integer
incr key = liftFC $ Incr key id

hset :: KVDBKey -> KVDBField -> KVDBValue -> KVDB Bool
hset key field value = liftFC $ HSet key field value id

hget :: KVDBKey -> KVDBField -> KVDB (Maybe ByteString)
hget key field = liftFC $ HGet key field id

publish :: KVDBChannel -> KVDBMessage -> KVDB Integer
publish chan msg = liftFC $ Publish chan msg id

-- subscribe :: [ByteString] -> KVDB X.PubSub
-- subscribe chan = liftFC $ Subscribe chan id
--
-- unsubscribe :: [ByteString] -> KVDB X.PubSub
-- unsubscribe chan = liftFC $ Unsubscribe chan id
--
-- subHandle :: X.PubSub -> (X.Message -> IO X.PubSub) -> KVDB ()
-- subHandle sub callback = liftFC $ SubHandle sub callback id
