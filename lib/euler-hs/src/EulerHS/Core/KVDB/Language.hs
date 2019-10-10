{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module EulerHS.Core.KVDB.Language
  ( KVDB, KVDBTx, KVDBKey, KVDBValue, KVDBDuration, KVDBField, KVDBChannel, KVDBMessage
  , KVDBF(..), KeyValueF(..), TransactionF(..)
  , set, get, exists, del, expire, incr, hset, hget, multiExec
  , setTx, getTx, delTx
  ) where

import            EulerHS.Prelude hiding (get)
import qualified  Database.Redis  as R

type RawKVDBAnswer = Either R.Reply

type KVDBKey = ByteString
type KVDBValue = ByteString
type KVDBDuration = Integer
type KVDBField = ByteString
type KVDBChannel = ByteString
type KVDBMessage = ByteString

----------------------------------------------------------------------

data KeyValueF f next where
  Set :: KVDBKey -> KVDBValue -> (f R.Status -> next) -> KeyValueF f next
  Get :: KVDBKey -> (f (Maybe ByteString) -> next) -> KeyValueF f next
  Exists :: KVDBKey -> (f Bool -> next) -> KeyValueF f next
  Del :: [KVDBKey] -> (f Integer -> next) -> KeyValueF f next
  Expire :: KVDBKey -> KVDBDuration -> (f Bool -> next) -> KeyValueF f next
  Incr :: KVDBKey -> (f Integer -> next) -> KeyValueF f next
  HSet :: KVDBKey -> KVDBField -> KVDBValue -> (f Bool -> next) -> KeyValueF f next
  HGet :: KVDBKey -> KVDBField -> (f (Maybe ByteString) -> next) -> KeyValueF f next

instance Functor (KeyValueF f) where
  fmap f (Set k value next)            = Set k value (f . next)
  fmap f (Get k next)                  = Get k (f . next)
  fmap f (Exists k next)               = Exists k (f . next)
  fmap f (Del ks next)                 = Del ks (f . next)
  fmap f (Expire k sec next)           = Expire k sec (f . next)
  fmap f (Incr k next)                 = Incr k (f . next)
  fmap f (HSet k field value next)     = HSet k field value (f . next)
  fmap f (HGet k field next)           = HGet k field (f . next)

type KVDBTx = F (KeyValueF R.Queued)

----------------------------------------------------------------------

data TransactionF next where
  MultiExec
    :: KVDBTx (R.Queued a)
    -> (RawKVDBAnswer (R.TxResult a) -> next)
    -> TransactionF next

instance Functor TransactionF where
  fmap f (MultiExec dsl next) = MultiExec dsl (f . next)

----------------------------------------------------------------------

data KVDBF next
  = KV (KeyValueF RawKVDBAnswer next)
  | TX (TransactionF next)
  deriving Functor

type KVDB next = ExceptT R.Reply (F KVDBF) next

----------------------------------------------------------------------

setTx :: KVDBKey -> KVDBValue -> KVDBTx (R.Queued R.Status)
setTx key value = liftFC $ Set key value id

getTx :: KVDBKey -> KVDBTx (R.Queued (Maybe ByteString))
getTx key = liftFC $ Get key id

delTx :: [KVDBKey] -> KVDBTx (R.Queued Integer)
delTx ks = liftFC $ Del ks id

---

set :: KVDBKey -> KVDBValue -> KVDB R.Status
set key value = ExceptT $ liftFC $ KV $ Set key value id

get :: KVDBKey -> KVDB (Maybe ByteString)
get key = ExceptT $ liftFC $ KV $ Get key id

exists :: KVDBKey -> KVDB Bool
exists key = ExceptT $ liftFC $ KV $ Exists key id

del :: [KVDBKey] -> KVDB Integer
del ks = ExceptT $ liftFC $ KV $ Del ks id

expire :: KVDBKey -> KVDBDuration -> KVDB Bool
expire key sec = ExceptT $ liftFC $ KV $ Expire key sec id

incr :: KVDBKey -> KVDB Integer
incr key = ExceptT $ liftFC $ KV $ Incr key id

hset :: KVDBKey -> KVDBField -> KVDBValue -> KVDB Bool
hset key field value = ExceptT $ liftFC $ KV $ HSet key field value id

hget :: KVDBKey -> KVDBField -> KVDB (Maybe ByteString)
hget key field = ExceptT $ liftFC $ KV $ HGet key field id

multiExec :: KVDBTx (R.Queued a) -> KVDB (R.TxResult a)
multiExec kvtx = ExceptT $ liftFC $ TX $ MultiExec kvtx id
