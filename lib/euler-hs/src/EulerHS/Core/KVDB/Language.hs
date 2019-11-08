{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module EulerHS.Core.KVDB.Language
  ( KVDB, KVDBTx, KVDBKey, KVDBValue, KVDBDuration, KVDBField, KVDBChannel, KVDBMessage
  , KVDBF(..), KeyValueF(..), TransactionF(..)
  , set, get, exists, del, expire, incr, hset, hget, multiExec
  , setTx, getTx, delTx
  ) where

import            EulerHS.Prelude hiding (get)
import qualified Database.Redis     as R
import qualified EulerHS.Core.Types as T

type KVDBKey = ByteString
type KVDBValue = ByteString
type KVDBDuration = Integer
type KVDBField = ByteString
type KVDBChannel = ByteString
type KVDBMessage = ByteString

----------------------------------------------------------------------

data KeyValueF f next where
  Set :: KVDBKey -> KVDBValue -> (f T.KVDBStatus -> next) -> KeyValueF f next
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
    :: T.JSONEx a
    => KVDBTx (R.Queued a)
    -> (T.KVDBAnswer (T.TxResult a) -> next)
    -> TransactionF next

instance Functor TransactionF where
  fmap f (MultiExec dsl next) = MultiExec dsl (f . next)

----------------------------------------------------------------------

data KVDBF next
  = KV (KeyValueF T.KVDBAnswer next)
  | TX (TransactionF next)
  deriving Functor

type KVDB next = ExceptT T.KVDBReply (F KVDBF) next

----------------------------------------------------------------------
-- | Set the value of a key. Transaction version.
setTx :: KVDBKey -> KVDBValue -> KVDBTx (R.Queued T.KVDBStatus)
setTx key value = liftFC $ Set key value id

-- | Get the value of a key. Transaction version.
getTx :: KVDBKey -> KVDBTx (R.Queued (Maybe ByteString))
getTx key = liftFC $ Get key id

-- | Delete a keys. Transaction version.
delTx :: [KVDBKey] -> KVDBTx (R.Queued Integer)
delTx ks = liftFC $ Del ks id

---
-- | Set the value of a key
set :: KVDBKey -> KVDBValue -> KVDB T.KVDBStatus
set key value = ExceptT $ liftFC $ KV $ Set key value id

-- | Get the value of a key
get :: KVDBKey -> KVDB (Maybe ByteString)
get key = ExceptT $ liftFC $ KV $ Get key id

-- | Determine if a key exists
exists :: KVDBKey -> KVDB Bool
exists key = ExceptT $ liftFC $ KV $ Exists key id

-- | Delete a keys
del :: [KVDBKey] -> KVDB Integer
del ks = ExceptT $ liftFC $ KV $ Del ks id

-- | Set a key's time to live in seconds
expire :: KVDBKey -> KVDBDuration -> KVDB Bool
expire key sec = ExceptT $ liftFC $ KV $ Expire key sec id

-- | Increment the integer value of a key by one
incr :: KVDBKey -> KVDB Integer
incr key = ExceptT $ liftFC $ KV $ Incr key id

-- | Set the value of a hash field
hset :: KVDBKey -> KVDBField -> KVDBValue -> KVDB Bool
hset key field value = ExceptT $ liftFC $ KV $ HSet key field value id

-- | Get the value of a hash field
hget :: KVDBKey -> KVDBField -> KVDB (Maybe ByteString)
hget key field = ExceptT $ liftFC $ KV $ HGet key field id

-- | Run commands inside a transaction.
multiExec :: T.JSONEx a => KVDBTx (R.Queued a) -> KVDB (T.TxResult a)
multiExec kvtx = ExceptT $ liftFC $ TX $ MultiExec kvtx id
