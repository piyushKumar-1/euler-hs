{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module EulerHS.Core.KVDB.Interpreter(runKVDB) where

import           EulerHS.Prelude

import qualified Database.Redis             as R
import qualified EulerHS.Core.KVDB.Language as L
import           EulerHS.Core.Types.KVDB

import Data.Generics.Product.Fields
import GHC.Generics
import GHC.TypeLits
import Type.Reflection (typeRep)
import Unsafe.Coerce

takeMockedVal ::forall (f :: Symbol) a r
  .  (KnownSymbol f, Typeable r, HasField' f r [a])
  => MVar r -> IO a
takeMockedVal mmv = do
  mv <- takeMVar mmv
  (v,t) <- case (getField @f mv) of
    [] -> error $ "empty " <> (show $ typeRep @f) <> " in " <> (show $ typeRep @r)
    (x:xs) -> pure (x,xs)
  putMVar mmv $ setField @f t mv
  pure v

interpretMockTX :: KVDBMockedValues -> L.TransactionF a -> IO a
interpretMockTX mv (L.MultiExec a next)= do
  (v :: R.TxResult Any) <- takeMockedVal @"kvdbTX" mv
  case v of
    R.TxAborted -> fmap next $ pure $ pure R.TxAborted
    R.TxSuccess a -> fmap next $ pure $ pure $ R.TxSuccess (unsafeCoerce  a)
    R.TxError str -> fmap next $ pure $ pure $ R.TxError str


interpretMockKV ::  KVDBMockedValues -> L.KeyValueF (Either R.Reply) a -> IO a

interpretMockKV mv (L.Exists k next) = do
  v <- takeMockedVal @"kvdbExists" mv
  fmap next $ pure $ Right True

interpretMockKV mv (L.Del x next) = do
  v <- takeMockedVal @"kvdbDel" mv
  fmap next $ pure $ Right v

interpretMockKV mv (L.Set k v next) = do
  x <- takeMockedVal @"kvdbSet" mv
  fmap next $ pure $ Right x

interpretMockKV mv (L.Get x next) = do
  v <- takeMockedVal @"kvdbGet" mv
  fmap next $ pure $ Right v

interpretMockKV mv (L.Expire k sec next) = do
  v <- takeMockedVal @"kvdbExpire" mv
  fmap next $ pure $ Right v

interpretMockKV mv (L.Incr x next) = do
  v <- takeMockedVal @"kvdbIncr" mv
  fmap next $ pure $ Right v

interpretMockKV mv (L.HSet k field value next) = do
  v <- takeMockedVal @"kvdbHSet" mv
  fmap next $ pure $ Right v

interpretMockKV mv (L.HGet k field next) = do
  v <- takeMockedVal @"kvdbHGet" mv
  fmap next $ pure $ Right v

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



interpretMockedConn :: KVDBMockedValues -> L.KVDBF a -> IO a
interpretMockedConn mv (L.KV f ) = interpretMockKV mv f
interpretMockedConn mv (L.TX f) = interpretMockTX mv f

runKVDB :: KVDBConn -> L.KVDB a -> IO (Either KVDBReply a)
runKVDB kvdbconn = case kvdbconn of
  Mocked mv -> fmap (first hedisReplyToKVDBReply) .foldF (interpretMockedConn mv) . runExceptT  --go
  Redis conn -> fmap (join . first exceptionToKVDBReply) . try @_ @SomeException .
            R.runRedis conn . fmap (first hedisReplyToKVDBReply) . foldF interpretDbF . runExceptT


