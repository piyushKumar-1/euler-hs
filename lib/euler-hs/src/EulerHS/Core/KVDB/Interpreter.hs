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
import GHC.TypeLits
import Type.Reflection (typeRep)
import Unsafe.Coerce

import qualified EulerHS.Core.Playback.Machine as P
import qualified EulerHS.Core.KVDB.Entries as E
import qualified EulerHS.Core.Types         as D

takeMockedVal
  :: forall (f :: Symbol) a r
   . (KnownSymbol f, Typeable r, HasField' f r [a])
  => MVar r -> IO a
takeMockedVal mmv = do
  mv    <- takeMVar mmv
  (v,t) <- case (getField @f mv) of
    []     -> error $ "empty " <> (show $ typeRep @f) <> " in " <> (show $ typeRep @r)
    (x:xs) -> pure (x,xs)
  putMVar mmv $ setField @f t mv
  pure v


interpretMockTX :: KVDBMockedValues -> L.TransactionF a -> IO a
interpretMockTX mv (L.MultiExec a next)= do
  (v :: D.TxResult Any) <- takeMockedVal @"kvdbTX" mv
  case v of
    D.TxAborted   -> fmap next $ pure $ pure D.TxAborted
    D.TxSuccess a -> fmap next $ pure $ pure $ D.TxSuccess (unsafeCoerce  a)
    D.TxError str -> fmap next $ pure $ pure $ D.TxError str


interpretMockKV ::  KVDBMockedValues -> L.KeyValueF (Either KVDBReply) a -> IO a
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


interpretKeyValueF :: D.RunMode -> L.KeyValueF (Either KVDBReply) a -> R.Redis a
interpretKeyValueF runMode (L.Set k v next) =
  fmap next $ P.withRunMode runMode (E.mkSetEntry k v) $
    fmap (bimap hedisReplyToKVDBReply fromRdStatus) $
      R.set k v

interpretKeyValueF runMode (L.Get k next) =
  fmap next $ P.withRunMode runMode (E.mkGetEntry k) $
    fmap (first hedisReplyToKVDBReply) $
      R.get k

interpretKeyValueF runMode (L.Exists k next) =
  fmap next $ P.withRunMode runMode (E.mkExistsEntry k) $
    fmap (first hedisReplyToKVDBReply) $
      R.exists k

interpretKeyValueF runMode (L.Del [] next) =
  fmap next $ P.withRunMode runMode (E.mkDelEntry []) $
    fmap (first hedisReplyToKVDBReply) $
      pure $ pure 0

interpretKeyValueF runMode (L.Del ks next) =
  fmap next $ P.withRunMode runMode (E.mkDelEntry ks) $
    fmap (first hedisReplyToKVDBReply) $
      R.del ks

interpretKeyValueF runMode (L.Expire k sec next) =
  fmap next $ P.withRunMode runMode (E.mkExpireEntry k sec) $
    fmap (first hedisReplyToKVDBReply) $
      R.expire k sec

interpretKeyValueF runMode (L.Incr k next) =
  fmap next $ P.withRunMode runMode (E.mkIncrEntry k) $
    fmap (first hedisReplyToKVDBReply) $
      R.incr k

interpretKeyValueF runMode (L.HSet k field value next) =
  fmap next $ P.withRunMode runMode (E.mkHSetEntry k field value) $
    fmap (first hedisReplyToKVDBReply) $
      R.hset k field value

interpretKeyValueF runMode (L.HGet k field next) =
  fmap next $ P.withRunMode runMode (E.mkHGetEntry k field) $
    fmap (first hedisReplyToKVDBReply) $
      R.hget k field


interpretKeyValueTxF :: L.KeyValueF R.Queued a -> R.RedisTx a
interpretKeyValueTxF (L.Set k v next) =
  fmap next $ fmap (fmap D.fromRdStatus) $ R.set k v

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


interpretTransactionF :: D.RunMode -> L.TransactionF a -> R.Redis a
interpretTransactionF runMode (L.MultiExec dsl next) =
  fmap (next . Right) $ P.withRunMode runMode E.mkMultiExecEntry $
    fmap fromRdTxResult . R.multiExec $ foldF interpretKeyValueTxF dsl


interpretDbF :: D.RunMode -> L.KVDBF a -> R.Redis a
interpretDbF runMode (L.KV f) = interpretKeyValueF    runMode f
interpretDbF runMode (L.TX f) = interpretTransactionF runMode f


interpretMockedConn :: KVDBMockedValues -> L.KVDBF a -> IO a
interpretMockedConn mv (L.KV f ) = interpretMockKV mv f
interpretMockedConn mv (L.TX f) = interpretMockTX mv f


runKVDB :: D.RunMode -> KVDBConn -> L.KVDB a -> IO (Either KVDBReply a)
runKVDB runMode kvdbconn = case kvdbconn of
  Mocked mv  -> foldF (interpretMockedConn mv) . runExceptT
  Redis conn -> fmap (join . first exceptionToKVDBReply) . try @_ @SomeException .
    R.runRedis conn . foldF (interpretDbF runMode). runExceptT


