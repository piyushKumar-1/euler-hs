{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeApplications          #-}


module EulerHS.Testing.KVDB.Interpreter where

import           EulerHS.Prelude -- hiding (view, (^.))
import           Data.Aeson                      (encode, decode)
import Servant.Client (ClientError(..))
import qualified EulerHS.Language as L
-- import qualified EulerHS.Runtime as R
-- import qualified EulerHS.Interpreters as R
import qualified  Database.Redis  as R

import EulerHS.Testing.Types
import Unsafe.Coerce

import Control.Lens
import Data.Generics.Product.Fields
import GHC.Generics
import GHC.TypeLits
import Type.Reflection (typeRep)

-- import qualified EulerHS.Core.KVDB.Language as L
import           EulerHS.Types

-- interpretKeyValue_F :: (Applicative f) => KVDBMockedValues -> L.KeyValueF f a -> IO a
-- interpretKeyValue_F mv (L.Set k v next) = do
--   val <-   takeMockedVal @"kvdbSet" mv
--   next <$> (pure val) --R.set k v
-- 
-- interpretKeyValue_F mv (L.Get k next) = do
--   val <- takeMockedVal @"kvdbGet" mv
--   fmap next $ pure val --R.get k
-- 
-- interpretKeyValue_F mv (L.Exists k next) = do
--   val <- takeMockedVal @"kvdbExists" mv
--   fmap next $ pure val -- R.exists k
-- 
-- -- interpretKeyValue_F mv (L.Del [] next) =
-- --   fmap next $ return $ pure 0
-- -- 
-- -- interpretKeyValue_F mv (L.Del ks next) =
-- --   fmap next $ R.del ks
-- -- 
-- -- interpretKeyValue_F mv (L.Expire k sec next) =
-- --   fmap next $ R.expire k sec
-- -- 
-- -- interpretKeyValue_F mv (L.Incr k next) =
-- --   fmap next $ R.incr k
-- -- 
-- -- interpretKeyValue_F mv (L.HSet k field value next) =
-- --   fmap next $ R.hset k field value
-- -- 
-- -- interpretKeyValue_F mv (L.HGet k field next) =
-- --   fmap next $ R.hget k field
-- 
-- interpretTransactionF :: KVDBMockedValues -> L.TransactionF a -> R.Redis a
-- interpretTransactionF mv (L.MultiExec dsl next) =
--   fmap (next . Right) $ R.multiExec $ foldF (interpretKeyValueTxF mv) dsl
-- 
-- interpretKeyValueF :: KVDBMockedValues -> L.KeyValueF (Either R.Reply) a -> R.Redis a
-- interpretKeyValueF = interpretKeyValue_F
-- 
-- interpretKeyValueTxF :: KVDBMockedValues -> L.KeyValueF R.Queued a -> R.RedisTx a
-- interpretKeyValueTxF = interpretKeyValue_F
-- 
-- interpretDbF :: KVDBMockedValues -> L.KVDBF a -> R.Redis a
-- interpretDbF mv (L.KV f) = interpretKeyValueF mv f
-- interpretDbF mv (L.TX f) = interpretTransactionF mv f
-- 
-- runKVDB :: KVDBMockedValues -> R.Connection -> L.KVDB a -> IO (Either KVDBReply a)
-- runKVDB mv conn =
--   fmap (join . first exceptionToKVDBReply) . try @_ @SomeException .
--     R.runRedis conn . fmap (first hedisReplyToKVDBReply) . foldF (interpretDbF mv) . runExceptT
-- 
-- takeMockedVal ::forall (f :: Symbol) a r
--   .  (KnownSymbol f, Typeable r, HasField' f r [a])
--   => MVar r -> IO a
-- takeMockedVal mmv = do
--   mv <- takeMVar mmv
--   (v,t) <- case (getField @f mv) of
--     [] -> EulerHS.Prelude.error $ "empty " <> (show $ typeRep @f) <> " in " <> (show $ typeRep @r)
--     (x:xs) -> pure (x,xs)
--   putMVar mmv $ setField @f t mv
--   pure v