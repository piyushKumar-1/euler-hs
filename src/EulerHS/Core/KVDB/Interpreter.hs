{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE TypeFamilies        #-}
module EulerHS.Core.KVDB.Interpreter
  (
    -- * KVDB Interpreter
    runKVDB
  ) where

import           EulerHS.Prelude

import qualified Data.Map as Map
import qualified Database.Redis as R
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified EulerHS.Core.KVDB.Language as L
import           EulerHS.Core.Types.KVDB

import qualified EulerHS.Core.KVDB.Entries as E
import qualified EulerHS.Core.Playback.Machine as P
import qualified EulerHS.Core.Types as D


interpretKeyValueF
  :: (forall b . R.Redis (Either R.Reply b) -> IO (Either KVDBReply b))
  -- -> D.RunMode
  -> L.KeyValueF (Either KVDBReply) a
  -> IO a
interpretKeyValueF runRedis (L.Set k v next) =
  -- fmap next $ P.withRunMode runMode (E.mkSetEntry k v) $
  fmap next $
    fmap (second fromRdStatus) $ runRedis $ R.set k v

interpretKeyValueF runRedis (L.SetEx k e v next) =
  -- fmap next $ P.withRunMode runMode (E.mkSetExEntry k e v) $
  fmap next $
    fmap (second fromRdStatus) $ runRedis $ R.setex k e v

interpretKeyValueF runRedis (L.SetOpts k v ttl cond next) =
  -- fmap next $ P.withRunMode runMode (E.mkSetOptsEntry k v ttl cond) $ do
  fmap next $ do
    result <- runRedis $ R.setOpts k v (makeSetOpts ttl cond)
    pure $ case result of
      Right _ -> Right True
      -- (nil) is ok, app should not fail
      Left (Bulk Nothing) -> Right False
      Left reply -> Left reply

interpretKeyValueF runRedis (L.Get k next) =
  -- fmap next $ P.withRunMode runMode (E.mkGetEntry k) $
  fmap next $
    runRedis $ R.get k

interpretKeyValueF runRedis (L.Exists k next) =
  -- fmap next $ P.withRunMode runMode (E.mkExistsEntry k) $
  fmap next $
    runRedis $ R.exists k

interpretKeyValueF _ (L.Del [] next) =
  -- fmap next $ P.withRunMode runMode (E.mkDelEntry []) $
  fmap next $
    pure $ pure 0

interpretKeyValueF runRedis (L.Del ks next) =
  -- fmap next $ P.withRunMode runMode (E.mkDelEntry ks) $
  fmap next $
    runRedis $ R.del ks

interpretKeyValueF runRedis (L.Expire k sec next) =
  -- fmap next $ P.withRunMode runMode (E.mkExpireEntry k sec) $
  fmap next $
    runRedis $ R.expire k sec

interpretKeyValueF runRedis (L.Incr k next) =
  -- fmap next $ P.withRunMode runMode (E.mkIncrEntry k) $
  fmap next $
    runRedis $ R.incr k

interpretKeyValueF runRedis (L.HSet k field value next) =
  -- fmap next $ P.withRunMode runMode (E.mkHSetEntry k field value) $
  fmap next $
    runRedis $ R.hset k field value

interpretKeyValueF runRedis (L.HGet k field next) =
  -- fmap next $ P.withRunMode runMode (E.mkHGetEntry k field) $
  fmap next $
    runRedis $ R.hget k field

interpretKeyValueF runRedis (L.XAdd stream entryId items next) =
  -- fmap next $ P.withRunMode runMode (E.mkXAddEntry stream entryId items) $
  fmap next $
    runRedis $ do
      result <- R.xadd stream (makeStreamEntryId entryId) items
      pure $ parseStreamEntryId <$> result
  where
    makeStreamEntryId (L.EntryID (L.KVDBStreamEntryID ms seq)) = show ms <> "-" <> show seq
    makeStreamEntryId L.AutoID = "*"

    parseStreamEntryId bs =
      -- "number-number" is redis entry id invariant
      let [ms, seq] = read . T.unpack <$> T.splitOn "-" (TE.decodeUtf8 bs)
      in L.KVDBStreamEntryID ms seq

interpretKeyValueF runRedis (L.XLen stream next) =
  -- fmap next $ P.withRunMode runMode (E.mkXLenEntry stream) $
  fmap next $
    runRedis $ R.xlen stream


interpretKeyValueTxF :: L.KeyValueF R.Queued a -> R.RedisTx a
interpretKeyValueTxF (L.Set k v next) =
  fmap next $ fmap (fmap D.fromRdStatus) $ R.set k v

interpretKeyValueTxF (L.SetEx k e v next) =
  fmap next $ fmap (fmap D.fromRdStatus) $ R.setex k e v

interpretKeyValueTxF (L.SetOpts k v ttl cond next) =
  fmap next $ fmap (fmap rdStatusToBool) $ R.setOpts k v (makeSetOpts ttl cond)
    where
      rdStatusToBool R.Ok = True
      rdStatusToBool _ = False

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

interpretKeyValueTxF (L.XLen stream next) =
  fmap next $ R.xlen stream

interpretKeyValueTxF (L.XAdd stream entryId items next) =
  fmap next $ fmap (fmap parseStreamEntryId) $ R.xadd stream (makeStreamEntryId entryId) items
  where
    makeStreamEntryId (L.EntryID (L.KVDBStreamEntryID ms seq)) = show ms <> "-" <> show seq
    makeStreamEntryId L.AutoID = "*"

    parseStreamEntryId bs =
      -- "number-number" is redis entry id invariant
      let [ms, seq] = read . T.unpack <$> T.splitOn "-" (TE.decodeUtf8 bs)
      in L.KVDBStreamEntryID ms seq



interpretTransactionF
  :: (forall b. R.Redis (Either R.Reply b) -> IO (Either KVDBReply b))
  -- -> D.RunMode
  -> L.TransactionF a
  -> IO a
interpretTransactionF runRedis (L.MultiExec dsl next) =
  -- fmap next $ P.withRunMode runMode E.mkMultiExecEntry $
  fmap next $
    runRedis $ fmap (Right . fromRdTxResult) $ R.multiExec $ foldF interpretKeyValueTxF dsl

interpretTransactionF runRedis (L.MultiExecWithHash h dsl next) =
  -- fmap next $ P.withRunMode runMode (E.mkMultiExecWithHashEntry h) $
  fmap next $
    runRedis $ fmap (Right . fromRdTxResult) $ R.multiExecWithHash h $ foldF interpretKeyValueTxF dsl


interpretDbF
  :: (forall b. R.Redis (Either R.Reply b) -> IO (Either KVDBReply b))
  -- -> D.RunMode
  -> L.KVDBF a
  -> IO a
interpretDbF runRedis (L.KV f) = interpretKeyValueF    runRedis f
interpretDbF runRedis (L.TX f) = interpretTransactionF runRedis f


runKVDB :: Text -> MVar (Map Text NativeKVDBConn) -> L.KVDB a -> IO (Either KVDBReply a)
runKVDB cName kvdbConnMapMVar =
  fmap (join . first exceptionToKVDBReply) . try @_ @SomeException .
    foldF (interpretDbF runRedis) . runExceptT
  where
    runRedis :: R.Redis (Either R.Reply a) -> IO (Either KVDBReply a)
    runRedis redisDsl = do
      connections <- readMVar kvdbConnMapMVar
      case Map.lookup cName connections of
        Nothing   -> pure $ Left $ KVDBError KVDBConnectionDoesNotExist "Can't find redis connection"
        Just conn ->
          case conn of
            NativeKVDB c         -> fmap (first hedisReplyToKVDBReply) $ R.runRedis c redisDsl
            NativeKVDBMockedConn -> pure $ Right $
              error "Result of runRedis with mocked connection should not ever be evaluated"


makeSetOpts :: L.KVDBSetTTLOption -> L.KVDBSetConditionOption -> R.SetOpts
makeSetOpts ttl cond =
  R.SetOpts
    { setSeconds =
        case ttl of
          L.Seconds s -> Just s
          _ -> Nothing
    , setMilliseconds =
        case ttl of
          L.Milliseconds ms -> Just ms
          _ -> Nothing
    , setCondition =
        case cond of
          L.SetAlways -> Nothing
          L.SetIfExist -> Just R.Xx
          L.SetIfNotExist -> Just R.Nx
    }
