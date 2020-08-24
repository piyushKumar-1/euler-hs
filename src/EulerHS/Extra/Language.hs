module EulerHS.Extra.Language
  ( getOrInitSqlConn
  , getOrInitKVDBConn
  , rExpire
  , rExpireB
  , rDel
  , rDelB
  , rExists
  , rHget
  , rHgetB
  , rHset
  , rIncr
  , rSet
  , rSetT
  , rSetB
  , rGet
  , rGetB
  , rGetT
  , rSetex
  , rSetexB
  , rSetexT
  , keyToSlot
  ) where

import           EulerHS.Prelude hiding (get, id)

import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text.Encoding as TE
import           Database.Redis (keyToSlot)
import qualified EulerHS.Core.KVDB.Language as L
import qualified EulerHS.Core.Types as T
import qualified EulerHS.Framework.Language as L

-- | Get existing SQL connection, or init a new connection.
getOrInitSqlConn :: (L.MonadFlow m) =>
  T.DBConfig beM -> m (T.DBResult (T.SqlConn beM))
getOrInitSqlConn cfg = do
  eConn <- L.getSqlDBConnection cfg
  case eConn of
    Left (T.DBError T.ConnectionDoesNotExist _) -> L.initSqlDBConnection cfg
    res                                         -> pure res

-- | Get existing Redis connection, or init a new connection.
getOrInitKVDBConn :: (L.MonadFlow m) => T.KVDBConfig -> m (T.KVDBAnswer T.KVDBConn)
getOrInitKVDBConn cfg = do
  conn <- L.getKVDBConnection cfg
  case conn of
    Left (T.KVDBError T.KVDBConnectionDoesNotExist _) -> L.initKVDBConnection cfg
    res -> pure res

-- KVDB helpers

rExpire :: (ToJSON k, Integral t, L.MonadFlow m) =>
  Text -> k -> t -> m (Either T.KVDBReply Bool)
rExpire cName k t = do
  res <- L.runKVDB cName $ L.expire (BSL.toStrict $ A.encode k) (toInteger t)
  case res of
    Right _ -> do
      -- L.logInfo @Text "Redis expire" $ show r
      pure res
    Left err -> do
      L.logError @Text "Redis expire" $ show err
      pure res

rExpireB :: (L.MonadFlow m) =>
  Text -> ByteString -> Integer -> m (Either T.KVDBReply Bool)
rExpireB cName k t = do
  res <- L.runKVDB cName $ L.expire k t
  case res of
    Right _ -> do
      -- L.logInfo @Text "Redis expire" $ show r
      pure res
    Left err -> do
      L.logError @Text "Redis expire" $ show err
      pure res

rDel :: (ToJSON k, L.MonadFlow m) =>
  Text -> [k] -> m (Either T.KVDBReply Integer)
rDel cName ks = do
  res <- L.runKVDB cName $ L.del (BSL.toStrict . A.encode <$> ks)
  case res of
    Right _ -> do
      -- L.logInfo @Text "Redis del" $ show r
      pure res
    Left err -> do
      L.logError @Text "Redis del" $ show err
      pure res

rDelB :: (L.MonadFlow m) =>
  Text -> [ByteString] -> m (Either T.KVDBReply Integer)
rDelB cName ks = do
  res <- L.runKVDB cName $ L.del ks
  case res of
    Right _ -> do
      -- L.logInfo @Text "Redis del" $ show r
      pure res
    Left err -> do
      L.logError @Text "Redis del" $ show err
      pure res

rExists :: (ToJSON k, L.MonadFlow m) =>
  Text -> k -> m (Either T.KVDBReply Bool)
rExists cName k = do
  res <- L.runKVDB cName $ L.exists (BSL.toStrict $ A.encode k)
  case res of
    Right _ -> do
      -- L.logInfo @Text "Redis exists" $ show r
      pure res
    Left err -> do
      L.logError @Text "Redis exists" $ show err
      pure res

rHget :: (ToJSON k, ToJSON f, FromJSON v, L.MonadFlow m)
  => Text -> k -> f -> m (Maybe v)
rHget cName k f = do
  res <- L.runKVDB cName $
    L.hget (BSL.toStrict $ A.encode k)
           (BSL.toStrict $ A.encode f)
  case res of
    Right (Just val) -> pure $ A.decode $ BSL.fromStrict val
    Right Nothing -> pure Nothing
    Left err -> do
      L.logError @Text "Redis hget" $ show err
      pure Nothing

rHgetB :: (L.MonadFlow m) =>
  Text -> ByteString -> ByteString -> m (Maybe ByteString)
rHgetB cName k f = do
  res <- L.runKVDB cName $ L.hget k f
  case res of
    Right (Just val) -> pure $ Just val
    Right Nothing -> pure Nothing
    Left err -> do
      L.logError @Text "Redis hget" $ show err
      pure Nothing

rHset :: (ToJSON k, ToJSON f, ToJSON v, L.MonadFlow m)
  => Text -> k -> f -> v -> m (Either T.KVDBReply Bool)
rHset cName k f v = do
  res <- L.runKVDB cName $
    L.hset (BSL.toStrict $ A.encode k)
           (BSL.toStrict $ A.encode f)
           (BSL.toStrict $ A.encode v)
  case res of
    Right _ -> do
      -- L.logInfo @Text "Redis hset" $ show r
      pure res
    Left err -> do
      L.logError @Text "Redis hset" $ show err
      pure res

rIncr :: (ToJSON k, L.MonadFlow m) =>
  Text -> k -> m (Either T.KVDBReply Integer)
rIncr cName k = do
  res <- L.runKVDB cName $ L.incr (BSL.toStrict $ A.encode k)
  case res of
    Right _ -> do
      -- L.logInfo @Text "Redis incr" $ show r
      pure res
    Left err -> do
      L.logError @Text "Redis incr" $ show err
      pure res

rSetT :: (ToJSON v, L.MonadFlow m) =>
  Text -> Text -> v -> m (Either T.KVDBReply T.KVDBStatus)
rSetT cName k v = do
  res <- L.runKVDB cName $ L.set (TE.encodeUtf8 k) (BSL.toStrict $ A.encode v)
  case res of
    Right _ -> do
      -- L.logInfo @Text "Redis set" $ show r
      pure res
    Left err -> do
      L.logError @Text "Redis set" $ show err
      pure res

rSet :: (ToJSON k, ToJSON v, L.MonadFlow m) =>
  Text -> k -> v -> m (Either T.KVDBReply T.KVDBStatus)
rSet cName k v = do
  res <- L.runKVDB cName $ L.set (BSL.toStrict $ A.encode k) (BSL.toStrict $ A.encode v)
  case res of
    Right _ -> do
      -- L.logInfo @Text "Redis set" $ show r
      pure res
    Left err -> do
      L.logError @Text "Redis set" $ show err
      pure res

rSetB :: (L.MonadFlow m) =>
  Text -> ByteString -> ByteString -> m (Either T.KVDBReply T.KVDBStatus)
rSetB cName k v = do
  res <- L.runKVDB cName $ L.set k v
  case res of
    Right _ -> do
      -- L.logInfo @Text "Redis set" $ show r
      pure res
    Left err -> do
      L.logError @Text "Redis set" $ show err
      pure res

rGetT :: (FromJSON v, L.MonadFlow m) =>
  Text -> Text -> m (Maybe v)
rGetT cName k = do
  mv <- L.runKVDB cName $ L.get (TE.encodeUtf8 k)
  -- L.logDebug @Text "Raw value" $ show mv
  case mv of
    Right (Just val) -> do
      let v = A.eitherDecode $ BSL.fromStrict val
      case v of
        Left err -> do
          L.logError @Text "Decoding error: " $ show err
          pure Nothing
        Right v' -> do
          -- L.logDebug @Text "Decoded value" $ show v'
          pure $ Just v'
    Right Nothing -> pure Nothing
    Left err -> do
      L.logError @Text "Redis get" $ show err
      pure Nothing

rGet :: (ToJSON k, FromJSON v, L.MonadFlow m) =>
  Text -> k -> m (Maybe v)
rGet cName k = do
  mv <- L.runKVDB cName $ L.get (BSL.toStrict $ A.encode k)
  case mv of
    Right (Just val) -> pure $ A.decode $ BSL.fromStrict val
    Right Nothing -> pure Nothing
    Left err -> do
      L.logError @Text "Redis get" $ show err
      pure Nothing

rGetB :: (L.MonadFlow m) =>
  Text -> ByteString -> m (Maybe ByteString) -- Binary.decode?
rGetB cName k = do
  mv <- L.runKVDB cName $ L.get k
  case mv of
    Right mval -> pure mval
    Left err -> do
      L.logError @Text "Redis get" $ show err
      pure Nothing

rSetex :: (ToJSON k, ToJSON v, Integral t, L.MonadFlow m) =>
  Text -> k -> v -> t -> m (Either T.KVDBReply T.KVDBStatus)
rSetex cName k v t = do
  res <- L.runKVDB cName $
    L.setex (BSL.toStrict $ A.encode k) (toInteger t) (BSL.toStrict $ A.encode v)
  case res of
    Right _ -> do
      -- L.logInfo @Text "Redis setex" $ show r
      pure res
    Left err -> do
      L.logError @Text "Redis setex" $ show err
      pure res

rSetexB :: (L.MonadFlow m) =>
  Text -> ByteString -> ByteString -> Integer -> m (Either T.KVDBReply T.KVDBStatus)
rSetexB cName k v t = do
  res <- L.runKVDB cName $ L.setex k t v
  case res of
    Right _ -> do
      -- L.logInfo @Text "Redis setex" $ show r
      pure res
    Left err -> do
      L.logError @Text "Redis setex" $ show err
      pure res

rSetexT :: (ToJSON v, Integral t, L.MonadFlow m) =>
  Text -> Text -> v -> t -> m (Either T.KVDBReply T.KVDBStatus)
rSetexT cName k v t = do
  res <- L.runKVDB cName $
    L.setex (TE.encodeUtf8 k) (toInteger t) (BSL.toStrict $ A.encode v)
  case res of
    Right _ -> do
      -- L.logInfo @Text "Redis setex" $ show r
      pure res
    Left err -> do
      L.logError @Text "Redis setex" $ show err
      pure res
