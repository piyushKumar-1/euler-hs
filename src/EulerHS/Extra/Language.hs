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
  ) where

import           EulerHS.Prelude            hiding (get, id)


import qualified EulerHS.Core.KVDB.Language as L
import qualified EulerHS.Core.Types         as T
import qualified EulerHS.Framework.Language as L

import qualified Data.Aeson                 as A
import qualified Data.ByteString.Lazy       as BSL
import qualified Data.Text.Encoding         as TE

-- | Get existing SQL connection, or init a new connection.
getOrInitSqlConn :: T.DBConfig beM -> L.Flow (T.DBResult (T.SqlConn beM))
getOrInitSqlConn cfg = do
  eConn <- L.getSqlDBConnection cfg
  case eConn of
    Left (T.DBError T.ConnectionDoesNotExist _) -> L.initSqlDBConnection cfg
    res                                         -> pure res

-- | Get existing Redis connection, or init a new connection.
getOrInitKVDBConn
  :: T.KVDBConfig
  -> L.Flow (T.KVDBAnswer T.KVDBConn)
getOrInitKVDBConn cfg = do
  conn <- L.getKVDBConnection cfg
  case conn of
    Left (T.KVDBError T.KVDBConnectionDoesNotExist _) -> L.initKVDBConnection cfg
    res -> pure res

-- KVDB helpers

rExpire :: (ToJSON k, Integral t) => Text -> k -> t -> L.Flow (Either T.KVDBReply Bool)
rExpire cName k t = do
  res <- L.runKVDB cName $ L.expire (BSL.toStrict $ A.encode k) (toInteger t)
  case res of
    Right r -> do
      -- L.logInfo  "Redis expire" $ show r
      pure res
    Left err -> do
      L.logError "Redis expire" $ show err
      pure res

rExpireB :: Text -> ByteString -> Integer -> L.Flow (Either T.KVDBReply Bool)
rExpireB cName k t = do
  res <- L.runKVDB cName $ L.expire k t
  case res of
    Right r -> do
      -- L.logInfo  "Redis expire" $ show r
      pure res
    Left err -> do
      L.logError "Redis expire" $ show err
      pure res

rDel :: (ToJSON k) => Text -> [k] -> L.Flow (Either T.KVDBReply Integer)
rDel cName ks = do
  res <- L.runKVDB cName $ L.del (BSL.toStrict . A.encode <$> ks)
  case res of
    Right r -> do
      -- L.logInfo  "Redis del" $ show r
      pure res
    Left err -> do
      L.logError "Redis del" $ show err
      pure res

rDelB ::  Text -> [ByteString] -> L.Flow (Either T.KVDBReply Integer)
rDelB cName ks = do
  res <- L.runKVDB cName $ L.del ks
  case res of
    Right r -> do
      -- L.logInfo  "Redis del" $ show r
      pure res
    Left err -> do
      L.logError "Redis del" $ show err
      pure res

rExists :: (ToJSON k) => Text -> k -> L.Flow (Either T.KVDBReply Bool)
rExists cName k = do
  res <- L.runKVDB cName $ L.exists (BSL.toStrict $ A.encode k)
  case res of
    Right r -> do
      -- L.logInfo  "Redis exists" $ show r
      pure res
    Left err -> do
      L.logError "Redis exists" $ show err
      pure res

rHget :: (ToJSON k, ToJSON f, FromJSON v)
  => Text -> k -> f -> L.Flow (Maybe v)
rHget cName k f = do
  res <- L.runKVDB cName $
    L.hget (BSL.toStrict $ A.encode k)
           (BSL.toStrict $ A.encode f)
  case res of
    Right (Just val) -> pure $ A.decode $ BSL.fromStrict val
    Right (Nothing) -> pure Nothing
    Left err -> do
      L.logError "Redis hget" $ show err
      pure Nothing

rHgetB :: Text -> ByteString -> ByteString -> L.Flow (Maybe ByteString)
rHgetB cName k f = do
  res <- L.runKVDB cName $ L.hget k f
  case res of
    Right (Just val) -> pure $ Just val
    Right (Nothing) -> pure Nothing
    Left err -> do
      L.logError "Redis hget" $ show err
      pure Nothing

rHset :: (ToJSON k, ToJSON f, ToJSON v)
  => Text -> k -> f -> v -> L.Flow (Either T.KVDBReply Bool)
rHset cName k f v = do
  res <- L.runKVDB cName $
    L.hset (BSL.toStrict $ A.encode k)
           (BSL.toStrict $ A.encode f)
           (BSL.toStrict $ A.encode v)
  case res of
    Right r -> do
      -- L.logInfo  "Redis hset" $ show r
      pure res
    Left err -> do
      L.logError "Redis hset" $ show err
      pure res

rIncr :: (ToJSON k) => Text -> k -> L.Flow (Either T.KVDBReply Integer)
rIncr cName k = do
  res <- L.runKVDB cName $ L.incr (BSL.toStrict $ A.encode k)
  case res of
    Right r -> do
      -- L.logInfo  "Redis incr" $ show r
      pure res
    Left err -> do
      L.logError "Redis incr" $ show err
      pure res

rSetT :: (ToJSON v) => Text -> Text -> v -> L.Flow (Either T.KVDBReply T.KVDBStatus)
rSetT cName k v = do
  res <- L.runKVDB cName $ L.set (TE.encodeUtf8 k) (BSL.toStrict $ A.encode v)
  case res of
    Right r -> do
      -- L.logInfo  "Redis set" $ show r
      pure res
    Left err -> do
      L.logError "Redis set" $ show err
      pure res

rSet :: (ToJSON k, ToJSON v) => Text -> k -> v -> L.Flow (Either T.KVDBReply T.KVDBStatus)
rSet cName k v = do
  res <- L.runKVDB cName $ L.set (BSL.toStrict $ A.encode k) (BSL.toStrict $ A.encode v)
  case res of
    Right r -> do
      -- L.logInfo  "Redis set" $ show r
      pure res
    Left err -> do
      L.logError "Redis set" $ show err
      pure res

rSetB ::  Text -> ByteString -> ByteString -> L.Flow (Either T.KVDBReply T.KVDBStatus)
rSetB cName k v = do
  res <- L.runKVDB cName $ L.set k v
  case res of
    Right r -> do
      -- L.logInfo  "Redis set" $ show r
      pure res
    Left err -> do
      L.logError "Redis set" $ show err
      pure res

rGetT :: (FromJSON v, Show v) => Text -> Text -> L.Flow (Maybe v)
rGetT cName k = do
  mv <- L.runKVDB cName $ L.get (TE.encodeUtf8 k)
  -- L.logDebug  "Raw value" $ show mv
  case mv of
    Right (Just val) -> do
      let v = A.eitherDecode $ BSL.fromStrict val
      case v of
        Left err -> do
          L.logError "Decoding error: " $ show err
          pure Nothing
        Right v' -> do
          -- L.logDebug  "Decoded value" $ show v'
          pure $ Just v'
    Right (Nothing) -> pure Nothing
    Left err -> do
      L.logError "Redis get" $ show err
      pure Nothing

rGet :: (ToJSON k, FromJSON v) => Text -> k -> L.Flow (Maybe v)
rGet cName k = do
  mv <- L.runKVDB cName $ L.get (BSL.toStrict $ A.encode k)
  case mv of
    Right (Just val) -> pure $ A.decode $ BSL.fromStrict val
    Right (Nothing) -> pure Nothing
    Left err -> do
      L.logError "Redis get" $ show err
      pure Nothing

rGetB ::  Text -> ByteString -> L.Flow (Maybe ByteString) -- Binary.decode?
rGetB cName k = do
  mv <- L.runKVDB cName $ L.get k
  case mv of
    Right mval -> pure $ mval
    Left err -> do
      L.logError "Redis get" $ show err
      pure Nothing

rSetex :: (ToJSON k, ToJSON v, Integral t) => Text -> k -> v -> t -> L.Flow (Either T.KVDBReply T.KVDBStatus)
rSetex cName k v t = do
  res <- L.runKVDB cName $
    L.setex (BSL.toStrict $ A.encode k) (toInteger t) (BSL.toStrict $ A.encode v)
  case res of
    Right r -> do
      -- L.logInfo  "Redis setex" $ show r
      pure res
    Left err -> do
      L.logError "Redis setex" $ show err
      pure res

rSetexB :: Text -> ByteString -> ByteString -> Integer -> L.Flow (Either T.KVDBReply T.KVDBStatus)
rSetexB cName k v t = do
  res <- L.runKVDB cName $ L.setex k t v
  case res of
    Right r -> do
      -- L.logInfo  "Redis setex" $ show r
      pure res
    Left err -> do
      L.logError "Redis setex" $ show err
      pure res

rSetexT :: (ToJSON v, Integral t) => Text -> Text -> v -> t -> L.Flow (Either T.KVDBReply T.KVDBStatus)
rSetexT cName k v t = do
  res <- L.runKVDB cName $
    L.setex (TE.encodeUtf8 k) (toInteger t) (BSL.toStrict $ A.encode v)
  case res of
    Right r -> do
      -- L.logInfo  "Redis setex" $ show r
      pure res
    Left err -> do
      L.logError "Redis setex" $ show err
      pure res
