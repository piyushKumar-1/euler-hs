module EulerHS.Extra.Language
  ( getOrInitSqlConn
  , getOrInitKVDBConn
  , rExpire
  , rDel
  , rExists
  , rHget
  , rHset
  , rIncr
  , rSet
  , rGet
  , rSetex
  ) where

import           EulerHS.Prelude hiding (id, get)


import qualified EulerHS.Core.Types          as T
import qualified EulerHS.Framework.Language  as L
import qualified EulerHS.Core.KVDB.Language  as L
import qualified EulerHS.Core.SqlDB.Language as L

import qualified Data.Aeson           as A
import qualified Data.ByteString.Lazy as BSL
import qualified Database.Beam        as B

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
      L.logInfo @Text "Redis expire" $ show r
      pure res
    Left err -> do
      L.logError @Text "Redis expire" $ show err
      pure res

rDel :: (ToJSON k) => Text -> [k] -> L.Flow (Either T.KVDBReply Integer)
rDel cName ks = do
  res <- L.runKVDB cName $ L.del (BSL.toStrict . A.encode <$> ks)
  case res of
    Right r -> do
      L.logInfo @Text "Redis del" $ show r
      pure res
    Left err -> do
      L.logError @Text "Redis del" $ show err
      pure res

rExists :: (ToJSON k) => Text -> k -> L.Flow (Either T.KVDBReply Bool)
rExists cName k = do
  res <- L.runKVDB cName $ L.exists (BSL.toStrict $ A.encode k)
  case res of
    Right r -> do
      L.logInfo @Text "Redis exists" $ show r
      pure res
    Left err -> do
      L.logError @Text "Redis exists" $ show err
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
      L.logError @Text "Redis hget" $ show err
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
      L.logInfo @Text "Redis hset" $ show r
      pure res
    Left err -> do
      L.logError @Text "Redis hset" $ show err
      pure res

rIncr :: (ToJSON k) => Text -> k -> L.Flow (Either T.KVDBReply Integer)
rIncr cName k = do
  res <- L.runKVDB cName $ L.incr (BSL.toStrict $ A.encode k)
  case res of
    Right r -> do
      L.logInfo @Text "Redis incr" $ show r
      pure res
    Left err -> do
      L.logError @Text "Redis incr" $ show err
      pure res

rSet :: (ToJSON k, ToJSON v) => Text -> k -> v -> L.Flow (Either T.KVDBReply T.KVDBStatus)
rSet cName k v = do
  res <- L.runKVDB cName $ L.set (BSL.toStrict $ A.encode k) (BSL.toStrict $ A.encode v)
  case res of
    Right r -> do
      L.logInfo @Text "Redis set" $ show r
      pure res
    Left err -> do
      L.logError @Text "Redis set" $ show err
      pure res

rGet :: (ToJSON k, FromJSON v) => Text -> k -> L.Flow (Maybe v)
rGet cName k = do
  mv <- L.runKVDB cName $ L.get (BSL.toStrict $ A.encode k)
  case mv of
    Right (Just val) -> pure $ A.decode $ BSL.fromStrict val
    Right (Nothing) -> pure Nothing
    Left err -> do
      L.logError @Text "Redis get" $ show err
      pure Nothing

rSetex :: (ToJSON k, ToJSON v, Integral t) => Text -> k -> v -> t -> L.Flow (Either T.KVDBReply T.KVDBStatus)
rSetex cName k v t = do
  res <- L.runKVDB cName $
    L.setex (BSL.toStrict $ A.encode k) (toInteger t) (BSL.toStrict $ A.encode v)
  case res of
    Right r -> do
      L.logInfo @Text "Redis setex" $ show r
      pure res
    Left err -> do
      L.logError @Text "Redis setex" $ show err
      pure res
