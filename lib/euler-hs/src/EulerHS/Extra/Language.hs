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
  , insertRow
  ) where

import           EulerHS.Prelude hiding (id, get)


import qualified EulerHS.Core.Types         as T
import qualified EulerHS.Framework.Language as L
import qualified EulerHS.Core.KVDB.Language as L

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
    Left (T.ExceptionMessage _) -> L.initKVDBConnection cfg
    res                         -> pure res



-- KVDB helpers

rExpire :: (ToJSON k, Integral t) => k -> t -> L.Flow (Either T.KVDBReply Bool)
rExpire k t = do
  res <- L.runKVDB $ L.expire (BSL.toStrict $ A.encode k) (toInteger t)
  case res of
    Right r -> do
      L.logInfo @Text "Redis expire" $ show r
      pure res
    Left err -> do
      L.logError @Text "Redis expire" $ show err
      pure res

rDel :: (ToJSON k) => [k] -> L.Flow (Either T.KVDBReply Integer)
rDel ks = do
  res <- L.runKVDB $ L.del (BSL.toStrict . A.encode <$> ks)
  case res of
    Right r -> do
      L.logInfo @Text "Redis del" $ show r
      pure res
    Left err -> do
      L.logError @Text "Redis del" $ show err
      pure res

rExists :: (ToJSON k) => k -> L.Flow (Either T.KVDBReply Bool)
rExists k = do
  res <- L.runKVDB $ L.exists (BSL.toStrict $ A.encode k)
  case res of
    Right r -> do
      L.logInfo @Text "Redis exists" $ show r
      pure res
    Left err -> do
      L.logError @Text "Redis exists" $ show err
      pure res

rHget :: (ToJSON k, ToJSON f, FromJSON v)
  => k -> f -> L.Flow (Maybe v)
rHget k f = do
  res <- L.runKVDB $
    L.hget (BSL.toStrict $ A.encode k)
           (BSL.toStrict $ A.encode f)
  case res of
    Right (Just val) -> pure $ A.decode $ BSL.fromStrict val
    Right (Nothing) -> pure Nothing
    Left err -> do
      L.logError @Text "Redis hget" $ show err
      pure Nothing

rHset :: (ToJSON k, ToJSON f, ToJSON v)
  => k -> f -> v -> L.Flow (Either T.KVDBReply Bool)
rHset k f v = do
  res <- L.runKVDB $
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

rIncr :: (ToJSON k) => k -> L.Flow (Either T.KVDBReply Integer)
rIncr k = do
  res <- L.runKVDB $ L.incr (BSL.toStrict $ A.encode k)
  case res of
    Right r -> do
      L.logInfo @Text "Redis incr" $ show r
      pure res
    Left err -> do
      L.logError @Text "Redis incr" $ show err
      pure res

rSet :: (ToJSON k, ToJSON v) => k -> v -> L.Flow (Either T.KVDBReply T.KVDBStatus)
rSet k v = do
  res <- L.runKVDB $ L.set (BSL.toStrict $ A.encode k) (BSL.toStrict $ A.encode v)
  case res of
    Right r -> do
      L.logInfo @Text "Redis set" $ show r
      pure res
    Left err -> do
      L.logError @Text "Redis set" $ show err
      pure res

rGet :: (ToJSON k, FromJSON v) => k -> L.Flow (Maybe v)
rGet k = do
  mv <- L.runKVDB $ L.get (BSL.toStrict $ A.encode k)
  case mv of
    Right (Just val) -> pure $ A.decode $ BSL.fromStrict val
    Right (Nothing) -> pure Nothing
    Left err -> do
      L.logError @Text "Redis get" $ show err
      pure Nothing

rSetex :: (ToJSON k, ToJSON v, Integral t) => k -> v -> t -> L.Flow (Either T.KVDBReply T.KVDBStatus)
rSetex k v t = do
  res <- L.runKVDB $
    L.setex (BSL.toStrict $ A.encode k) (toInteger t) (BSL.toStrict $ A.encode v)
  case res of
    Right r -> do
      L.logInfo @Text "Redis setex" $ show r
      pure res
    Left err -> do
      L.logError @Text "Redis setex" $ show err
      pure res



-- | Inserts some rows but returns the first result dropping others.
-- Use this function with care.
insertRow
  :: ( B.Beamable table
     , B.FromBackendRow be (table Identity)
     , T.BeamRuntime be beM
     , T.BeamRunner beM
     , T.JSONEx table
     )
  => T.DBConfig beM
  -> B.SqlInsert be table
  -> L.Flow (Either Text (table Identity))
insertRow db insertStmt = do
  results <- withDB db $ L.insertRowsReturningList insertStmt
  pure $ case results of
    []    -> Left "Unexpected empty result."
    (x:_) -> Right x
