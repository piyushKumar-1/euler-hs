module Euler.KVDB.Redis where

import EulerHS.Prelude hiding (id, get)
import EulerHS.Language
import EulerHS.Types

import qualified Data.Aeson           as A
import qualified Data.ByteString.Lazy as BSL

-- EHS: invalid namespace for these functions.

rGet ::(ToJSON k, FromJSON v) => k -> Flow (Maybe v)
rGet k = do
  mv <- runKVDB $ get $ BSL.toStrict $ A.encode k
  case mv of
    Right (Just val) -> pure $ A.decode $ BSL.fromStrict val
    Right (Nothing) -> pure Nothing
    Left err -> do
      logError "Redis Get" $ show err
      pure Nothing

setCacheWithExpiry :: (ToJSON k, ToJSON v, Integral t) => k -> v -> t -> Flow (Either KVDBReply KVDBStatus)
setCacheWithExpiry k v t = do
  res <- runKVDB $ do
    setex (BSL.toStrict $ A.encode k) (toInteger t) (BSL.toStrict $ A.encode v)

  case res of
    Right r  -> logInfo "Redis setCacheWithExpiry" $ show r
    Left err -> logError "Redis setCacheWithExpiry" $ show err
    
  pure res
