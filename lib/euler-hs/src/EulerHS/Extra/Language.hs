module EulerHS.Extra.Language
  ( getOrInitSqlConn
  ) where

import           EulerHS.Prelude

import qualified EulerHS.Core.Types         as T
import qualified EulerHS.Framework.Language as L


-- | Get existing connection, or init a new connection.
getOrInitSqlConn :: T.DBConfig beM -> L.Flow (T.DBResult (T.SqlConn beM))
getOrInitSqlConn cfg = do
  eConn <- L.getSqlDBConnection cfg
  case eConn of
    Left (T.DBError T.ConnectionDoesNotExist _) -> L.initSqlDBConnection cfg
    res                                         -> pure res

-- | Get existing connection, or init a new connection.
-- getOrInitRedisConn
--   :: MVar (Map ByteString NativeKVDBConn)
--   -> ByteString
--   -> IO (Either KVDBReply KVDBConn)
-- getOrInitRedisConn kvdbConnMapMVar connTag = do
--   conn <- getRedisConnection kvdbConnMapMVar connTag
--   case conn of
--     Left (ExceptionMessage _) -> initRedisConnection kvdbConnMapMVar connTag
--     res                       -> pure res
