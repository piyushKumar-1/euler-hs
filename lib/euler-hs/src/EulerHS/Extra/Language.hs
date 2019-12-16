module EulerHS.Extra.Language
  ( getOrInitSqlConn
  , getOrInitKVDBConn
  ) where

import           EulerHS.Prelude

import qualified EulerHS.Core.Types         as T
import qualified EulerHS.Framework.Language as L


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
