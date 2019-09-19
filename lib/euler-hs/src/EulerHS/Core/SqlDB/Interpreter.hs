module EulerHS.Core.SqlDB.Interpreter where

import EulerHS.Prelude

import qualified Database.SQLite.Simple as SQLite

import qualified EulerHS.Core.Language as L
import qualified EulerHS.Core.Types as T
import qualified EulerHS.Core.Runtime as R
import qualified EulerHS.Core.SqlDB.Impl.SQLite as SQLite


interpretSqlDBAction :: R.CoreRuntime -> L.SqlDBAction a -> IO a
interpretSqlDBAction _ (L.RawQuery q next) = error "not implemented"


runSqlDB :: R.CoreRuntime -> T.SqlConn -> L.SqlDB a -> IO a
runSqlDB coreRt conn = foldF (interpretSqlDBAction coreRt)
