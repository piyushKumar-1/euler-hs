module EulerHS.Core.SqlDB.Interpreter
  (
  -- * SQL DB Interpreter
  runSqlDB
  ) where

import           EulerHS.Prelude

import qualified EulerHS.Core.Language as L
import qualified EulerHS.Core.Types as T

-- import           Control.Exception (throwIO)


-- TODO: The runner runner gets composed in in `sqlDBMethod`. Move it into the interpreter!
interpretSqlDBMethod
  :: T.NativeSqlConn
  -> (Text -> IO ())
  -> L.SqlDBMethodF beM a
  -> IO a
interpretSqlDBMethod conn logger (L.SqlDBMethod runner next) =
  next <$> runner conn logger

-- interpretSqlMethod (L.SqlThrowException ex _) = do
--   void <$> throwIO ex

runSqlDB  :: T.NativeSqlConn -> (Text -> IO ()) -> L.SqlDB beM a -> IO a
runSqlDB sqlConn logger = foldF (interpretSqlDBMethod sqlConn logger)
