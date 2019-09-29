--{-# LANGUAGE StandaloneDeriving #-}
--{-# LANGUAGE DeriveAnyClass #-}
--{-# LANGUAGE PartialTypeSignatures #-}

module EulerHS.Core.SqlDB.Interpreter where

import EulerHS.Prelude

import qualified EulerHS.Core.Language as L
import qualified EulerHS.Core.Types as T
import qualified EulerHS.Core.Runtime as R

-- import qualified EulerHS.Core.SqlDB.Impl.SQLite as SQLite
-- import qualified Database.SQLite.Simple as SQLite

import qualified Database.Beam as B
-- import qualified Database.Beam.Backend.SQL as B
-- import qualified Database.Beam.Schema.Tables as B

import qualified Database.Beam.Sqlite as BS
-- import qualified Database.Beam.Sqlite.Syntax as BS


-- import qualified Database.Beam.Postgres as BP


interpretSqlDBAction :: R.CoreRuntime -> T.SqlConn -> L.SqlDBAction a -> ExceptT T.DBError IO a
interpretSqlDBAction _ _ (L.RawQuery q next) = error "not implemented"

interpretSqlDBAction rt conn (L.Create ent rows next) =
  next <$> case conn of
    T.MockedSql _ -> error "not implemented MockedSql"

    T.SQLiteConn connection ->
      ExceptT $ map (first $ T.DBError T.SomeError . show) $
        try @_ @SomeException $ BS.runBeamSqliteDebug putStrLn connection $
          B.runInsert $ B.insert ent $ B.insertExpressions rows

interpretSqlDBAction rt conn (L.FindOne ent query next) =
  next <$> case conn of
    T.MockedSql _ -> error "not implemented MockedSql"

    T.SQLiteConn connection ->
      ExceptT $ map (first $ T.DBError T.SomeError . show) $
        try @_ @SomeException $ BS.runBeamSqliteDebug putStrLn connection $
          B.runSelectReturningOne $ B.select $ B.limit_ 1 $ query $ B.all_ ent


runSqlDB :: R.CoreRuntime -> T.SqlConn -> L.SqlDB a -> ExceptT T.DBError IO a
runSqlDB coreRt conn = foldF (interpretSqlDBAction coreRt conn)
