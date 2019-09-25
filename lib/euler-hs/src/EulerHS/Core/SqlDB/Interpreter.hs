--{-# LANGUAGE StandaloneDeriving #-}
--{-# LANGUAGE DeriveAnyClass #-}

module EulerHS.Core.SqlDB.Interpreter where

import EulerHS.Prelude

import qualified EulerHS.Core.Language as L
import qualified EulerHS.Core.Types as T
import qualified EulerHS.Core.Runtime as R

-- import qualified EulerHS.Core.SqlDB.Impl.SQLite as SQLite
-- import qualified Database.SQLite.Simple as SQLite

import qualified Database.Beam as B
import qualified Database.Beam.Backend.SQL as B
import qualified Database.Beam.Schema.Tables as B

import qualified Database.Beam.Sqlite as BS

import qualified Database.Beam.Postgres as BP


interpretSqlDBAction :: R.CoreRuntime -> T.SqlConn -> L.SqlDBAction a -> IO a
interpretSqlDBAction _ _ (L.RawQuery q next) = error "not implemented"

interpretSqlDBAction rt conn (L.Create ent rows next) =
  next <$> case conn of
    T.MockedSql _ -> error "not implemented MockedSql"

    -- Note, that SomeError is more like a stub. Not a final solution, for sure
    -- TODO: more precise errors handling
    T.PostgresConn connection -> do
      result <- try $ BP.runBeamPostgresDebug putStrLn connection $ insert ent rows
      pure $ case result of
        Left (e :: SomeException) -> Left $ T.DBError T.SomeError $ show e
        Right res -> Right res

    T.SQLiteConn connection -> do
      result <- try $ BS.runBeamSqliteDebug putStrLn connection $ insert ent rows
      pure $ case result of
        Left (e :: SomeException) -> Left $ T.DBError T.SomeError $ show e
        Right res -> Right res

  where
    insert ::
      ( B.BeamSqlBackend be
      , B.MonadBeam be m
      , B.Beamable table
      , B.FieldsFulfillConstraint (B.BeamSqlBackendCanSerialize be) table
      ) => B.DatabaseEntity be db (B.TableEntity table) -> [table Identity] -> m ()
    insert ent rows = B.runInsert $
      B.insert ent $ B.insertValues rows


runSqlDB :: R.CoreRuntime -> T.SqlConn -> L.SqlDB a -> IO a
runSqlDB coreRt conn = foldF (interpretSqlDBAction coreRt conn)


-- this code has been used for debugging purposes

----------------------------------------------------------------------

-- data UserT f
--  = User
--    { _userFirstName :: B.Columnar f Text
--    , _userLastName  :: B.Columnar f Text
--    } deriving (Generic, B.Beamable)

-- instance B.Table UserT where
--  data PrimaryKey UserT f =
--    UserId (B.Columnar f Text) deriving (Generic, B.Beamable)

--  primaryKey = UserId . _userFirstName

-- type User = UserT Identity
-- type UserId = B.PrimaryKey UserT Identity

-- deriving instance Show User
-- deriving instance Eq User

-- data EulerDb f
--  = EulerDb
--    { _users :: f (B.TableEntity UserT)
--    } deriving (Generic, B.Database be)

-- eulerDb :: B.DatabaseSettings be EulerDb
-- eulerDb = B.defaultDbSettings

----------------------------------------------------------------------
