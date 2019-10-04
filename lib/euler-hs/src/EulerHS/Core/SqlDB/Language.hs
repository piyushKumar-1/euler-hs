{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module EulerHS.Core.SqlDB.Language where

import           EulerHS.Prelude
import qualified Database.Beam as B
import qualified EulerHS.Core.Types as T

-- import qualified Database.Beam.Query.Internal as B
-- import qualified Database.Beam.Backend.SQL as B
-- import qualified Database.Beam.Schema.Tables as B
-- import qualified Database.Beam.Postgres.Syntax as BP
-- import qualified Database.Beam.Postgres as BP
-- import qualified Database.Beam.Sqlite.Syntax as BS
-- import qualified Database.Beam.Sqlite as BS


data SqlDBAction next where
  -- RawQuery
  --   :: String
  --   -> (a -> next)
  --   -> SqlDBAction next

  RunSelect
    :: B.FromBackendRow T.DbBackend a
    => B.SqlSelect T.DbBackend a
    -> ([a] -> next)
    -> SqlDBAction next

  RunSelectOne
    :: B.FromBackendRow T.DbBackend a
    => B.SqlSelect T.DbBackend a
    -> (Maybe a -> next)
    -> SqlDBAction next

  RunInsert
    :: B.SqlInsert T.DbBackend table
    -> (() -> next)
    -> SqlDBAction next

  RunUpdate
    :: B.SqlUpdate T.DbBackend table
    -> (() -> next)
    -> SqlDBAction next

  RunDelete
    :: B.SqlDelete T.DbBackend table
    -> (() -> next)
    -> SqlDBAction next

instance Functor SqlDBAction where
  -- fmap f (RawQuery q next) = RawQuery q (f . next)

  fmap f (RunSelect a next) = RunSelect a (f . next)

  fmap f (RunSelectOne a next) = RunSelectOne a (f . next)

  fmap f (RunInsert  a next) = RunInsert a (f . next)

  fmap f (RunUpdate a next) = RunUpdate a (f . next)

  fmap f (RunDelete a next) = RunDelete a (f . next)


type SqlDB = F SqlDBAction

-- rawQuery' :: String -> SqlDB a
-- rawQuery' q = liftFC $ RawQuery q id

runSelect :: B.FromBackendRow T.DbBackend a => B.SqlSelect T.DbBackend a -> SqlDB [a]
runSelect a = liftFC $ RunSelect a id

runSelectOne :: B.FromBackendRow T.DbBackend a => B.SqlSelect T.DbBackend a -> SqlDB (Maybe a)
runSelectOne a = liftFC $ RunSelectOne a id

runInsert :: B.SqlInsert T.DbBackend table -> SqlDB ()
runInsert a = liftFC $ RunInsert a id

runUpdate :: B.SqlUpdate T.DbBackend table -> SqlDB ()
runUpdate a = liftFC $ RunUpdate a id

runDelete :: B.SqlDelete T.DbBackend table -> SqlDB ()
runDelete a = liftFC $ RunDelete a id
