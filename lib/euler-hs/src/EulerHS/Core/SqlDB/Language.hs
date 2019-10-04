{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module EulerHS.Core.SqlDB.Language where

import           EulerHS.Prelude
import qualified Database.Beam as B
-- import qualified EulerHS.Core.Types as T
-- import qualified Database.Beam.Query.Internal as B
-- import qualified Database.Beam.Backend.SQL as B
-- import qualified Database.Beam.Schema.Tables as B
-- import qualified Database.Beam.Postgres.Syntax as BP
-- import qualified Database.Beam.Postgres as BP
-- import qualified Database.Beam.Sqlite.Syntax as BS
-- import qualified Database.Beam.Sqlite as BS


data SqlDBAction be next where
  -- RawQuery
  --   :: String
  --   -> (a -> next)
  --   -> SqlDBAction next

  RunSelect
    :: B.FromBackendRow be a
    => B.SqlSelect be a
    -> ([a] -> next)
    -> SqlDBAction be next

  RunSelectOne
    :: B.FromBackendRow be a
    => B.SqlSelect be a
    -> (Maybe a -> next)
    -> SqlDBAction be next

  RunInsert
    :: B.SqlInsert be table
    -> (() -> next)
    -> SqlDBAction be next

  RunUpdate
    :: B.SqlUpdate be table
    -> (() -> next)
    -> SqlDBAction be next

  RunDelete
    :: B.SqlDelete be table
    -> (() -> next)
    -> SqlDBAction be next

instance Functor (SqlDBAction be) where
  -- fmap f (RawQuery q next) = RawQuery q (f . next)

  fmap f (RunSelect a next) = RunSelect a (f . next)

  fmap f (RunSelectOne a next) = RunSelectOne a (f . next)

  fmap f (RunInsert  a next) = RunInsert a (f . next)

  fmap f (RunUpdate a next) = RunUpdate a (f . next)

  fmap f (RunDelete a next) = RunDelete a (f . next)


type SqlDB be = F (SqlDBAction be)

-- rawQuery' :: String -> SqlDB a
-- rawQuery' q = liftFC $ RawQuery q id

runSelect :: B.FromBackendRow be a => B.SqlSelect be a -> SqlDB be [a]
runSelect a = liftFC $ RunSelect a id

runSelectOne :: B.FromBackendRow be a => B.SqlSelect be a -> SqlDB be (Maybe a)
runSelectOne a = liftFC $ RunSelectOne a id

runInsert :: B.SqlInsert be table -> SqlDB be ()
runInsert a = liftFC $ RunInsert a id

runUpdate :: B.SqlUpdate be table -> SqlDB be ()
runUpdate a = liftFC $ RunUpdate a id

runDelete :: B.SqlDelete be table -> SqlDB be ()
runDelete a = liftFC $ RunDelete a id
