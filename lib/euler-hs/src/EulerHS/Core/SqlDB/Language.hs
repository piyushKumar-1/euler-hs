{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

module EulerHS.Core.SqlDB.Language where

import           EulerHS.Prelude

import qualified EulerHS.Core.Types as T

import qualified Database.Beam as B
import qualified Database.Beam.Backend.SQL as B
import qualified Database.Beam.Schema.Tables as B

import qualified Database.Beam.Postgres.Syntax as BP

import qualified Database.Beam.Sqlite.Syntax as BS

data SqlDBAction next where
  RawQuery :: String -> (a -> next) -> SqlDBAction next

  Create
    ::
      ( B.Beamable table
      , B.FieldsFulfillConstraint (B.HasSqlValueSyntax BP.PgValueSyntax) table
      , B.FieldsFulfillConstraint (B.HasSqlValueSyntax BS.SqliteValueSyntax) table
      )
    => (forall be . B.DatabaseEntity be db (B.TableEntity table))
    -> [table Identity]
    -> (T.DBResult () -> next)
    -> SqlDBAction next

  -- FindOne :: (() -> next) -> SqlDBAction next
  -- FindAll :: (() -> next) -> SqlDBAction next
  -- FindOrCreate :: (() -> next) -> SqlDBAction next
  -- Query :: (() -> next) -> SqlDBAction next
  -- Update :: (() -> next) -> SqlDBAction next
  -- Delete :: (() -> next) -> SqlDBAction next

instance Functor SqlDBAction where
  fmap f (RawQuery q next) = RawQuery q (f . next)

  fmap f (Create ent rows next) = Create ent rows (f . next)

  -- fmap f (FindOne next) = FindOne (f . next)
  -- fmap f (FindAll next) = FindAll (f . next)
  -- fmap f (FindOrCreate next) = FindOrCreate (f . next)
  -- fmap f (Query next) = Query (f . next)
  -- fmap f (Update next) = Update (f . next)
  -- fmap f (Delete next) = Delete (f . next)

type SqlDB = F SqlDBAction

rawQuery' :: String -> SqlDB a
rawQuery' q = liftFC $ RawQuery q id

create
  ::
    ( B.Beamable table
    , B.FieldsFulfillConstraint (B.HasSqlValueSyntax BP.PgValueSyntax) table
    , B.FieldsFulfillConstraint (B.HasSqlValueSyntax BS.SqliteValueSyntax) table
    )
  => (forall be . B.DatabaseEntity be db (B.TableEntity table))
  -> [table Identity]
  -> SqlDB (T.DBResult ())
create ent rows = liftFC $ Create ent rows id
