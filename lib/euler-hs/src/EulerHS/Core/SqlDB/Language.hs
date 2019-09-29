{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

module EulerHS.Core.SqlDB.Language where

import           EulerHS.Prelude

-- import qualified EulerHS.Core.Types as T

import qualified Database.Beam as B
import qualified Database.Beam.Backend.SQL as B
import qualified Database.Beam.Schema.Tables as B

-- import qualified Database.Beam.Postgres.Syntax as BP
-- import qualified Database.Beam.Postgres as BP

import qualified Database.Beam.Sqlite.Syntax as BS
import qualified Database.Beam.Sqlite as BS

type DBEntity db table =
  B.DatabaseEntity BS.Sqlite db (B.TableEntity table)

type DBTable db table =
  ( B.Table table
  , B.Database BS.Sqlite db
  , B.FieldsFulfillConstraint (B.HasSqlValueSyntax BS.SqliteValueSyntax) table
  , B.FromBackendRow BS.Sqlite (table Identity)
  )

data SqlDBAction next where
  RawQuery :: String -> (a -> next) -> SqlDBAction next

  Create
    :: DBTable db table
    => DBEntity db table
    -> (forall s . [table (B.QExpr BS.Sqlite s)])
    -> (() -> next)
    -> SqlDBAction next

  FindOne
    :: DBTable db table
    => DBEntity db table
    ->
      ( forall s
         . B.Q BS.Sqlite db s (table (B.QExpr BS.Sqlite s))
        -> B.Q BS.Sqlite db s (table (B.QExpr BS.Sqlite s))
      )
    -> (Maybe (table Identity) -> next)
    -> SqlDBAction next

  -- FindAll :: (() -> next) -> SqlDBAction next
  -- FindOrCreate :: (() -> next) -> SqlDBAction next
  -- Query :: (() -> next) -> SqlDBAction next
  -- Update :: (() -> next) -> SqlDBAction next
  -- Delete :: (() -> next) -> SqlDBAction next

instance Functor SqlDBAction where
  fmap f (RawQuery q next) = RawQuery q (f . next)

  fmap f (Create ent rows next) = Create ent rows (f . next)

  fmap f (FindOne ent q next) = FindOne ent q (f . next)

  -- fmap f (FindAll next) = FindAll (f . next)
  -- fmap f (FindOrCreate next) = FindOrCreate (f . next)
  -- fmap f (Query next) = Query (f . next)
  -- fmap f (Update next) = Update (f . next)
  -- fmap f (Delete next) = Delete (f . next)

type SqlDB = F SqlDBAction

rawQuery' :: String -> SqlDB a
rawQuery' q = liftFC $ RawQuery q id

create
  :: DBTable db table
  => DBEntity db table
  -> (forall s . [table (B.QExpr BS.Sqlite s)])
  -> SqlDB ()
create ent rows = liftFC $ Create ent rows id

findOne
  :: DBTable db table
  => DBEntity db table
  ->
    ( forall s
       . B.Q BS.Sqlite db s (table (B.QExpr BS.Sqlite s))
      -> B.Q BS.Sqlite db s (table (B.QExpr BS.Sqlite s))
    )
  -> SqlDB (Maybe (table Identity))
findOne ent q = liftFC $ FindOne ent q id



