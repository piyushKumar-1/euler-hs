{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}

module EulerHS.Core.SqlDB.Language
  (
  -- * SQLDB language
  -- ** Types
    SqlDB
  , SqlDBMethodF(..)
  -- ** Methods
  , sqlThrowException -- for tests
  , findRow
  , findRows
  , insertRows
  , insertRowsReturningList
  , updateRows
  , deleteRows
  , deleteRowsReturningListPG
  , updateRowsReturningListPG
  ) where

import qualified Database.Beam          as B
import qualified Database.Beam.MySQL    as BM
import qualified Database.Beam.Postgres as BP
import qualified EulerHS.Core.Types     as T
import           EulerHS.Prelude


type SqlDB beM = F (SqlDBMethodF beM)

data SqlDBMethodF beM next where
  SqlDBMethod :: (T.NativeSqlConn -> (Text -> IO ()) -> IO a) -> (a -> next) -> SqlDBMethodF beM next

  SqlThrowException :: (Exception e)
    => e -> (a -> next) -> SqlDBMethodF beM next

instance Functor (SqlDBMethodF beM) where
  fmap f (SqlDBMethod runner next) = SqlDBMethod runner (f . next)
  fmap f (SqlThrowException message next) = SqlThrowException message (f . next)

sqlDBMethod
  :: (T.BeamRunner beM, T.BeamRuntime be beM)
  => beM a
  -> SqlDB beM a
sqlDBMethod act = liftFC $ SqlDBMethod (flip T.getBeamDebugRunner act) id

-- For testing purposes
sqlThrowException :: forall a e beM be . (Exception e, T.BeamRunner beM, T.BeamRuntime be beM) => e -> SqlDB beM a
sqlThrowException ex = liftFC $ SqlThrowException ex id

-- Convenience interface

-- | Select many
findRows
  :: (T.BeamRunner beM, T.BeamRuntime be beM, B.FromBackendRow be a)
  => B.SqlSelect be a
  -> SqlDB beM [a]
findRows = sqlDBMethod . T.rtSelectReturningList

-- | Select one
findRow
  :: (T.BeamRunner beM, T.BeamRuntime be beM, B.FromBackendRow be a)
  => B.SqlSelect be a
  -> SqlDB beM (Maybe a)
findRow = sqlDBMethod . T.rtSelectReturningOne

-- | Insert
insertRows
  :: (T.BeamRunner beM, T.BeamRuntime be beM)
  => B.SqlInsert be table
  -> SqlDB beM ()
insertRows = sqlDBMethod . T.rtInsert

-- | Insert returning list
insertRowsReturningList
  :: (B.Beamable table, B.FromBackendRow be (table Identity), T.BeamRuntime be beM, T.BeamRunner beM)
  => B.SqlInsert be table
  -> SqlDB beM [table Identity]
insertRowsReturningList = sqlDBMethod . T.rtInsertReturningList

-- | Update
updateRows
  :: (T.BeamRunner beM, T.BeamRuntime be beM)
  => B.SqlUpdate be table
  -> SqlDB beM ()
updateRows = sqlDBMethod . T.rtUpdate

-- | Delete
deleteRows
  :: (T.BeamRunner beM, T.BeamRuntime be beM)
  => B.SqlDelete be table
  -> SqlDB beM ()
deleteRows = sqlDBMethod . T.rtDelete


-- Postgres only extra methods

deleteRowsReturningListPG
  :: (B.Beamable table, B.FromBackendRow BP.Postgres (table Identity))
  => B.SqlDelete BP.Postgres table
  -> SqlDB BP.Pg [table Identity]
deleteRowsReturningListPG = sqlDBMethod . T.deleteReturningListPG

updateRowsReturningListPG
  :: (B.Beamable table, B.FromBackendRow BP.Postgres (table Identity))
  => B.SqlUpdate BP.Postgres table
  -> SqlDB BP.Pg [table Identity]
updateRowsReturningListPG = sqlDBMethod . T.updateReturningListPG
