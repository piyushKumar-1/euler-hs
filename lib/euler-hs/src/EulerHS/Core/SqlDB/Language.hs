{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module EulerHS.Core.SqlDB.Language
  (
  -- * SQLDB language
  -- ** Types
    SqlDB
  , SqlDBMethodF(..)
  -- ** Methods
  , findRow
  , findRows
  , insertRows
  , updateRows
  , deleteRows
  ) where

import           EulerHS.Prelude
import qualified Database.Beam as B
import qualified Database.Beam.Backend.SQL as B
import qualified Database.Beam.Query as B
import qualified EulerHS.Core.Types as T


data SqlDBAction beM a where
  SqlDBAction :: beM a -> SqlDBAction beM a

select''
  :: (T.BeamRuntime be beM, B.FromBackendRow be a)
  => B.SqlSelect be a
  -> SqlDBAction beM [a]
select'' a = SqlDBAction (T.rtSelectReturningList a)

selectOne''
  :: (T.BeamRuntime be beM, B.FromBackendRow be a)
  => B.SqlSelect be a
  -> SqlDBAction beM (Maybe a)
selectOne'' a = SqlDBAction (T.rtSelectReturningOne a)

insert''
  :: T.BeamRuntime be beM
  => B.SqlInsert be table
  -> SqlDBAction beM ()
insert'' a = SqlDBAction (T.rtInsert a)

update''
  :: T.BeamRuntime be beM
  => B.SqlUpdate be table
  -> SqlDBAction beM ()
update'' a = SqlDBAction (T.rtUpdate a)

delete''
  :: T.BeamRuntime be beM
  => B.SqlDelete be table
  -> SqlDBAction beM ()
delete'' a = SqlDBAction (T.rtDelete a)


getBeamRunner'
  :: (T.BeamRunner beM, T.BeamRuntime be beM)
  => T.NativeSqlConn
  -> SqlDBAction beM a
  -> ((String -> IO ()) -> IO a)
getBeamRunner' conn (SqlDBAction beM) = T.getBeamDebugRunner conn beM



data SqlDBMethodF beM next where
  SqlDBMethod :: (T.NativeSqlConn -> (String -> IO ()) -> IO a) -> (a -> next) -> SqlDBMethodF beM next

instance Functor (SqlDBMethodF beM) where
  fmap f (SqlDBMethod runner next) = SqlDBMethod runner (f . next)

type SqlDB beM = F (SqlDBMethodF beM)

sqlDBMethod
  :: (T.BeamRunner beM, T.BeamRuntime be beM)
  => SqlDBAction beM a
  -> SqlDB beM a
sqlDBMethod act = do
  let runner = \conn -> getBeamRunner' conn act
  liftFC $ SqlDBMethod runner id



select'
  :: (T.BeamRunner beM, T.BeamRuntime be beM, B.FromBackendRow be a)
  => B.SqlSelect be a
  -> SqlDB beM [a]
select' = sqlDBMethod . select''

selectOne'
  :: (T.BeamRunner beM, T.BeamRuntime be beM, B.FromBackendRow be a)
  => B.SqlSelect be a
  -> SqlDB beM (Maybe a)
selectOne' = sqlDBMethod . selectOne''

insert'
  :: (T.BeamRunner beM, T.BeamRuntime be beM)
  => B.SqlInsert be table
  -> SqlDB beM ()
insert' = sqlDBMethod . insert''

update'
  :: (T.BeamRunner beM, T.BeamRuntime be beM)
  => B.SqlUpdate be table
  -> SqlDB beM ()
update' = sqlDBMethod . update''

delete'
  :: (T.BeamRunner beM, T.BeamRuntime be beM)
  => B.SqlDelete be table
  -> SqlDB beM ()
delete' = sqlDBMethod . delete''



-- Convenience interface

-- | Select many
findRows
  :: (T.BeamRunner beM, T.BeamRuntime be beM, B.FromBackendRow be a)
  => B.SqlSelect be a
  -> SqlDB beM [a]
findRows = select'

-- | Select one
findRow
  :: (T.BeamRunner beM, T.BeamRuntime be beM, B.FromBackendRow be a)
  => B.SqlSelect be a
  -> SqlDB beM (Maybe a)
findRow = selectOne'

-- | Insert
insertRows
  :: (T.BeamRunner beM, T.BeamRuntime be beM)
  => B.SqlInsert be table
  -> SqlDB beM ()
insertRows = insert'

-- | Update
updateRows
  :: (T.BeamRunner beM, T.BeamRuntime be beM)
  => B.SqlUpdate be table
  -> SqlDB beM ()
updateRows = update'

-- | Delete
deleteRows
  :: (T.BeamRunner beM, T.BeamRuntime be beM)
  => B.SqlDelete be table
  -> SqlDB beM ()
deleteRows = delete'
