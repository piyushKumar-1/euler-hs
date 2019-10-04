--{-# LANGUAGE StandaloneDeriving #-}
--{-# LANGUAGE DeriveAnyClass #-}
--{-# LANGUAGE PartialTypeSignatures #-}

module EulerHS.Core.SqlDB.Interpreter where

import EulerHS.Prelude

import qualified EulerHS.Core.Language as L
import qualified EulerHS.Core.Types as T
import qualified Database.Beam as B


interpretSqlDBAction :: L.SqlDBAction a -> T.DbBackendM a
-- interpretSqlDBAction (L.RawQuery q next) =
--   error "not implemented"

interpretSqlDBAction (L.RunSelect a next) =
  fmap next $ B.runSelectReturningList a

interpretSqlDBAction (L.RunSelectOne a next) =
  fmap next $ B.runSelectReturningOne a

interpretSqlDBAction (L.RunInsert a next) =
  fmap next $ B.runInsert a

interpretSqlDBAction (L.RunUpdate a next) =
  fmap next $ B.runUpdate a

interpretSqlDBAction (L.RunDelete a next) =
  fmap next $ B.runDelete a


runSqlDB :: L.SqlDB a -> T.DbBackendM a
runSqlDB = foldF interpretSqlDBAction

