{-# LANGUAGE DeriveAnyClass #-}

module Euler.Storage.Repository.EulerDB where

import EulerHS.Prelude hiding(getOption)

import           EulerHS.Language
import           EulerHS.Types
import           WebService.Language

import qualified Database.Beam         as B

import qualified Euler.Common.Errors.PredefinedErrors as Errs
import qualified Database.Beam.MySQL  as BM

data EulerDbCfg = EulerDbCfg
  deriving (Generic, Typeable, Show, Eq, ToJSON, FromJSON)

instance OptionEntity EulerDbCfg (DBConfig BM.MySQLM)

unsafeInsertRowEulerDB ::
  ( B.Beamable table
  , B.FromBackendRow BM.MySQL (table Identity)
  , BeamRuntime BM.MySQL BM.MySQLM
  , BeamRunner BM.MySQLM
  , JSONEx (table Identity)
  , Exception exception
  )
  => exception
  -> B.SqlInsert BM.MySQL table
  -> Flow (table Identity)
unsafeInsertRowEulerDB ex insertStmt = do
  dbcfg <- getOption EulerDbCfg
  case dbcfg of
    Just cfg -> unsafeInsertRow ex cfg insertStmt
    Nothing -> do
      logError @String "MissingDB identifier" "Can't find EulerDB identifier in options"
      throwException Errs.internalError

withEulerDB :: JSONEx a => SqlDB BM.MySQLM a -> Flow a
withEulerDB act = do
  dbcfg <- getOption EulerDbCfg
  case dbcfg of
    Just cfg -> withDB cfg act
    Nothing -> do
      logError @String "MissingDB identifier" "Can't find EulerDB identifier in options"
      throwException Errs.internalError