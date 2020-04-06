{-# LANGUAGE DeriveAnyClass #-}

module Euler.Storage.Repository.EulerDB where

import EulerHS.Prelude hiding(getOption)

import           EulerHS.Language
import           EulerHS.Types
import           WebService.Language

import           Euler.Options.Options (EulerDbCfg(..))
import qualified Euler.Common.Errors.PredefinedErrors as Errs

import qualified Database.Beam       as B
import qualified Database.Beam.MySQL as BM



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
      logErrorT "MissingDB identifier" "Can't find EulerDB identifier in options"
      throwException Errs.internalError

withEulerDB :: JSONEx a => SqlDB BM.MySQLM a -> Flow a
withEulerDB act = do
  (dbcfg :: Maybe (DBConfig BM.MySQLM)) <- getOption EulerDbCfg
  case dbcfg of
    Just cfg -> withDB cfg act
    Nothing -> do
      logErrorT "MissingDB identifier" "Can't find EulerDB identifier in options"
      throwException Errs.internalError
