--{-# LANGUAGE StandaloneDeriving #-}
--{-# LANGUAGE DeriveAnyClass #-}
--{-# LANGUAGE PartialTypeSignatures #-}

module EulerHS.Core.SqlDB.Interpreter where

import EulerHS.Prelude

import qualified EulerHS.Core.Language as L
import qualified EulerHS.Core.Types as T
import qualified Database.Beam as B
import qualified Database.Beam.Backend.SQL as B


interpretSqlDBMethod
  :: T.SqlConn beM
  -> (String -> IO ())
  -> L.SqlDBMethodF beM a
  -> IO a
interpretSqlDBMethod conn logger (L.SqlDBMethod runner next) =
  next <$> runner conn logger

runSqlDB  :: T.SqlConn beM -> (String -> IO ()) -> L.SqlDB beM a -> IO a
runSqlDB sqlConn logger = foldF (interpretSqlDBMethod sqlConn logger)
