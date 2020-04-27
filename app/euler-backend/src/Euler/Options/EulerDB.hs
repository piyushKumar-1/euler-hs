{-# LANGUAGE DeriveAnyClass #-}

module Euler.Options.EulerDB where

import EulerHS.Prelude

import           EulerHS.Types

import qualified Database.Beam.MySQL  as BM

data EulerDbCfg = EulerDbCfg
  deriving (Generic, Typeable, Show, Eq, ToJSON, FromJSON)

instance OptionEntity EulerDbCfg (DBConfig BM.MySQLM)