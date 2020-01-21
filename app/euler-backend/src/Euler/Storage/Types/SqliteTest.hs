{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE DeriveGeneric #-}

module Euler.Storage.Types.SqliteTest
  ( TestTableT(..)
  , TestTable
  , SomeInt
  , testTableEMod
  , defaultTestTable
  ) where

import EulerHS.Prelude hiding (id)
import Data.Time
import Euler.Common.Types.DefaultDate (defaultDate)
import qualified Database.Beam as B


data TestTableT f = TestTable
  { someInt          :: B.C f Int
  , someText         :: B.C f Text
  , someBool         :: B.C f Bool
  , someTime         :: B.C f LocalTime
  , someDouble       :: B.C f Double
  }
  deriving (Generic, B.Beamable)

instance B.Table TestTableT where
  data PrimaryKey TestTableT f =
    SomeInt (B.C f Int) deriving (Generic, B.Beamable)
  primaryKey = SomeInt . someInt

type TestTable = TestTableT Identity
type SomeInt = B.PrimaryKey TestTableT Identity

deriving instance Show TestTable
deriving instance Eq TestTable
deriving instance ToJSON TestTable
deriving instance FromJSON TestTable
deriving instance Read TestTable
deriving instance Ord TestTable

testTableEMod :: B.EntityModification
  (B.DatabaseEntity be db) be (B.TableEntity TestTableT)
testTableEMod = B.modifyTableFields
  B.tableModification
    { someInt = B.fieldNamed "some_int"
    , someText = B.fieldNamed "some_text"
    , someBool = B.fieldNamed "some_bool"
    , someTime = B.fieldNamed "some_time"
    , someDouble = B.fieldNamed "some_double"
    }

defaultTestTable :: TestTable
defaultTestTable = TestTable
  { someInt = 1
  , someText = "defaultText"
  , someBool = True
  , someTime = defaultDate
  , someDouble = 0.12
  }