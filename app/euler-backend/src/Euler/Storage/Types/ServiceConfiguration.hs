{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE DeriveGeneric #-}

module Euler.Storage.Types.ServiceConfiguration
  ( ServiceConfigurationT(..)
  , ServiceConfiguration
  , ServiceConfigurationId
  , serviceConfigurationEMod
  ) where

import EulerHS.Prelude hiding (id)
import qualified Database.Beam as B

data ServiceConfigurationT f = ServiceConfiguration
  { id      :: B.C f (Maybe Int)
  , version :: B.C f Int
  , name    :: B.C f Text
  , value   :: B.C f Text
  }
  deriving (Generic, B.Beamable)

instance B.Table ServiceConfigurationT where
  data PrimaryKey ServiceConfigurationT f =
    ServiceConfigurationId (B.C f (Maybe Int)) deriving (Generic, B.Beamable)
  primaryKey = ServiceConfigurationId . id

type ServiceConfiguration = ServiceConfigurationT Identity
type ServiceConfigurationId = B.PrimaryKey ServiceConfigurationT Identity

deriving instance Show ServiceConfiguration
deriving instance Eq ServiceConfiguration
deriving instance ToJSON ServiceConfiguration
deriving instance FromJSON ServiceConfiguration
deriving instance Read ServiceConfiguration
deriving instance Ord ServiceConfiguration

serviceConfigurationEMod :: B.EntityModification
  (B.DatabaseEntity be db) be (B.TableEntity ServiceConfigurationT)
serviceConfigurationEMod = B.modifyTableFields
  B.tableModification
    { id = B.fieldNamed "id"
    , version = B.fieldNamed "version"
    , name = B.fieldNamed "name"
    , value = B.fieldNamed "value"
    }
