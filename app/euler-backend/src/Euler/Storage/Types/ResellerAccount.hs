{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE DeriveGeneric #-}

module Euler.Storage.Types.ResellerAccount
  ( ResellerAccountT(..)
  , ResellerAccount
  , ResellerId
  , resellerAccountEMod
  , defaultResellerAccount
  ) where

import EulerHS.Prelude hiding (id)
import Data.Time
import qualified Database.Beam as B

data ResellerAccountT f = ResellerAccount
  { userId              :: B.C f (Maybe Int)
  , resellerName        :: B.C f (Maybe Text)
  , resellerApiEndpoint :: B.C f (Maybe Text)
  , dateCreated         :: B.C f (Maybe LocalTime)
  , lastModified        :: B.C f (Maybe LocalTime)
  , resellerId          :: B.C f Text
  }
  deriving (Generic, B.Beamable)

instance B.Table ResellerAccountT where
  data PrimaryKey ResellerAccountT f =
    ResellerId (B.C f (Text)) deriving (Generic, B.Beamable)
  primaryKey = ResellerId . resellerId

type ResellerAccount = ResellerAccountT Identity
type ResellerId = B.PrimaryKey ResellerAccountT Identity

deriving instance Show ResellerAccount
deriving instance Eq ResellerAccount
deriving instance ToJSON ResellerAccount
deriving instance FromJSON ResellerAccount
deriving instance Read ResellerAccount
deriving instance Ord ResellerAccount

resellerAccountEMod :: B.EntityModification
  (B.DatabaseEntity be db) be (B.TableEntity ResellerAccountT)
resellerAccountEMod = B.modifyTableFields
  B.tableModification
    { userId = B.fieldNamed "user_id"
    , resellerName = B.fieldNamed "reseller_name"
    , resellerApiEndpoint = B.fieldNamed "reseller_api_endpoint"
    , dateCreated = B.fieldNamed "date_created"
    , lastModified = B.fieldNamed "last_modified"
    , resellerId = B.fieldNamed "reseller_id"
    }

defaultResellerAccount :: ResellerAccount
defaultResellerAccount = ResellerAccount
  { userId              = Nothing      -- :: Maybe Int
  , resellerName        = Nothing      -- :: Maybe Text
  , resellerApiEndpoint = Just "/test" -- :: Maybe Text
  , dateCreated         = Nothing      -- :: Maybe LocalTime
  , lastModified        = Nothing      -- :: Maybe LocalTime
  , resellerId          = ""           -- :: Text
  }