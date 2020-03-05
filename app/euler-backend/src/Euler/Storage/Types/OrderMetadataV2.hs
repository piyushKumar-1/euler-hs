{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE DeriveGeneric #-}

module Euler.Storage.Types.OrderMetadataV2
  ( OrderMetadataV2T(..)
  , OrderMetadataV2
  -- , Id
  , orderMetadataV2EMod
  , defaultOrderMetadataV2
  ) where

import EulerHS.Prelude hiding (id)
import Data.Time
import Euler.Common.Types.DefaultDate
import qualified Database.Beam as B


data OrderMetadataV2T f = OrderMetadataV2
  { id               :: B.C f (Maybe Int)
  , browser          :: B.C f (Maybe Text)
  , browserVersion   :: B.C f (Maybe Text)
  , dateCreated      :: B.C f LocalTime
  , device           :: B.C f (Maybe Text)
  , lastUpdated      :: B.C f LocalTime
  , metadata         :: B.C f (Maybe Text)
  , mobile           :: B.C f (Maybe Bool)
  , operatingSystem  :: B.C f (Maybe Text)
  , orderReferenceId :: B.C f Int
  , ipAddress        :: B.C f (Maybe Text)
  , referer          :: B.C f (Maybe Text)
  , userAgent        :: B.C f (Maybe Text)
  }
  deriving (Generic, B.Beamable)

instance B.Table OrderMetadataV2T where
  data PrimaryKey OrderMetadataV2T f =
    Id (B.C f (Maybe Int)) deriving (Generic, B.Beamable)
  primaryKey = Id . id

type OrderMetadataV2 = OrderMetadataV2T Identity
-- type Id = B.PrimaryKey OrderMetadataV2T Identity

deriving instance Show OrderMetadataV2
deriving instance Eq OrderMetadataV2
deriving instance ToJSON OrderMetadataV2
deriving instance FromJSON OrderMetadataV2
deriving instance Read OrderMetadataV2
deriving instance Ord OrderMetadataV2

orderMetadataV2EMod :: B.EntityModification
  (B.DatabaseEntity be db) be (B.TableEntity OrderMetadataV2T)
orderMetadataV2EMod = B.modifyTableFields
  B.tableModification
    { id = B.fieldNamed "id"
    , browser = B.fieldNamed "browser"
    , browserVersion = B.fieldNamed "browser_version"
    , dateCreated = B.fieldNamed "date_created"
    , device = B.fieldNamed "device"
    , lastUpdated = B.fieldNamed "last_updated"
    , metadata = B.fieldNamed "metadata"
    , mobile = B.fieldNamed "mobile"
    , operatingSystem = B.fieldNamed "operating_system"
    , orderReferenceId = B.fieldNamed "order_reference_id"
    , ipAddress = B.fieldNamed "ip_address"
    , referer = B.fieldNamed "referer"
    , userAgent = B.fieldNamed "user_agent"
    }

defaultOrderMetadataV2 :: OrderMetadataV2
defaultOrderMetadataV2 = OrderMetadataV2
  { id               = Nothing -- :: Maybe Int
  , browser          = Nothing -- :: Maybe Text
  , browserVersion   = Nothing -- :: Maybe Text
  , dateCreated      = defaultDate -- :: LocalTime
  , device           = Nothing -- :: Maybe Text
  , lastUpdated      = defaultDate -- :: LocalTime
  , metadata         = Nothing -- :: Maybe Text
  , mobile           = Just False -- :: Maybe Bool
  , operatingSystem  = Nothing -- :: Maybe Text
  , orderReferenceId = 1 -- :: Int
  , ipAddress        = Nothing -- :: Maybe Text
  , referer          = Nothing -- :: Maybe Text
  , userAgent        = Nothing -- :: Maybe Text
  }
