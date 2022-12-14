{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
module EulerHS.KVConnector.InMemConfig.Types 

    where

import           EulerHS.Prelude hiding (maximum)
import qualified Data.Aeson as A
import           EulerHS.Options (OptionEntity)
import           EulerHS.KVConnector.Types (MeshError)
import qualified EulerHS.Types as T

data IMCEnabledTables = IMCEnabledTables
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance T.OptionEntity IMCEnabledTables [Text]

data IsIMCEnabled = IsIMCEnabled
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance T.OptionEntity IsIMCEnabled Bool

data InMemCacheResult =  EntryValid Any | 
                        EntryExpired Any (Maybe KeyForInMemConfig) | 
                        EntryNotFound (KeyForInMemConfig) |
                        TableIneligible | 
                        UnknownError MeshError 
  deriving (Show)

type KeyForInMemConfig = Text

data LooperStarted  = LooperStarted Text
  deriving (Generic, A.ToJSON, Typeable, Show)

instance OptionEntity LooperStarted Bool

data RecordId = RecordId
  deriving (Generic, Typeable, Show, Eq, ToJSON, FromJSON)

instance OptionEntity RecordId Text

type LatestRecordId = Text
type RecordKeyValues = (Text, ByteString)
