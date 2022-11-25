{-# LANGUAGE DeriveAnyClass     #-}
module EulerHS.KVConnector.InMemConfig.Types 

    where

import           EulerHS.Prelude hiding (maximum)
import qualified Data.Aeson as A
import           EulerHS.Options (OptionEntity)
import           EulerHS.KVConnector.Types (MeshError)

data InMemCacheResult = EntryValid Text | 
                        EntryExpired Text (Maybe KeyForInMemConfig) | 
                        EntryNotFound (KeyForInMemConfig) |
                        TableIneligible | 
                        UnknownError MeshError 

type KeyForInMemConfig = Text

data LooperStarted = LooperStarted
  deriving (Generic, A.ToJSON, Typeable)

instance OptionEntity LooperStarted Bool

data RecordId = RecordId
  deriving (Generic, Typeable, Show, Eq, ToJSON, FromJSON)

instance OptionEntity RecordId Text

type LatestRecordId = Text
type RecordKeyValues = (Text, Text)
