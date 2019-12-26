module Dashboard.Query.Config where

import Dashboard.Query.Types (FieldName, TableName)
import Data.Aeson.Types (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Universum (Eq, Show)

data FieldType
  = IntType
  | FloatType
  | StringType
  | DateTimeType
  deriving (Generic, Show, Eq)

newtype TableConfiguration =
  TableConfiguration [(FieldName, FieldType)]
  deriving (Generic, Show, Eq)

newtype QueryConfiguration =
  QueryConfiguration [(TableName, TableConfiguration)]
  deriving (Generic, Show)

instance ToJSON FieldType

instance FromJSON FieldType

instance FromJSON TableConfiguration

instance ToJSON TableConfiguration

instance FromJSON QueryConfiguration

instance ToJSON QueryConfiguration

