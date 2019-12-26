module Dashboard.Query.Config where

import Dashboard.Query.Types (FieldName, TableName)
import Dhall (Interpret)
import GHC.Generics (Generic)
import Universum

data FieldType
  = IntType
  | FloatType
  | StringType
  | DateTimeType
  deriving (Generic, Show, Eq)

data FieldConfiguration =
  FieldConfiguration
    { fieldName :: FieldName
    , fieldType :: FieldType
    }
  deriving (Generic, Show)

data TableConfiguration =
  TableConfiguration
    { tableName :: TableName
    , fields :: [FieldConfiguration]
    }
  deriving (Generic, Show)

newtype QueryConfiguration =
  QueryConfiguration
    { tables :: [TableConfiguration]
    }
  deriving (Generic, Show)

-- For using in Dhall configuration
instance Interpret FieldType
instance Interpret FieldConfiguration
instance Interpret TableConfiguration
instance Interpret QueryConfiguration

lookupTable :: TableName -> QueryConfiguration -> Maybe TableConfiguration
lookupTable t (QueryConfiguration qc) = find (\tc -> t == tableName tc) qc

lookupField :: FieldName -> TableConfiguration -> Maybe FieldType
lookupField f = map fieldType . find (\fc -> f == fieldName fc) . fields
