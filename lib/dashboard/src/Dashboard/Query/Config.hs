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

-- Used for transforming string results from a query
-- FIXME: Make this a Map when we move to more recent Dhall
type FieldValueMap
  = [(String, String)]

data FieldConfiguration =
  FieldConfiguration
    { fieldName :: FieldName
    , fieldType :: FieldType
    , fieldValueMap :: FieldValueMap
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
