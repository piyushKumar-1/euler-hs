{-# LANGUAGE DeriveGeneric #-}

module Dashboard.Query.Types where

import Data.Int (Int64)
import Data.Time.Clock

import Data.Aeson.Types (FromJSON, ToJSON)

import GHC.Generics
import Universum hiding (All)

type FieldName = String

data SelectField
  = All
  | Field FieldName
  deriving (Generic, Show, Eq)

type TableName = String

data SelectOp
  = SUM
  | COUNT
  | AVG
  deriving (Generic, Show, Eq)

newtype Selection =
  Selection [(SelectOp, SelectField)]
  deriving (Generic, Show, Eq)

newtype Timestamp =
  Timestamp UTCTime
  deriving (Generic, Show, Eq)

newtype Milliseconds =
  Milliseconds Int64
  deriving (Generic, Show, Eq)

data Interval =
  Interval
    { start :: Timestamp
    , stop  :: Timestamp
    , step  :: Maybe Milliseconds
    , field :: FieldName
    }
  deriving (Generic, Show, Eq)

-- Needs to be any supported data type, not just String
data Value
  = StringValue String
  | IntValue Int
  | FloatValue Float
  deriving (Generic, Show, Eq)

data FilterOp =
  EQUALS
  deriving (Generic, Show, Eq)

newtype Filter =
  Filter [(FieldName, FilterOp, Value)]
  deriving (Generic, Show, Eq)

newtype GroupBy =
  GroupBy [FieldName]
  deriving (Generic, Show, Eq)

data Query =
  Query Selection TableName Interval Filter GroupBy
  deriving (Generic, Show, Eq)

data QueryResultValue =
  QueryResultValue Timestamp Timestamp Value
  deriving (Generic, Show, Eq)

data QueryResult
  = SingleResult QueryResultValue
  | MultipleResult [(String, QueryResult)]
  deriving (Generic, Show, Eq)

data FieldType
  = IntType
  | FloatType
  | StringType
  deriving (Generic,Show,Eq)

newtype TableConfiguration =
  TableConfiguration [(FieldName, FieldType)]
  deriving (Generic, Show, Eq)

newtype QueryConfiguration =
  QueryConfiguration [(TableName, TableConfiguration)]
  deriving (Generic, Show)

data QueryValidationError =
  QueryValidationError QueryErrorType String
  deriving (Generic, Show, Eq)

--Assuming single fields
data QueryErrorType
  = TableNotFound TableName
  | SelectFieldNotFound FieldName
  | FilterFieldNotFound FieldName
  | GroupByFieldNotFound FieldName
  | IntervalFieldNotFound FieldName
  | FilterTypeMismatch FieldName
  | SelectOperationNotValid SelectOp
  deriving (Generic, Show, Eq)

-- All the JSON instances
instance FromJSON Selection

instance ToJSON Selection

instance FromJSON Timestamp

instance ToJSON Timestamp

instance FromJSON Milliseconds

instance ToJSON Milliseconds

instance FromJSON Interval

instance ToJSON Interval

instance FromJSON Value

instance ToJSON Value

instance FromJSON FilterOp

instance ToJSON FilterOp

instance FromJSON Filter

instance ToJSON Filter

instance FromJSON GroupBy

instance ToJSON GroupBy

instance FromJSON Query

instance ToJSON Query

instance ToJSON QueryResultValue

instance FromJSON QueryResultValue

instance ToJSON QueryResult

instance FromJSON QueryResult

instance ToJSON FieldType

instance FromJSON FieldType

instance FromJSON TableConfiguration

instance ToJSON TableConfiguration

instance FromJSON QueryConfiguration

instance ToJSON QueryConfiguration

instance FromJSON SelectField

instance ToJSON SelectField

instance FromJSON SelectOp

instance ToJSON SelectOp

instance FromJSON QueryErrorType

instance ToJSON QueryErrorType

instance FromJSON QueryValidationError

instance ToJSON QueryValidationError
