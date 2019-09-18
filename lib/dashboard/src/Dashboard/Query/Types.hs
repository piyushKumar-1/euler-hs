{-# LANGUAGE DeriveGeneric #-}

module Dashboard.Query.Types where

import Data.Int (Int64)
import Data.Time.Clock

import Data.Aeson.Types (FromJSON, ToJSON)

import GHC.Generics
import Universum hiding (All)

type FieldName = String

-- FIXME: Support multiple selections
data SelectField
  = All
  | Field FieldName
  deriving (Generic, Show, Eq)

instance FromJSON SelectField
instance ToJSON SelectField

type TableName = String

data SelectOp
  = SUM
  | COUNT
  | AVG
  deriving (Generic, Show, Eq)

instance FromJSON SelectOp
instance ToJSON SelectOp

-- FIXME: Extend to multiple fields for custom queries
newtype Selection = Selection (SelectOp, SelectField)
  deriving (Generic, Show, Eq)

instance FromJSON Selection
instance ToJSON Selection

newtype Timestamp = Timestamp UTCTime
  deriving (Generic, Show, Eq)

instance FromJSON Timestamp
instance ToJSON Timestamp

newtype Milliseconds = Milliseconds Int64
  deriving (Generic, Show, Eq)

instance FromJSON Milliseconds
instance ToJSON Milliseconds

data Interval = Interval { start :: Timestamp
                         , stop  :: Timestamp
                         , step  :: Maybe Milliseconds
                         }
  deriving (Generic, Show, Eq)

instance FromJSON Interval
instance ToJSON Interval

-- Needs to be any supported data type, not just String
data Value
  = StringValue String
  | IntValue Int
  | FloatValue Float
  deriving (Generic, Show, Eq)

instance FromJSON Value
instance ToJSON Value

data FilterOp =
  EQUALS
  deriving (Generic, Show, Eq)

instance FromJSON FilterOp
instance ToJSON FilterOp

newtype Filter = Filter [(FieldName, FilterOp, Value)]
  deriving (Generic, Show, Eq)

instance FromJSON Filter
instance ToJSON Filter

newtype GroupBy = GroupBy [FieldName]
  deriving (Generic, Show, Eq)

instance FromJSON GroupBy
instance ToJSON GroupBy

data Query =
  Query Selection TableName Interval Filter GroupBy
  deriving (Generic, Show, Eq)

instance FromJSON Query
instance ToJSON Query

data QueryResultValue =
  QueryResultValue Timestamp Timestamp Value
  deriving (Generic, Show, Eq)

instance ToJSON QueryResultValue
instance FromJSON QueryResultValue

data QueryResult
  = SingleResult QueryResultValue
  | MultipleResult [(String, QueryResult)]
  deriving (Generic, Show, Eq)

instance ToJSON QueryResult
instance FromJSON QueryResult

data Table =
  Table TableName [FieldName]
  deriving (Generic, Show, Eq)

instance FromJSON Table
instance ToJSON Table

newtype QueryConfiguration = QueryConfiguration [Table]
  deriving (Generic, Show)

instance FromJSON QueryConfiguration
instance ToJSON QueryConfiguration

data QueryValidationError =
  QueryValidationError QueryErrorType String
  deriving (Generic, Show, Eq)
instance FromJSON QueryValidationError
instance ToJSON QueryValidationError

--Assuming single fields
data QueryErrorType =
  TableFieldError TableName | SelectFieldError FieldName | FilterFieldError FieldName | GroupByFieldError FieldName | FieldError FieldName
  deriving (Generic, Show, Eq)
instance FromJSON QueryErrorType
instance ToJSON QueryErrorType
