{-# LANGUAGE DeriveGeneric #-}

module Dashboard.Query.Types where

import Data.Time.Clock

import Data.Aeson.Types (FromJSON, ToJSON)

import GHC.Generics
import Universum hiding (All)

-- | IMPORTANT
-- | These types are used in JSON API on servers in this repository as well as
-- | clients _outside_ this repo, in the form of generated purescript code (see
-- | the console-dashboard-gen-ps target).
-- |
-- | If you make any changes to these types, make sure that all clients talking
-- | to newer servers are also updated. This currently only includes the
-- | Console client in the euler-ps repository.

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
  Selection [(Maybe SelectOp, SelectField)]
  deriving (Generic, Show, Eq)

newtype Timestamp =
  Timestamp UTCTime
  deriving (Generic, Show, Eq, Ord)

newtype Milliseconds =
  Milliseconds { unMs :: Int }
  deriving (Generic, Show, Eq, Ord)

data Interval =
  Interval
    { start :: Timestamp
    , stop  :: Timestamp
    , step  :: Maybe Milliseconds
    , field :: FieldName
    }
  deriving (Generic, Show, Eq, Ord)

-- Needs to be any supported data type, not just String
data Value
  = StringValue String
  | IntValue Int
  | FloatValue Double
  deriving (Generic, Show, Eq)

data FilterOp
  = EQUAL
  | NOT_EQUAL
  deriving (Generic, Show, Eq)

newtype Filter =
  Filter [(FieldName, FilterOp, Value)]
  deriving (Generic, Show, Eq)

newtype GroupBy =
  GroupBy [FieldName]
  deriving (Generic, Show, Eq)

data Query =
  Query
    { selection :: Selection
    , table     :: TableName
    , interval  :: Interval
    , filter    :: Filter
    , groupBy   :: GroupBy
    }
  deriving (Generic, Show, Eq)

data QueryResultRow =
  QueryResultRow
    { start   :: Timestamp
    , end     :: Timestamp
    , columns :: [Value]
    }
  deriving (Generic, Show, Eq)

instance Ord QueryResultRow where
  compare (QueryResultRow s1 e1 _) (QueryResultRow s2 e2 _) =
    if s1 /= s2
       then compare s1 s2
       else compare e1 e2

newtype QueryResult
  = QueryResult [QueryResultRow]
  deriving (Generic, Show, Eq)

data FieldType
  = IntType
  | FloatType
  | StringType
  | DateTimeType
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
  | SelectEmpty
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

instance ToJSON QueryResultRow

instance FromJSON QueryResultRow

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
