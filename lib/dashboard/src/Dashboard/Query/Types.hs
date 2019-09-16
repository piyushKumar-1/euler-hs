module Dashboard.Query.Types where

import Data.Int (Int64)
import Data.Time.Clock (UTCTime)
import Universum

type FieldName = String

data SelectField
  = All
  | Field FieldName

type TableName = String

data SelectOp
  = SUM
  | COUNT
  | AVG

-- FIXME: Extend to multiple fields for custom queries
newtype Selection = Selection (SelectOp, SelectField)

newtype Timestamp = Timestamp UTCTime
newtype Milliseconds = Milliseconds Int64

data Interval = Interval { start :: Timestamp
                         , stop  :: Timestamp
                         , step  :: Maybe Milliseconds
                         }

-- Needs to be any supported data type, not just String
data Value
  = StringValue String
  | IntValue Int
  | FloatValue Float

data FilterOp =
  EQUALS

newtype Filter = Filter [(FieldName, FilterOp, Value)]

newtype GroupBy = GroupBy [FieldName]

data Query =
  Query Selection TableName Interval Filter GroupBy

data QueryResultValue =
  QueryResultValue Timestamp Timestamp Value

data QueryResult
  = SingleResult QueryResultValue
  | MultipleResult [(String, QueryResult)]
