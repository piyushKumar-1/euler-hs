module Console.Query where

import Universum

import Data.Time.Clock
import Data.Time.Calendar (Day(ModifiedJulianDay))

import Dashboard.Query.Types

ts :: Timestamp
ts = Timestamp $ UTCTime {
      utctDay = ModifiedJulianDay 1,
      utctDayTime = secondsToDiffTime 0
      }

dummyResult :: QueryResult
dummyResult = QueryResult [QueryResultRow ts ts [IntValue 5]]

  -- stub for runQuery which will eventually
  -- pick a query backend
  -- execute the query and get a response using the said backend
  -- communicate failures on the backend out.
  -- Note that this itself can be a QueryBackend
  -- We may have to change this to allow for errors (consider ExceptT)
runQuery :: QueryConfiguration -> Query -> IO QueryResult
runQuery queryConf query = return dummyResult
