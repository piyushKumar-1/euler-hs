module Dashboard.Query.Backend.BigQuery.SQL
  ( printSQL
  ) where

import Data.Time.Clock
import Data.Time.Clock.POSIX
import Universum hiding (All, filter, group)

import Dashboard.Query.Types

timeStampField :: String
timeStampField = "ts"

toPOSIXSeconds :: UTCTime -> Int64
toPOSIXSeconds t = round $ utcTimeToPOSIXSeconds t

printSelectField :: SelectField -> String
printSelectField s =
  case s of
       All        -> "*"
       (Field sf) -> sf

-- Includes a trailing comma assuming if there's an interval, we have other
-- select fields as well
-- (UNIX_SECONDS(PARSE_TIMESTAMP('%F %T', txn_last_modified)) - MOD(UNIX_SECONDS(PARSE_TIMESTAMP('%F %T', txn_last_modified)), 15)) AS ts
printIntervalSelection :: Interval -> String
printIntervalSelection (Interval _ _ step intervalField) =
  maybeToMonoid . fmap print' $ step

  where
    print' (Milliseconds duration) =
      "UNIX_SECONDS(PARSE_TIMESTAMP('%F %T', " ++ intervalField ++ "))" ++
      " - " ++
      "MOD(UNIX_SECONDS(PARSE_TIMESTAMP('%F %T', " ++ intervalField ++ ")), " ++
           show (duration `quot` 1000) ++ ")" ++
      " AS " ++ timeStampField

-- FIXME: Will need to use ROUND over aggregate functions if DB fields are float.
-- Likewise will need to parse an Int out of DB field if they are strings
printSelection :: Selection -> Interval -> String
printSelection (Selection selections) interval =
  "SELECT " ++
    printIntervalSelection interval ++ ", " ++
    (intercalate ", " . fmap printOneSelection $ selections)

  where
    printOneSelection (Nothing, sField) = printSelectField sField
    printOneSelection (Just op, sField) = show op ++ "(" ++ printSelectField sField ++ ")"

printFrom :: String -> String
printFrom table = "FROM " ++ table

printIntervalFilter :: Interval -> String
printIntervalFilter (Interval (Timestamp start) (Timestamp end) _ field) =
  -- UNIX_SECONDS(PARSE_TIMESTAMP('%F %T', txn_last_modified)) BETWEEN 1569456045 AND 1569457470
  "(UNIX_SECONDS(PARSE_TIMESTAMP('%F %T', " ++ field ++ ")) " ++
  "BETWEEN " ++ show (toPOSIXSeconds start) ++ " AND " ++ show (toPOSIXSeconds end) ++ ")"

printValue :: Value -> String
printValue value =
  case value of
    StringValue filterVal -> "\"" ++ filterVal ++ "\""
    IntValue filterVal    -> show filterVal
    FloatValue filterVal  -> show filterVal

printFilterOp :: FilterOp -> String
printFilterOp EQUALS = "="

printFilter :: Interval -> Filter -> String
printFilter interval (Filter fs) =
  "WHERE " ++
  printIntervalFilter interval ++
  concatMap
    (\(filterField, filterOp, filterValue) ->
       " AND " ++
       filterField ++ " " ++ printFilterOp filterOp ++ " " ++ printValue filterValue)
    fs

printGroup :: GroupBy -> Maybe Milliseconds -> String
printGroup (GroupBy gs) step =
  let timeField = if isJust step then timeStampField else ""
  in
    "GROUP BY " ++ intercalate ", " (gs ++ [timeField])

-- FIXME: Return Text instead of String?
printSQL :: Query -> String
printSQL (Query selection table interval filter group) =
  printSelection selection interval ++
  " " ++
  printFrom table ++
  " " ++
  printFilter interval filter ++
  " " ++
  printGroup group (step interval) ++
  ";"
