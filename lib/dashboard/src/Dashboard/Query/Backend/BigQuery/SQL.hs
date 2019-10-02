{-# LANGUAGE OverloadedStrings #-}
module Dashboard.Query.Backend.BigQuery.SQL
  ( printSQL
  ) where

import Data.Time.Clock
import Data.Time.Clock.POSIX
import Fmt ((+|), (|+))
import qualified Fmt
import Universum hiding (All, filter, group)

import Dashboard.Query.Types

timeStampField :: Fmt.Builder
timeStampField = "ts"

toPOSIXSeconds :: UTCTime -> Int64
toPOSIXSeconds t = round $ utcTimeToPOSIXSeconds t

fmtSelectField :: SelectField -> Fmt.Builder
fmtSelectField s =
  Fmt.build $ case s of
       All        -> "*"
       (Field sf) -> sf

-- Includes a trailing comma assuming if there's an interval, we have other
-- select fields as well
-- (UNIX_SECONDS(PARSE_TIMESTAMP('%F %T', txn_last_modified)) - MOD(UNIX_SECONDS(PARSE_TIMESTAMP('%F %T', txn_last_modified)), 15)) AS ts
fmtIntervalSelection :: Interval -> Fmt.Builder
fmtIntervalSelection (Interval _ _ step intervalField) =
  Fmt.maybeF . fmap fmt' $ step

  where
    fmt' :: Milliseconds -> Fmt.Builder
    fmt' (Milliseconds duration) =
      "UNIX_SECONDS(PARSE_TIMESTAMP('%F %T', " +| intervalField ++ "))" |+
        " - MOD(UNIX_SECONDS(PARSE_TIMESTAMP('%F %T', " +| intervalField |+ ")), " +|
           (duration `quot` 1000) |+ ") AS " <> timeStampField

fmtSelection :: Selection -> Interval -> Fmt.Builder
fmtSelection (Selection selections) interval =
  "SELECT " +| fmtIntervalSelection interval |+
    ", " +| fmtCommaSepSelections selections

  where
    fmtCommaSepSelections = mconcat . intersperse ", " . fmap fmtOneSelection

    fmtOneSelection :: (Maybe SelectOp, SelectField) -> Fmt.Builder
    fmtOneSelection (Nothing, sField) = fmtSelectField sField
    fmtOneSelection (Just op, sField) =
      fmtSelectOp op <> "(" +| fmtSelectField sField |+ ")"

    fmtSelectOp op =
      case op of
           SUM   -> "SUM"
           COUNT -> "COUNT"
           AVG   -> "AVG"

fmtFrom :: String -> Fmt.Builder
fmtFrom table = "FROM `" +| table |+ "`"

fmtIntervalFilter :: Interval -> Fmt.Builder
fmtIntervalFilter (Interval (Timestamp start) (Timestamp end) _ field) =
  -- UNIX_SECONDS(PARSE_TIMESTAMP('%F %T', txn_last_modified)) BETWEEN 1569456045 AND 1569457470
  "(UNIX_SECONDS(PARSE_TIMESTAMP('%F %T', " +| field |+
    ")) BETWEEN " +| toPOSIXSeconds start |+ " AND " +| toPOSIXSeconds end |+ ")"

fmtValue :: Value -> Fmt.Builder
fmtValue value =
  case value of
       StringValue filterVal -> "\"" +| filterVal |+ "\""
       IntValue filterVal    -> Fmt.build filterVal
       FloatValue filterVal  -> Fmt.build filterVal

fmtFilterOp :: FilterOp -> Fmt.Builder
fmtFilterOp EQUALS = "="

fmtFilter :: Interval -> Filter -> Fmt.Builder
fmtFilter interval (Filter fs) =
  "WHERE " +| fmtIntervalFilter interval |+
  foldMap
    (\(filterField, filterOp, filterValue) ->
       " AND " +| filterField |+ " " +| fmtFilterOp filterOp |+ " " +| fmtValue filterValue)
    fs

fmtGroup :: GroupBy -> Maybe Milliseconds -> Fmt.Builder
fmtGroup (GroupBy gs) step =
  let timeField = fmap (const timeStampField) step
  in
    "GROUP BY " <> (mconcat . intersperse ", " $ fmap Fmt.build gs <> maybeToList timeField)

-- FIXME: Return Text instead of String?
printSQL :: Query -> Text
printSQL (Query selection table interval filter group) =
  fmtSelection selection interval |+
  " " +| fmtFrom table |+
  " " +| fmtFilter interval filter |+
  " " +| fmtGroup group (step interval) |+
  ";"
