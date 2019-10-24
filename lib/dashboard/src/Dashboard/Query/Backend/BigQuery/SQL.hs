{-# LANGUAGE OverloadedStrings #-}
module Dashboard.Query.Backend.BigQuery.SQL
  ( printSQL
  ) where

import Data.Time.Clock
import Data.Time.Clock.POSIX
import Fmt ((+|), (|+))
import qualified Fmt
import Universum hiding (All, Sum, filter, group)

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

fmtDateTime :: Bool -> FieldName -> Fmt.Builder
fmtDateTime dateIsString field =
  let field' = if dateIsString
                  then "PARSE_TIMESTAMP('%F %T', " +| field |+ ")"
                  else "TIMESTAMP(" +| field |+ ")"
  in "UNIX_SECONDS(" <> field' <> ")"

-- Includes a trailing comma assuming if there's an interval, we have other
-- select fields as well
-- (UNIX_SECONDS(PARSE_TIMESTAMP('%F %T', txn_last_modified)) - MOD(UNIX_SECONDS(PARSE_TIMESTAMP('%F %T', txn_last_modified)), 15)) AS ts
fmtIntervalSelection :: Bool -> Interval -> Fmt.Builder
fmtIntervalSelection dateIsString (Interval (Timestamp start) _ step intervalField) =
  case step of
    Just (Milliseconds duration) ->
      fmtDateTime dateIsString intervalField <> fmtStepMod duration <> " AS " <> timeStampField
    Nothing -> toPOSIXSeconds start |+ " AS " +| timeStampField
  where
    fmtStepMod duration =
       " - MOD(" +| fmtDateTime dateIsString intervalField |+ ", " +| (duration `quot` 1000) |+ ")"

fmtSelection :: Bool -> Selection -> Interval -> Fmt.Builder
fmtSelection dateIsString (Selection selections) interval =
  "SELECT " +| fmtIntervalSelection dateIsString interval |+
    ", " +| fmtCommaSepSelections selections

  where
    fmtCommaSepSelections = mconcat . intersperse ", " . fmap fmtOneSelection

    fmtOneSelection :: (Maybe SelectOp, SelectField) -> Fmt.Builder
    fmtOneSelection (Nothing, sField) = fmtSelectField sField
    fmtOneSelection (Just op, sField) =
      fmtSelectOp op <> "(" +| fmtSelectField sField |+ ")"

    fmtSelectOp op =
      case op of
           Average -> "AVG"
           Count   -> "COUNT"
           Sum     -> "SUM"

fmtFrom :: String -> Fmt.Builder
fmtFrom table = "FROM `" +| table |+ "`"

fmtIntervalFilter :: Bool -> Interval -> Fmt.Builder
fmtIntervalFilter dateIsString (Interval (Timestamp start) (Timestamp end) _ field) =
  -- UNIX_SECONDS(PARSE_TIMESTAMP('%F %T', txn_last_modified)) BETWEEN 1569456045 AND 1569457470
  "(" +| fmtDateTime dateIsString field |+
    " BETWEEN " +| toPOSIXSeconds start |+ " AND " +| toPOSIXSeconds end |+ ")"

fmtValue :: Value -> Fmt.Builder
fmtValue value =
  case value of
       StringValue filterVal -> "\"" +| filterVal |+ "\""
       IntValue filterVal    -> Fmt.build filterVal
       FloatValue filterVal  -> Fmt.build filterVal

fmtFilterOp :: FilterOp -> Fmt.Builder
fmtFilterOp Equal    = "="
fmtFilterOp NotEqual = "<>"

fmtFilter :: Bool -> Interval -> Filter -> Fmt.Builder
fmtFilter dateIsString interval (Filter fs) =
  "WHERE " +| fmtIntervalFilter dateIsString interval |+
  foldMap
    (\(filterField, filterOp, filterValue) ->
       " AND " +| filterField |+ " " +| fmtFilterOp filterOp |+ " " +| fmtValue filterValue)
    fs

fmtGroup :: GroupBy -> Fmt.Builder
fmtGroup (GroupBy gs) =
  "GROUP BY " <> (mconcat . intersperse ", " $ fmap Fmt.build gs <> [timeStampField])

printSQL :: Bool -> Query -> Text
printSQL dateIsString (Query selection table interval filter group) =
  fmtSelection dateIsString selection interval |+
  " " +| fmtFrom table |+
  " " +| fmtFilter dateIsString interval filter |+
  " " +| fmtGroup group |+
  ";"
