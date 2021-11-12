module EulerHS.Extra.Time
  ( readToLocalTime
  , convertLocalToUTC
  , junkUTC
  ) where

import           Data.Time (Day (ModifiedJulianDay), LocalTime,
                            UTCTime (UTCTime), localTimeToUTC, utc,
                            utcToLocalTime)
import           EulerHS.Prelude

readToLocalTime :: Maybe UTCTime -> Maybe LocalTime
readToLocalTime = fmap (utcToLocalTime utc)

convertLocalToUTC :: LocalTime -> UTCTime
convertLocalToUTC = localTimeToUTC utc

junkUTC :: UTCTime
junkUTC = UTCTime (ModifiedJulianDay 0) 0
