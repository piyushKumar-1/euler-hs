module WebService.Language.Extra
  ( throwOnFailedWithLog
  , getCurrentTimeUTC
  , getCurrentDateInSeconds
  , getCurrentDateInMillis
  , getCurrentDateStringWithSecOffset
  ) where

import EulerHS.Prelude

import Data.Time

import qualified Data.Text             as Text
import qualified Data.Time.Clock.POSIX as TP

import qualified EulerHS.Language as L
import qualified WebService.Types as WST

throwOnFailedWithLog :: Show e => Either e a -> (Text -> WST.AppException) -> Text -> L.Flow ()
throwOnFailedWithLog (Left err) mkException msg = do
  L.logError "" $ msg <> " " <> show err <> ""
  L.throwException $ mkException $ msg <> " " <> show err <> ""
throwOnFailedWithLog _ _ _ = pure ()

getCurrentTimeUTC :: L.Flow LocalTime
getCurrentTimeUTC = L.runIO getCurrentTimeUTC'

getCurrentTimeUTC' :: IO LocalTime
getCurrentTimeUTC' = (zonedTimeToLocalTime . utcToZonedTime utc ) <$> getCurrentTime

getCurrentDateInSeconds :: L.Flow Int
getCurrentDateInSeconds = L.runIO $ do
   t <- TP.getPOSIXTime
   pure $ floor t

getCurrentDateInMillis :: L.Flow Int
getCurrentDateInMillis = L.runIO $ do
   t <- (*1000) <$> TP.getPOSIXTime
   pure $ floor t

getCurrentDateStringWithSecOffset :: Int -> L.Flow Text
getCurrentDateStringWithSecOffset secs = do
  L.runIO $ (Text.pack . formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" . addUTCTime (realToFrac secs)) <$> getCurrentTime