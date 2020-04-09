module WebService.Language.Extra
  ( throwOnFailedWithLog
  , getCurrentTimeUTC
  , getCurrentDateInSeconds
  , getCurrentDateInMillis
  , getCurrentDateStringWithSecOffset
  , unsafeInsertRow
    -- * @Text tag typed loggers
  , logErrorT
  , logWarningT
  , logInfoT
  , logDebugT
  ) where

import EulerHS.Prelude

import Data.Time

import qualified Data.Text             as Text
import qualified Data.Time.Clock.POSIX as TP

import qualified EulerHS.Types         as T
import qualified EulerHS.Language      as L
import qualified Database.Beam         as B
import qualified WebService.Types      as WST



throwOnFailedWithLog :: Show e => Either e a -> (Text -> WST.AppException) -> Text -> L.Flow ()
throwOnFailedWithLog (Left err) mkException msg = do
  L.logError @Text "" $ msg <> " " <> show err <> ""
  L.throwException $ mkException $ msg <> " " <> show err <> ""
throwOnFailedWithLog _ _ _ = pure ()

getCurrentTimeUTC :: L.Flow LocalTime
getCurrentTimeUTC = L.runIO' "getCurrentTimeUTC" getCurrentTimeUTC'

getCurrentTimeUTC' :: IO LocalTime
getCurrentTimeUTC' = (zonedTimeToLocalTime . utcToZonedTime utc ) <$> getCurrentTime

getCurrentDateInSeconds :: L.Flow Int
getCurrentDateInSeconds = L.runIO' "getCurrentDateInSeconds" $ do
   t <- TP.getPOSIXTime
   pure $ floor t

getCurrentDateInMillis :: L.Flow Int
getCurrentDateInMillis = L.runIO' "getCurrentDateInMillis" $ do
   t <- (*1000) <$> TP.getPOSIXTime
   pure $ floor t

getCurrentDateStringWithSecOffset :: Int -> L.Flow Text
getCurrentDateStringWithSecOffset secs = do
  L.runIO' "getCurrentDateStringWithSecOffset" $ (Text.pack . formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" . addUTCTime (realToFrac secs)) <$> getCurrentTime


-- | Unsafe function that logs an error and throws a servant error.
unsafeInsertRow
  :: ( B.Beamable table
     , B.FromBackendRow be (table Identity)
     , T.BeamRuntime be beM
     , T.BeamRunner beM
     , T.JSONEx (table Identity)
     , Exception exception
     )
  => exception
  -> T.DBConfig beM
  -> B.SqlInsert be table
  -> L.Flow (table Identity)
unsafeInsertRow exception db insertStmt = do
  eRes <- L.insertRow db insertStmt
  case eRes of
    Left err -> do
      L.logError ("unsafeInsertRow" :: Text) err
      L.throwException exception
    Right x -> pure x

-- ----------------------------------------------------------------------------

logErrorT :: Text -> T.Message -> L.Flow ()
logErrorT = L.logError @Text


logWarningT :: Text -> T.Message -> L.Flow ()
logWarningT = L.logWarning @Text--logError' :: Show tag => tag -> Message -> Flow ()


logInfoT :: Text -> T.Message -> L.Flow ()
logInfoT = L.logInfo @Text


logDebugT :: Text -> T.Message -> L.Flow ()
logDebugT = L.logDebug @Text