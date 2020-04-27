{-# LANGUAGE TypeApplications #-}

module WebService.Language.Extra
  ( throwOnFailedWithLog
  , getCurrentTimeUTC
  , getCurrentDateInSeconds
  , getCurrentDateInMillis
  , getCurrentDateStringWithSecOffset
  , insertRow
  , unsafeInsertRow
  , withDB
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
import           Servant               (err500)



-- | Creates a connection and runs a DB query.
-- Acts like 'getSqlDBConnection' + 'runDB'.
-- Throws err500 exception on connection or runDB failure.
-- Writes an error into the log.
withDB ::
  ( T.JSONEx a
  , T.BeamRunner beM
  , T.BeamRuntime be beM
  )
  => T.DBConfig beM
  -> L.SqlDB beM a
  -> L.Flow a
withDB dbConf act = do
  mConn <- L.getSqlDBConnection dbConf
  conn <- case mConn of
    Right c -> pure c
    Left err -> do
      L.logError @Text "SqlDB connect" $ show err
      L.throwException err500
  res <- L.runDB conn act
  case res of
    Left dbError -> do
     L.logError @Text "SqlDB interraction" $ show dbError
     L.throwException err500
    Right r -> pure r


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


-- | Inserts some rows but returns the first result dropping others.
-- Use this function with care.
insertRow
  :: ( B.Beamable table
     , B.FromBackendRow be (table Identity)
     , T.BeamRuntime be beM
     , T.BeamRunner beM
     , T.JSONEx (table Identity)
     )
  => T.DBConfig beM
  -> B.SqlInsert be table
  -> L.Flow (Either Text (table Identity))
insertRow db insertStmt = do
  results <- withDB db $ L.insertRowsReturningList insertStmt
  pure $ case results of
    []    -> Left "Unexpected empty result."
    (x:_) -> Right x


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
  eRes <- insertRow db insertStmt
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