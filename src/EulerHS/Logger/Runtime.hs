{-# OPTIONS_GHC -Wno-incomplete-record-updates #-} -- due to RDP - Koz

module EulerHS.Logger.Runtime
  (
    -- * Core Runtime
    CoreRuntime(..)
  , LoggerRuntime(..)
  , SeverityHandle(..)
  , defaultSeverityHandle
  , shouldLogRawSql
  , incLogCounter
  , createCoreRuntime
  , createVoidLoggerRuntime
  , createMemoryLoggerRuntime
  , createLoggerRuntime
  , createLoggerRuntime'
  , clearCoreRuntime
  , clearLoggerRuntime
  , getLogMaskingConfig
  , module X
  ) where

import           EulerHS.Prelude
-- Currently, TinyLogger is highly coupled with the Runtime.
-- Fix it if an interchangable implementations are needed.
import qualified EulerHS.Logger.TinyLogger as Impl
import qualified EulerHS.Logger.Types as T
import           EulerHS.SqlDB.Types as X (withTransaction)
import qualified System.Logger as Log

-- TODO: add StaticLoggerRuntimeContext if we'll need more than a single Bool
data LoggerRuntime
  = LoggerRuntime
    { _flowFormatter    :: T.FlowFormatter
    , _logContext       :: T.LogContext
    , _logLevel         :: T.LogLevel
    , _logRawSql        :: T.ShouldLogSQL
    , _logCounter       :: !T.LogCounter
    , _logMaskingConfig :: Maybe T.LogMaskingConfig
    , _logLoggerHandle  :: Impl.LoggerHandle
    , _severityCounter  :: SeverityHandle
    }
  | MemoryLoggerRuntime !T.FlowFormatter T.LogContext !T.LogLevel !(MVar [Text]) !T.LogCounter

newtype CoreRuntime = CoreRuntime
    { _loggerRuntime :: LoggerRuntime
    }

-- | LogLevel counter. Handlepattern for metrics
data SeverityHandle = SeverityHandle
  { incrementSeverityCount :: T.LogLevel -> IO ()
  }

defaultSeverityHandle :: SeverityHandle
defaultSeverityHandle = SeverityHandle
  { incrementSeverityCount = const (pure ())
  }

-- createLoggerRuntime :: LoggerConfig -> IO LoggerRuntime
-- createLoggerRuntime (MemoryLoggerConfig cfgLogLevel) =
--   MemoryLoggerRuntime cfgLogLevel <$> newMVar []
-- createLoggerRuntime cfg = do
--   counter <- initLogCounter
--   LoggerRuntime (_level cfg) (_logRawSql cfg) counter Nothing Nothing (_logMaskingConfig cfg)<$> Impl.createLogger cfg

createMemoryLoggerRuntime :: T.FlowFormatter -> T.LogLevel -> IO LoggerRuntime
createMemoryLoggerRuntime flowFormatter logLevel =
  MemoryLoggerRuntime flowFormatter mempty logLevel <$> newMVar [] <*> initLogCounter

createLoggerRuntime :: T.FlowFormatter
  -> T.LoggerConfig
  -> SeverityHandle
  -> IO LoggerRuntime
createLoggerRuntime flowFormatter cfg severity = do
  counter <- initLogCounter
  logHandle <- Impl.createLogger flowFormatter cfg
  pure $ LoggerRuntime
    flowFormatter
    mempty
    (T._logLevel cfg)
    (T._logRawSql cfg)
    counter
    Nothing
    logHandle
    severity

createLoggerRuntime' :: Maybe Log.DateFormat
  -> Maybe Log.Renderer
  -> T.BufferSize
  -> T.FlowFormatter
  -> T.LoggerConfig
  -> SeverityHandle
  -> IO LoggerRuntime
createLoggerRuntime' mbDateFormat mbRenderer bufferSize flowFormatter cfg severity = do
  counter <- initLogCounter
  loggerHandle <- Impl.createLogger' mbDateFormat mbRenderer bufferSize flowFormatter cfg
  pure $ LoggerRuntime
    flowFormatter
    mempty
    (T._logLevel cfg)
    (T._logRawSql cfg)
    counter
    (T._logMaskingConfig cfg)
    loggerHandle
    severity

createVoidLoggerRuntime :: IO LoggerRuntime
createVoidLoggerRuntime = do
  counter <- initLogCounter
  logHandle <- Impl.createVoidLogger
  pure $ LoggerRuntime (const $ pure T.showingMessageFormatter) mempty T.Debug T.SafelyOmitSqlLogs counter Nothing logHandle defaultSeverityHandle

clearLoggerRuntime :: LoggerRuntime -> IO ()
clearLoggerRuntime (LoggerRuntime flowFormatter _ _ _ _ _ handle _) = Impl.disposeLogger flowFormatter handle
clearLoggerRuntime (MemoryLoggerRuntime _ _ _ msgsVar _) = void $ swapMVar msgsVar []

createCoreRuntime :: LoggerRuntime -> IO CoreRuntime
createCoreRuntime = pure . CoreRuntime

clearCoreRuntime :: CoreRuntime -> IO ()
clearCoreRuntime _ = pure ()

shouldLogRawSql :: LoggerRuntime -> Bool
shouldLogRawSql = \case
  (LoggerRuntime _ _ _ T.UnsafeLogSQL_DO_NOT_USE_IN_PRODUCTION _ _ _ _) -> True
  _                                                                     -> False

getLogMaskingConfig :: LoggerRuntime -> Maybe T.LogMaskingConfig
getLogMaskingConfig = \case
  (LoggerRuntime _ _ _ _ _ mbMaskConfig _ _) -> mbMaskConfig
  _                                          -> Nothing

initLogCounter :: IO T.LogCounter
initLogCounter = newIORef 0

incLogCounter :: T.LogCounter -> IO Int
incLogCounter = flip atomicModifyIORef' (\cnt -> (cnt + 1, cnt))
