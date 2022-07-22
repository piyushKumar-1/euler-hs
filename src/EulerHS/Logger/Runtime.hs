{-# OPTIONS_GHC -Wno-incomplete-record-updates #-} -- due to RDP - Koz

module EulerHS.Logger.Runtime
  (
    -- * Core Runtime
    CoreRuntime(..)
  , LoggerRuntime(..)
  , SeverityCounterHandle(..)
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
    { _flowFormatter          :: T.FlowFormatter
    , _logContext             :: MVar T.LogContext
    , _logLevel               :: T.LogLevel
    , _logRawSql              :: T.ShouldLogSQL
    , _logCounter             :: !T.LogCounter
    , _logMaskingConfig       :: Maybe T.LogMaskingConfig
    , _logLoggerHandle        :: Impl.LoggerHandle
    , _severityCounterHandle  :: Maybe SeverityCounterHandle
    }
  | MemoryLoggerRuntime
      !T.FlowFormatter
      !(MVar T.LogContext)
      !T.LogLevel
      !(MVar [Text])
      !T.LogCounter

newtype CoreRuntime = CoreRuntime
    { _loggerRuntime :: LoggerRuntime
    }

-- | Log entry counter by severity handle.
data SeverityCounterHandle = SeverityCounterHandle
  { incCounter :: T.LogLevel -> IO ()
  }

createMemoryLoggerRuntime :: T.FlowFormatter -> T.LogLevel -> IO LoggerRuntime
createMemoryLoggerRuntime flowFormatter logLevel = do 
  emptyLoggerCtx <- newMVar mempty
  MemoryLoggerRuntime flowFormatter emptyLoggerCtx logLevel <$> newMVar [] <*> initLogCounter

createLoggerRuntime
  :: T.FlowFormatter
  -> Maybe SeverityCounterHandle
  -> T.LoggerConfig
  -> IO LoggerRuntime
createLoggerRuntime flowFormatter severityCounterHandler cfg = do
  -- log entries' sequential number
  logSequence <- initLogCounter
  logHandle <- Impl.createLogger flowFormatter cfg
  emptyLoggerCtx <- newMVar mempty
  pure $ LoggerRuntime
    flowFormatter
    emptyLoggerCtx
    (T._logLevel cfg)
    (T._logRawSql cfg)
    logSequence
    Nothing
    logHandle
    severityCounterHandler

createLoggerRuntime' :: Maybe Log.DateFormat
  -> Maybe Log.Renderer
  -> T.BufferSize
  -> T.FlowFormatter
  -> Maybe SeverityCounterHandle
  -> T.LoggerConfig
  -> IO LoggerRuntime
createLoggerRuntime' mbDateFormat mbRenderer bufferSize flowFormatter severityCounterHandler cfg = do
  -- log entries' sequential number
  logSequence <- initLogCounter
  loggerHandle <- Impl.createLogger' mbDateFormat mbRenderer bufferSize flowFormatter cfg
  emptyLoggerCtx <- newMVar mempty
  pure $ LoggerRuntime
    flowFormatter
    emptyLoggerCtx
    (T._logLevel cfg)
    (T._logRawSql cfg)
    logSequence
    (T._logMaskingConfig cfg)
    loggerHandle
    severityCounterHandler

createVoidLoggerRuntime :: IO LoggerRuntime
createVoidLoggerRuntime = do
  -- log entries' sequential number
  logSequence <- initLogCounter
  logHandle <- Impl.createVoidLogger
  emptyLoggerCtx <- newMVar mempty
  pure $ LoggerRuntime
    (const $ pure T.showingMessageFormatter)
    emptyLoggerCtx
    T.Debug
    T.SafelyOmitSqlLogs
    logSequence
    Nothing
    logHandle
    Nothing

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
