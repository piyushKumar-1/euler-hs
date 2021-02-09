module EulerHS.Core.Runtime
  (
    -- * Core Runtime
    CoreRuntime(..)
  , LoggerRuntime(..)
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
import qualified EulerHS.Core.Logger.Impl.TinyLogger as Impl
import qualified EulerHS.Core.Types as T
import           EulerHS.Core.Types.DB as X (withTransaction)
import qualified System.Logger as Log


-- TODO: add StaticLoggerRuntimeContext if we'll need more than a single Bool
data LoggerRuntime
  = LoggerRuntime
    { _flowFormatter    :: T.FlowFormatter
    , _logLevel         :: T.LogLevel
    , _logRawSql        :: !Bool
    , _logCounter       :: !T.LogCounter
    , _logMaskingConfig :: Maybe T.LogMaskingConfig
    , _logLoggerHandle  :: Impl.LoggerHandle
    }
  | MemoryLoggerRuntime !T.FlowFormatter !T.LogLevel !(MVar [Text]) !T.LogCounter

data CoreRuntime = CoreRuntime
    { _loggerRuntime :: LoggerRuntime
    }

-- createLoggerRuntime :: LoggerConfig -> IO LoggerRuntime
-- createLoggerRuntime (MemoryLoggerConfig cfgLogLevel) =
--   MemoryLoggerRuntime cfgLogLevel <$> newMVar []
-- createLoggerRuntime cfg = do
--   counter <- initLogCounter
--   LoggerRuntime (_level cfg) (_logRawSql cfg) counter Nothing Nothing (_logMaskingConfig cfg)<$> Impl.createLogger cfg

createMemoryLoggerRuntime :: T.FlowFormatter -> T.LogLevel -> IO LoggerRuntime
createMemoryLoggerRuntime flowFormatter logLevel =
  MemoryLoggerRuntime flowFormatter logLevel <$> newMVar [] <*> initLogCounter

createLoggerRuntime :: T.FlowFormatter -> T.LoggerConfig -> IO LoggerRuntime
createLoggerRuntime flowFormatter cfg = do
  counter <- initLogCounter
  LoggerRuntime flowFormatter (T._logLevel cfg) (T._logRawSql cfg) counter Nothing
    <$> Impl.createLogger flowFormatter cfg

createLoggerRuntime'
  :: Maybe Log.DateFormat
  -> Maybe Log.Renderer
  -> T.BufferSize
  -> T.FlowFormatter
  -> T.LoggerConfig
  -> IO LoggerRuntime
createLoggerRuntime' mbDateFormat mbRenderer bufferSize flowFormatter cfg = do
  counter <- initLogCounter
  loggerHandle <- Impl.createLogger' mbDateFormat mbRenderer bufferSize flowFormatter cfg
  pure $ LoggerRuntime flowFormatter (T._logLevel cfg) (T._logRawSql cfg) counter Nothing loggerHandle

createVoidLoggerRuntime :: IO LoggerRuntime
createVoidLoggerRuntime = do
  counter <- initLogCounter
  LoggerRuntime (const $ pure T.showingMessageFormatter) T.Debug True counter Nothing <$> Impl.createVoidLogger


clearLoggerRuntime :: LoggerRuntime -> IO ()
clearLoggerRuntime (LoggerRuntime flowFormatter _ _ _ _ handle) = Impl.disposeLogger flowFormatter handle
clearLoggerRuntime (MemoryLoggerRuntime _ _ msgsVar _) = void $ swapMVar msgsVar []

createCoreRuntime :: LoggerRuntime -> IO CoreRuntime
createCoreRuntime = pure . CoreRuntime

clearCoreRuntime :: CoreRuntime -> IO ()
clearCoreRuntime _ = pure ()

shouldLogRawSql :: LoggerRuntime -> Bool
shouldLogRawSql = \case
  (LoggerRuntime _ _ logRawSql _ _ _) -> logRawSql
  _ -> True

getLogMaskingConfig :: LoggerRuntime -> Maybe T.LogMaskingConfig
getLogMaskingConfig = \case
  (LoggerRuntime _ _ _ _ mbMaskConfig _) -> mbMaskConfig
  _ -> Nothing

initLogCounter :: IO T.LogCounter
initLogCounter = newIORef 0

incLogCounter :: T.LogCounter -> IO Int
incLogCounter = flip atomicModifyIORef' (\cnt -> (cnt + 1, cnt))
