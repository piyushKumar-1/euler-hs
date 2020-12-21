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
  , module X
  ) where

import           EulerHS.Prelude

-- Currently, TinyLogger is highly coupled with the Runtime.
-- Fix it if an interchangable implementations are needed.
import qualified EulerHS.Core.Logger.Impl.TinyLogger as Impl
import qualified EulerHS.Core.Types as T
import           EulerHS.Core.Types.DB as X (withTransaction)
import qualified System.Logger as Log

data LoggerRuntime
  = LoggerRuntime
      { _formatter       :: !T.MessageFormatter
      , _logLevel        :: T.LogLevel
      , _logRawSql       :: !Bool
      , _logCounter      :: !T.LogCounter
      , _logLoggerHandle :: Impl.LoggerHandle
      }
  | MemoryLoggerRuntime !T.MessageFormatter !T.LogLevel !(MVar [Text])

data CoreRuntime = CoreRuntime
    { _loggerRuntime :: LoggerRuntime
    }

createMemoryLoggerRuntime :: T.MessageFormatter -> T.LogLevel -> IO LoggerRuntime
createMemoryLoggerRuntime formatter logLevel =
  MemoryLoggerRuntime formatter logLevel <$> newMVar []

createLoggerRuntime :: T.MessageFormatter -> T.LoggerConfig -> IO LoggerRuntime
createLoggerRuntime formatter cfg = do
  counter <- initLogCounter
  LoggerRuntime formatter (T._logLevel cfg) (T._logRawSql cfg) counter
    <$> Impl.createLogger formatter cfg

createLoggerRuntime'
  :: Maybe Log.DateFormat
  -> Maybe Log.Renderer
  -> T.BufferSize
  -> T.MessageFormatter
  -> T.LoggerConfig
  -> IO LoggerRuntime
createLoggerRuntime' mbDateFormat mbRenderer bufferSize formatter cfg = do
  counter <- initLogCounter
  loggerHandle <- Impl.createLogger' mbDateFormat mbRenderer bufferSize formatter cfg
  pure $ LoggerRuntime formatter (T._logLevel cfg) (T._logRawSql cfg) counter loggerHandle

createVoidLoggerRuntime :: IO LoggerRuntime
createVoidLoggerRuntime = do
  counter <- initLogCounter
  LoggerRuntime show T.Debug True counter <$> Impl.createVoidLogger

clearLoggerRuntime :: LoggerRuntime -> IO ()
clearLoggerRuntime (LoggerRuntime formatter _ _ _ handle) = Impl.disposeLogger formatter handle
clearLoggerRuntime (MemoryLoggerRuntime _ _ msgsVar) = void $ swapMVar msgsVar []

createCoreRuntime :: LoggerRuntime -> IO CoreRuntime
createCoreRuntime = pure . CoreRuntime

clearCoreRuntime :: CoreRuntime -> IO ()
clearCoreRuntime _ = pure ()

shouldLogRawSql :: LoggerRuntime -> Bool
shouldLogRawSql = \case
  (LoggerRuntime _ _ logRawSql _ _) -> logRawSql
  _ -> True

initLogCounter :: IO T.LogCounter
initLogCounter = newIORef 0

incLogCounter :: T.LogCounter -> IO Int
incLogCounter = flip atomicModifyIORef' (\cnt -> (cnt + 1, cnt))
