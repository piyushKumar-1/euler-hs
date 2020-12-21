module EulerHS.Core.Runtime
  (
    -- * Core Runtime
    CoreRuntime(..)
  , LoggerRuntime(..)
  , TransientLoggerContext
  , setTransientContext
  , shouldLogRawSql
  , clearTransientContext
  , incLogCounter
  , createCoreRuntime
  , createVoidLoggerRuntime
  , createLoggerRuntime
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

-- TODO: add StaticLoggerRuntimeContext if we'll need more than a single Bool
data LoggerRuntime
  = LoggerRuntime
      { _formatter                   :: !MessageFormatter
      , _level                       :: T.LogLevel
      , _logRawSql                   :: !Bool
      , _logCounter                  :: !T.LogCounter
      , _logLoggerHandle             :: Impl.LoggerHandle
      }
  | MemoryLoggerRuntime !MessageFormatter !T.LogLevel !(MVar [Text])

data CoreRuntime = CoreRuntime
    { _loggerRuntime :: LoggerRuntime
    }

createLoggerRuntime :: T.LoggerConfig -> IO LoggerRuntime
createLoggerRuntime (T.MemoryLoggerConfig formatter cfgLogLevel) =
  T.MemoryLoggerRuntime formatter cfgLogLevel <$> newMVar []
createLoggerRuntime cfg = do
  counter <- initLogCounter
  T.LoggerRuntime formatter (T._level cfg) (T._logRawSql cfg) counter
    <$> Impl.createLogger cfg

createVoidLoggerRuntime :: IO LoggerRuntime
createVoidLoggerRuntime = do
  counter <- initLogCounter
  T.LoggerRuntime show Debug True counter <$> Impl.createVoidLogger

clearLoggerRuntime :: LoggerRuntime -> IO ()
clearLoggerRuntime (LoggerRuntime _ _ _ _ handle) = Impl.disposeLogger handle
clearLoggerRuntime _                              = pure ()

createCoreRuntime :: LoggerRuntime -> IO CoreRuntime
createCoreRuntime = pure . CoreRuntime

clearCoreRuntime :: CoreRuntime -> IO ()
clearCoreRuntime _ = pure ()

shouldLogRawSql :: LoggerRuntime -> Bool
shouldLogRawSql = \case
  (LoggerRuntime _ _ logRawSql _ _) -> logRawSql
  _ -> True

initLogCounter :: IO LogCounter
initLogCounter = newIORef 0

incLogCounter :: LogCounter -> IO Int
incLogCounter = flip atomicModifyIORef' (\cnt -> (cnt + 1, cnt))
