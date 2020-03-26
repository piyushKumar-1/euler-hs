module EulerHS.Core.Runtime
  (
    -- * Core Runtime
    CoreRuntime(..)
  , LoggerRuntime(..)
  , createCoreRuntime
  , createVoidLoggerRuntime
  , createLoggerRuntime
  , clearCoreRuntime
  , clearLoggerRuntime
  , module X
  ) where

import           EulerHS.Prelude

import qualified EulerHS.Core.Logger.Impl.TinyLogger as Impl
import           EulerHS.Core.Types (LoggerConfig (..))
import           EulerHS.Core.Types.DB as X (withTransaction)

data LoggerRuntime
  = LoggerRuntime Impl.LoggerHandle
  | MemoryLoggerRuntime !(MVar [Text])

data CoreRuntime = CoreRuntime
  { _loggerRuntime :: LoggerRuntime
  }

createLoggerRuntime :: LoggerConfig -> IO LoggerRuntime
createLoggerRuntime MemoryLoggerConfig = MemoryLoggerRuntime <$> newMVar []
createLoggerRuntime cfg                = LoggerRuntime <$> Impl.createLogger cfg

createVoidLoggerRuntime :: IO LoggerRuntime
createVoidLoggerRuntime = LoggerRuntime <$> Impl.createVoidLogger

clearLoggerRuntime :: LoggerRuntime -> IO ()
clearLoggerRuntime (LoggerRuntime handle) = Impl.disposeLogger handle
clearLoggerRuntime _ = pure ()

createCoreRuntime :: LoggerRuntime -> IO CoreRuntime
createCoreRuntime = pure . CoreRuntime

clearCoreRuntime :: CoreRuntime -> IO ()
clearCoreRuntime _ = pure ()
