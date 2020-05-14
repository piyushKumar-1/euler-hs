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

import qualified EulerHS.Core.Logger.ImplMimicPSBad.TinyLogger as Impl
import           EulerHS.Core.Types                            (LogLevel (..), LoggerConfig (..))
import           EulerHS.Core.Types.DB                         as X (withTransaction)

data LoggerRuntime = LoggerRuntime !LogLevel Impl.LoggerHandle
    | MemoryLoggerRuntime !LogLevel !(MVar [Text])

data CoreRuntime = CoreRuntime
    { _loggerRuntime :: LoggerRuntime
    }

createLoggerRuntime :: LoggerConfig -> IO LoggerRuntime
createLoggerRuntime (MemoryLoggerConfig cfgLogLevel) = MemoryLoggerRuntime cfgLogLevel <$> newMVar []
createLoggerRuntime cfg                = LoggerRuntime (_level cfg) <$> Impl.createLogger cfg

createVoidLoggerRuntime :: IO LoggerRuntime
createVoidLoggerRuntime = LoggerRuntime Debug <$> Impl.createVoidLogger

clearLoggerRuntime :: LoggerRuntime -> IO ()
clearLoggerRuntime (LoggerRuntime _ handle) = Impl.disposeLogger handle
clearLoggerRuntime _                        = pure ()

createCoreRuntime :: LoggerRuntime -> IO CoreRuntime
createCoreRuntime = pure . CoreRuntime

clearCoreRuntime :: CoreRuntime -> IO ()
clearCoreRuntime _ = pure ()
