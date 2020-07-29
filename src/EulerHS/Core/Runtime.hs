module EulerHS.Core.Runtime
  (
    -- * Core Runtime
    CoreRuntime(..)
  , LoggerRuntime(..)
  , TransientLoggerContext
  , setTransientContext
  , clearTransientContext
  , createCoreRuntime
  , createVoidLoggerRuntime
  , createLoggerRuntime
  , clearCoreRuntime
  , clearLoggerRuntime
  , module X
  ) where

import           EulerHS.Prelude

import qualified EulerHS.Core.Logger.ImplMimicPSBad.TinyLogger as Impl
import           EulerHS.Core.Types (LogLevel (..), LoggerConfig (..), TransientLoggerContext)
import           EulerHS.Core.Types.DB as X (withTransaction)

data LoggerRuntime = LoggerRuntime !LogLevel TransientLoggerContext Impl.LoggerHandle
    | MemoryLoggerRuntime !LogLevel !(MVar [Text])

data CoreRuntime = CoreRuntime
    { _loggerRuntime :: LoggerRuntime
    }

createLoggerRuntime :: LoggerConfig -> IO LoggerRuntime
createLoggerRuntime (MemoryLoggerConfig cfgLogLevel) = MemoryLoggerRuntime cfgLogLevel <$> newMVar []
createLoggerRuntime cfg                = LoggerRuntime (_level cfg) Nothing <$> Impl.createLogger cfg

createVoidLoggerRuntime :: IO LoggerRuntime
createVoidLoggerRuntime = LoggerRuntime Debug Nothing <$> Impl.createVoidLogger

clearLoggerRuntime :: LoggerRuntime -> IO ()
clearLoggerRuntime (LoggerRuntime _ _ handle) = Impl.disposeLogger handle
clearLoggerRuntime _                        = pure ()

createCoreRuntime :: LoggerRuntime -> IO CoreRuntime
createCoreRuntime = pure . CoreRuntime

clearCoreRuntime :: CoreRuntime -> IO ()
clearCoreRuntime _ = pure ()

-- TODO: change Maybe Text to some type
setTransientContext :: TransientLoggerContext -> LoggerRuntime -> LoggerRuntime
setTransientContext ctx = \case
  LoggerRuntime logLvl _ handle -> LoggerRuntime logLvl ctx handle
  rt -> rt

clearTransientContext :: LoggerRuntime -> LoggerRuntime
clearTransientContext (LoggerRuntime logLvl _ handle) = LoggerRuntime logLvl Nothing handle
clearTransientContext rt = rt
