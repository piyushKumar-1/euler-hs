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

import qualified EulerHS.Core.Logger.ImplMimicPSBad.TinyLogger as Impl
import           EulerHS.Core.Types (LogCounter, LogLevel (..),
                                     LoggerConfig (..), TransientLoggerContext)
import           EulerHS.Core.Types.DB as X (withTransaction)

-- TODO: add StaticLoggerRuntimeContext if we'll need more than a single Bool
data LoggerRuntime
  = LoggerRuntime !LogLevel !Bool !LogCounter TransientLoggerContext TransientLoggerContext Impl.LoggerHandle
  | MemoryLoggerRuntime !LogLevel !(MVar [Text])

data CoreRuntime = CoreRuntime
    { _loggerRuntime :: LoggerRuntime
    }

createLoggerRuntime :: LoggerConfig -> IO LoggerRuntime
createLoggerRuntime (MemoryLoggerConfig cfgLogLevel) =
  MemoryLoggerRuntime cfgLogLevel <$> newMVar []
createLoggerRuntime cfg = do
  counter <- initLogCounter
  LoggerRuntime (_level cfg) (_logRawSql cfg) counter Nothing Nothing <$> Impl.createLogger cfg

createVoidLoggerRuntime :: IO LoggerRuntime
createVoidLoggerRuntime = do
  counter <- initLogCounter
  LoggerRuntime Debug True counter Nothing Nothing <$> Impl.createVoidLogger

clearLoggerRuntime :: LoggerRuntime -> IO ()
clearLoggerRuntime (LoggerRuntime _ _ _ _ _ handle) = Impl.disposeLogger handle
clearLoggerRuntime _                              = pure ()

createCoreRuntime :: LoggerRuntime -> IO CoreRuntime
createCoreRuntime = pure . CoreRuntime

clearCoreRuntime :: CoreRuntime -> IO ()
clearCoreRuntime _ = pure ()

-- TODO: change Maybe Text to some type
setTransientContext :: TransientLoggerContext -> TransientLoggerContext -> LoggerRuntime -> LoggerRuntime
setTransientContext ctx1 ctx2 = \case
  LoggerRuntime logLvl logRawSql counter _ _ handle -> LoggerRuntime logLvl logRawSql counter ctx1 ctx2 handle
  rt -> rt

clearTransientContext :: LoggerRuntime -> LoggerRuntime
clearTransientContext (LoggerRuntime logLvl logRawSql counter _ _ handle) =
  LoggerRuntime logLvl logRawSql counter Nothing Nothing handle
clearTransientContext rt = rt

shouldLogRawSql :: LoggerRuntime -> Bool
shouldLogRawSql = \case
  (LoggerRuntime _ logRawSql _ _ _ _) -> logRawSql
  _ -> True

initLogCounter :: IO LogCounter
initLogCounter = newIORef 0

incLogCounter :: LogCounter -> IO Int
incLogCounter = flip atomicModifyIORef' (\cnt -> (cnt + 1, cnt))
