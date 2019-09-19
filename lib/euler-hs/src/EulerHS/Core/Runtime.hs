module EulerHS.Core.Runtime where

import EulerHS.Prelude

import qualified EulerHS.Core.Types        as D
import qualified EulerHS.Core.Logger.Impl.TinyLogger as Impl

newtype LoggerRuntime = LoggerRuntime Impl.LoggerHandle

data CoreRuntime = CoreRuntime
  { _loggerRuntime :: LoggerRuntime

  }

createLoggerRuntime :: D.LoggerConfig -> IO LoggerRuntime
createLoggerRuntime cfg = LoggerRuntime <$> Impl.createLogger cfg

createVoidLoggerRuntime :: IO LoggerRuntime
createVoidLoggerRuntime = LoggerRuntime <$> Impl.createVoidLogger

clearLoggerRuntime :: LoggerRuntime -> IO ()
clearLoggerRuntime (LoggerRuntime handle) = Impl.disposeLogger handle


createCoreRuntime :: LoggerRuntime -> IO CoreRuntime
createCoreRuntime = pure . CoreRuntime

clearCoreRuntime :: CoreRuntime -> IO ()
clearCoreRuntime _ = pure ()
