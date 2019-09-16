module EulerHS.Core.Runtime where

import EulerHS.Prelude

import qualified EulerHS.Core.Types        as D
import qualified EulerHS.Core.Logger.Impl.TinyLogger as Impl

newtype LoggerRuntime = LoggerRuntime Impl.LoggerHandle


createLoggerRuntime :: D.LoggerConfig -> IO LoggerRuntime
createLoggerRuntime cfg = LoggerRuntime <$> Impl.createLogger cfg

createVoidLoggerRuntime :: IO LoggerRuntime
createVoidLoggerRuntime = LoggerRuntime <$> Impl.createVoidLogger

cleanLoggerRuntime :: LoggerRuntime -> IO ()
cleanLoggerRuntime (LoggerRuntime handle) = Impl.disposeLogger handle
