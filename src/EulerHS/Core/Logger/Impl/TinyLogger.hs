module EulerHS.Core.Logger.Impl.TinyLogger
  (
    -- * TinyLogger Implementation
    -- ** Types
    LoggerHandle
    -- ** Methods
  , sendPendingMsg
  , createLogger
  , createLogger'
  , createVoidLogger
  , disposeLogger
  , withLogger
  , defaultDateFormat
  , defaultRenderer
  , defaultBufferSize
  ) where

import           EulerHS.Prelude hiding ((.=))

import           Control.Concurrent (forkOn, getNumCapabilities)
import qualified Control.Concurrent.Chan.Unagi.Bounded as Chan
import qualified System.Logger as Log
import qualified System.Logger.Message as LogMsg

import qualified EulerHS.Core.Types as T

type LogQueue = (Chan.InChan T.PendingMsg, Chan.OutChan T.PendingMsg)

type Loggers = [Log.Logger]

data LoggerHandle
  = AsyncLoggerHandle [ThreadId] LogQueue Loggers
  | SyncLoggerHandle Loggers
  | VoidLoggerHandle

dispatchLogLevel :: T.LogLevel -> Log.Level
dispatchLogLevel T.Debug   = Log.Debug
dispatchLogLevel T.Info    = Log.Info
dispatchLogLevel T.Warning = Log.Warn
dispatchLogLevel T.Error   = Log.Error

logPendingMsg :: T.MessageFormatter -> Loggers -> T.PendingMsg -> IO ()
logPendingMsg formatter loggers pendingMsg@(T.PendingMsg lvl tag msg msgNum) = do
  let lvl' = dispatchLogLevel lvl
  let msg' = Log.msg (formatter pendingMsg)
  mapM_ (\logger -> Log.log logger lvl' msg') loggers

loggerWorker :: T.MessageFormatter -> Chan.OutChan T.PendingMsg -> Loggers -> IO ()
loggerWorker formatter outChan loggers = do
  pendingMsg <- Chan.readChan outChan
  logPendingMsg formatter loggers pendingMsg

sendPendingMsg :: T.MessageFormatter -> LoggerHandle -> T.PendingMsg -> IO ()
sendPendingMsg _ VoidLoggerHandle                    = const (pure ())
sendPendingMsg formatter (SyncLoggerHandle loggers)  = logPendingMsg formatter loggers
sendPendingMsg _ (AsyncLoggerHandle _ (inChan, _) _) = Chan.writeChan inChan

createVoidLogger :: IO LoggerHandle
createVoidLogger = pure VoidLoggerHandle

createLogger :: T.MessageFormatter -> T.LoggerConfig -> IO LoggerHandle
createLogger = createLogger' defaultDateFormat defaultRenderer defaultBufferSize

createLogger'
  :: Maybe Log.DateFormat
  -> Maybe Log.Renderer
  -> T.BufferSize
  -> T.MessageFormatter
  -> T.LoggerConfig
  -> IO LoggerHandle
createLogger'
  mbDateFormat
  mbRenderer
  bufferSize
  formatter
  (T.LoggerConfig isAsync _ logFileName isConsoleLog isFileLog maxQueueSize _) = do

    let fileSettings
          = Log.setFormat mbDateFormat
          $ maybe id Log.setRenderer mbRenderer
          $ Log.setBufSize bufferSize
          $ Log.setOutput (Log.Path logFileName)
          $ Log.defSettings

    let consoleSettings
          = Log.setFormat mbDateFormat
          $ maybe id Log.setRenderer mbRenderer
          $ Log.setBufSize bufferSize
          $ Log.setOutput Log.StdOut
          $ Log.defSettings

    let fileH    = [Log.new fileSettings    | isFileLog]
    let consoleH = [Log.new consoleSettings | isConsoleLog]
    let loggersH = fileH ++ consoleH

    when (not $ null loggersH) $
      if isAsync then putStrLn @String "Creating async loggers..."
                 else putStrLn @String "Creating sync loggers..."
    when isFileLog    $ putStrLn @String $ "Creating file logger (" +| logFileName |+ ")..."
    when isConsoleLog $ putStrLn @String "Creating console logger..."

    loggers <- sequence loggersH
    startLogger isAsync loggers
  where
    startLogger :: Bool -> Loggers -> IO LoggerHandle
    startLogger _ [] = pure VoidLoggerHandle
    startLogger False loggers = pure $ SyncLoggerHandle loggers
    startLogger True  loggers = do
      caps <- getNumCapabilities
      chan@(_, outChan) <- Chan.newChan (fromIntegral maxQueueSize)
      threadIds <- traverse ((flip forkOn) (forever $ loggerWorker formatter outChan loggers)) [1..caps]
      pure $ AsyncLoggerHandle threadIds chan loggers

disposeLogger :: T.MessageFormatter -> LoggerHandle -> IO ()
disposeLogger _ VoidLoggerHandle = pure ()
disposeLogger _ (SyncLoggerHandle loggers) = do
  putStrLn @String "Disposing sync logger..."
  mapM_ Log.flush loggers
  mapM_ Log.close loggers
disposeLogger formatter (AsyncLoggerHandle threadIds (_, outChan) loggers) = do
  putStrLn @String "Disposing async logger..."
  traverse_ killThread threadIds
  Chan.getChanContents outChan >>= mapM_ (logPendingMsg formatter loggers)
  mapM_ Log.flush loggers
  mapM_ Log.close loggers

withLogger'
  :: Maybe Log.DateFormat
  -> Maybe Log.Renderer
  -> T.BufferSize
  -> T.MessageFormatter
  -> T.LoggerConfig
  -> (LoggerHandle -> IO a)
  -> IO a
withLogger' mbDateFormat mbRenderer bufSize formatter cfg =
  bracket (createLogger' mbDateFormat mbRenderer bufSize formatter cfg) (disposeLogger formatter)

withLogger
  :: T.MessageFormatter
  -> T.LoggerConfig
  -> (LoggerHandle -> IO a)
  -> IO a
withLogger formatter cfg = bracket (createLogger formatter cfg) (disposeLogger formatter)

defaultBufferSize :: T.BufferSize
defaultBufferSize = 4096

defaultDateFormat :: Maybe Log.DateFormat
defaultDateFormat = Nothing

defaultRenderer :: Maybe Log.Renderer
defaultRenderer = Nothing
