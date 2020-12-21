module EulerHS.Core.Logger.Impl.TinyLogger
  (
    -- * TinyLogger Implementation
    -- ** Types
    LoggerHandle
    -- ** Methods
  , sendPendingMsg
  , createLogger
  , createVoidLogger
  , disposeLogger
  , withLogger
  ,
  ) where

import           EulerHS.Prelude hiding ((.=))

import           Control.Concurrent (forkOn, getNumCapabilities)
import qualified Control.Concurrent.Chan.Unagi.Bounded as Chan
import qualified System.Logger as Log
import qualified System.Logger.Message as LogMsg

import qualified EulerHS.Core.Types as D

type LogQueue = (Chan.InChan D.PendingMsg, Chan.OutChan D.PendingMsg)

data Loggers = [Log.Logger]

data LoggerHandle
  = AsyncLoggerHandle [ThreadId] LogQueue Loggers
  | SyncLoggerHandle Loggers
  | VoidLoggerHandle

dispatchLogLevel :: D.LogLevel -> Log.Level
dispatchLogLevel D.Debug   = Log.Debug
dispatchLogLevel D.Info    = Log.Info
dispatchLogLevel D.Warning = Log.Warn
dispatchLogLevel D.Error   = Log.Error

logPendingMsg :: MessageFormatter -> Loggers -> D.PendingMsg -> IO ()
logPendingMsg formatter loggers pendingMsg@(D.PendingMsg lvl tag msg msgNum) = do
  let lvl' = dispatchLogLevel lvl
  let msg' = formatter pendingMsg
  mapM_ (\logger -> Log.log logger lvl' msg') loggers

loggerWorker :: MessageFormatter -> Chan.OutChan D.PendingMsg -> Loggers -> IO ()
loggerWorker formatter outChan loggers = do
  pendingMsg <- Chan.readChan outChan
  logPendingMsg formatter loggers pendingMsg

sendPendingMsg :: MessageFormatter -> LoggerHandle -> D.PendingMsg -> IO ()
sendPendingMsg _ VoidLoggerHandle                    = const (pure ())
sendPendingMsg formatter (SyncLoggerHandle loggers)  = logPendingMsg formatter loggers
sendPendingMsg _ (AsyncLoggerHandle _ (inChan, _) _) = Chan.writeChan inChan

createVoidLogger :: IO LoggerHandle
createVoidLogger = pure VoidLoggerHandle

createLogger :: D.LoggerConfig -> IO LoggerHandle
createLogger = createLogger' defaultDateFormat defaultRenderer defaultBufferSize

createLogger'
  :: Maybe DateFormat
  -> Maybe Log.Renderer
  -> BufferSize
  -> D.LoggerConfig
  -> IO LoggerHandle
createLogger'
  mbDateFormat
  mbRenderer
  bufferSize
  (D.LoggerConfig _ formatter isAsync _ logFileName isConsoleLog isFileLog maxQueueSize _) = do

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
createLogger cfg = error $ "Unknown logger config: " <> show cfg

disposeLogger :: LoggerHandle -> IO ()
disposeLogger VoidLoggerHandle = pure ()
disposeLogger (SyncLoggerHandle _ loggers) = do
  putStrLn @String "Disposing sync logger..."
  mapM_ Log.flush loggers
  mapM_ Log.close loggers
disposeLogger (AsyncLoggerHandle threadIds (_, outChan) loggers) = do
  putStrLn @String "Disposing async logger..."
  traverse_ killThread threadIds
  Chan.getChanContents outChan >>= mapM_ (logPendingMsg loggers)
  mapM_ Log.flush loggers
  mapM_ Log.close loggers

withLogger'
  :: Maybe DateFormat
  -> Maybe Log.Renderer
  -> BufferSize
  -> D.LoggerConfig -> (LoggerHandle -> IO a) -> IO a
withLogger' mbDateFormat mbRenderer bufSize cfg =
  bracket (createLogger' mbDateFormat mbRenderer bufSize cfg) disposeLogger

withLogger :: D.LoggerConfig -> (LoggerHandle -> IO a) -> IO a
withLogger config = bracket (createLogger config) disposeLogger

defaultBufferSize :: BufferSize
defaultBufferSize = 4096

defaultDateFormat :: Maybe DateFormat
defaultDateFormat = Nothing

defaultRenderer :: Maybe Log.Renderer
defaultRenderer = Nothing
