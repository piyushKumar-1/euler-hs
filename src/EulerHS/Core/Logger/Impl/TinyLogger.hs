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

import           EulerHS.Prelude

import Control.Concurrent (forkOn, getNumCapabilities)
import qualified Control.Concurrent.Chan.Unagi.Bounded as Chan
import qualified System.Logger             as Log
import qualified System.Logger.Message     as LogMsg

import qualified EulerHS.Core.Types        as D

type LogQueue = (Chan.InChan D.PendingMsg, Chan.OutChan D.PendingMsg)

type Loggers = [Log.Logger]

data LoggerHandle
  = AsyncLoggerHandle [ThreadId] LogQueue Loggers
  | SyncLoggerHandle Loggers
  | VoidLoggerHandle

dispatchLogLevel :: D.LogLevel -> Log.Level
dispatchLogLevel D.Debug   = Log.Debug
dispatchLogLevel D.Info    = Log.Info
dispatchLogLevel D.Warning = Log.Warn
dispatchLogLevel D.Error   = Log.Error

-- TODO: extract formatting from this module.
-- Make it configurable.
strMsg :: String -> LogMsg.Msg -> LogMsg.Msg
strMsg = Log.msg

logPendingMsg :: Loggers -> D.PendingMsg -> IO ()
logPendingMsg loggers (D.PendingMsg lvl tag msg) = do
  let lvl' = dispatchLogLevel lvl
  let msg' = strMsg $ "[" +|| lvl ||+ "] <" +| tag |+ "> " +| msg |+ ""
  mapM_ (\logger -> Log.log logger lvl' msg') loggers

logPendingMsgSync :: Loggers -> D.PendingMsg -> IO ()
logPendingMsgSync loggers pendingMsg = do
  logPendingMsg loggers pendingMsg

loggerWorker :: Chan.OutChan D.PendingMsg -> Loggers -> IO ()
loggerWorker queue loggers = do
  pendingMsg <- Chan.readChan queue
  logPendingMsg loggers pendingMsg

sendPendingMsg :: LoggerHandle -> D.PendingMsg -> IO ()
sendPendingMsg VoidLoggerHandle = const (pure ())
sendPendingMsg (SyncLoggerHandle loggers) = logPendingMsgSync loggers
sendPendingMsg (AsyncLoggerHandle _ (inChan, _) _) = Chan.writeChan inChan

createVoidLogger :: IO LoggerHandle
createVoidLogger = pure VoidLoggerHandle

-- TODO: errors -> stderr
createLogger :: D.LoggerConfig -> IO LoggerHandle
createLogger (D.LoggerConfig _ isAsync _ logFileName isConsoleLog isFileLog maxQueueSize) = do
    let consoleSettings = Log.setBufSize 4096 $ Log.setOutput Log.StdOut Log.defSettings
    let fileSettings    = Log.setBufSize 4096 $ Log.setOutput (Log.Path logFileName) Log.defSettings
    let fileH           = [Log.new fileSettings    | isFileLog]
    let consoleH        = [Log.new consoleSettings | isConsoleLog]
    let loggersH        = fileH ++ consoleH
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
      threadIds <- traverse ((flip forkOn) (forever $ loggerWorker outChan loggers)) [1..caps]
      pure $ AsyncLoggerHandle threadIds chan loggers
createLogger cfg = error $ "Unknown logger config: " <> show cfg

disposeLogger :: LoggerHandle -> IO ()
disposeLogger VoidLoggerHandle = pure ()
disposeLogger (SyncLoggerHandle loggers) = do
  putStrLn @String "Disposing sync logger..."
  mapM_ Log.flush loggers
  mapM_ Log.close loggers
disposeLogger (AsyncLoggerHandle threadIds (_, outChan) loggers) = do
  putStrLn @String "Disposing async logger..."
  traverse_ killThread threadIds
  Chan.getChanContents outChan >>= mapM_ (logPendingMsg loggers)
  mapM_ Log.flush loggers
  mapM_ Log.close loggers

withLogger :: D.LoggerConfig -> (LoggerHandle -> IO a) -> IO a
withLogger config = bracket (createLogger config) disposeLogger
