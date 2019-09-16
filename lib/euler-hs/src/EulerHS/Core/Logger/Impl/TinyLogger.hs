module EulerHS.Core.Logger.Impl.TinyLogger
  ( LoggerHandle
  , sendPendingMsg
  , createLogger
  , createVoidLogger
  , disposeLogger
  , withLogger
  ,
  ) where

import           EulerHS.Prelude

import qualified System.Logger             as Log
import qualified System.Logger.Message     as LogMsg
import           System.Logger.Message     ((~~))

import qualified EulerHS.Core.Types        as D
import qualified EulerHS.Core.Language     as L

type LogChan = TChan D.PendingMsg

type Loggers = [Log.Logger]

data LoggerHandle
  = AsyncLoggerHandle ThreadId LogChan Loggers
  | SyncLoggerHandle (MVar Loggers)
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
  let msg'
         = strMsg "["  ~~ strMsg (show lvl) ~~ strMsg "]"
        ~~ strMsg " <" ~~ Log.msg tag        ~~ strMsg ">"
        ~~ strMsg " "  ~~ Log.msg msg
  mapM_ (\logger -> Log.log logger lvl' msg') loggers
  mapM_ Log.flush loggers

logPendingMsgSync :: MVar Loggers -> D.PendingMsg -> IO ()
logPendingMsgSync loggersVar pendingMsg = do
  loggers <- takeMVar loggersVar
  logPendingMsg loggers pendingMsg
  putMVar loggersVar loggers

loggerWorker :: LogChan -> Loggers -> IO ()
loggerWorker chan loggers = do
  pendingMsg <- atomically $ readTChan chan
  logPendingMsg loggers pendingMsg

sendPendingMsg :: LoggerHandle -> D.PendingMsg -> IO ()
sendPendingMsg VoidLoggerHandle = const (pure ())
sendPendingMsg (SyncLoggerHandle loggersVar) = logPendingMsgSync loggersVar
sendPendingMsg (AsyncLoggerHandle _ chan _)  = atomically . writeTChan chan

createVoidLogger :: IO LoggerHandle
createVoidLogger = pure VoidLoggerHandle

createLogger :: D.LoggerConfig -> IO LoggerHandle
createLogger (D.LoggerConfig format isAsync level logFileName isConsoleLog isFileLog) = do
    let consoleSettings = Log.setBufSize 1 $ Log.setOutput Log.StdOut Log.defSettings
    let fileSettings    = Log.setBufSize 1 $ Log.setOutput (Log.Path logFileName) Log.defSettings
    let fileH           = [Log.new fileSettings    | isFileLog]
    let consoleH        = [Log.new consoleSettings | isConsoleLog]
    loggers <- sequence $ fileH ++ consoleH
    startLogger isAsync loggers
  where
    startLogger :: Bool -> Loggers -> IO LoggerHandle
    startLogger False loggers = SyncLoggerHandle <$> newMVar loggers
    startLogger True  loggers = do
      chan <- newTChanIO
      threadId <- forkIO $ forever $ loggerWorker chan loggers
      pure $ AsyncLoggerHandle threadId chan loggers

disposeLogger :: LoggerHandle -> IO ()
disposeLogger VoidLoggerHandle = pure ()
disposeLogger (SyncLoggerHandle loggersVar) = do
  loggers <- takeMVar loggersVar
  mapM_ Log.flush loggers
  mapM_ Log.close loggers
  putMVar loggersVar loggers
disposeLogger (AsyncLoggerHandle threadId chan loggers) = do
  killThread threadId
  flushRest
  mapM_ Log.flush loggers
  mapM_ Log.close loggers
  where
    flushRest = do
      mbPendingMsg <- atomically $ tryReadTChan chan
      case mbPendingMsg of
        Nothing -> pure ()
        Just pendingMsg -> do
          logPendingMsg loggers pendingMsg
          flushRest

withLogger :: D.LoggerConfig -> (LoggerHandle -> IO a) -> IO a
withLogger config = bracket (createLogger config) disposeLogger
