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

import qualified System.Logger             as Log
import qualified System.Logger.Message     as LogMsg

import qualified EulerHS.Core.Types        as D

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
  let msg' = strMsg $ "[" +|| lvl ||+ "] <" +| tag |+ "> " +| msg |+ ""
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

-- TODO: errors -> stderr
createLogger :: D.LoggerConfig -> IO LoggerHandle
createLogger (D.LoggerConfig _ isAsync _ logFileName isConsoleLog isFileLog) = do
    let consoleSettings = Log.setBufSize 1 $ Log.setOutput Log.StdOut Log.defSettings
    let fileSettings    = Log.setBufSize 1 $ Log.setOutput (Log.Path logFileName) Log.defSettings
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
    startLogger False loggers = SyncLoggerHandle <$> newMVar loggers
    startLogger True  loggers = do
      chan <- newTChanIO
      threadId <- forkIO $ forever $ loggerWorker chan loggers
      pure $ AsyncLoggerHandle threadId chan loggers

disposeLogger :: LoggerHandle -> IO ()
disposeLogger VoidLoggerHandle = pure ()
disposeLogger (SyncLoggerHandle loggersVar) = do
  putStrLn @String "Disposing sync logger..."
  loggers <- takeMVar loggersVar
  mapM_ Log.flush loggers
  mapM_ Log.close loggers
  putMVar loggersVar loggers
disposeLogger (AsyncLoggerHandle threadId chan loggers) = do
  putStrLn @String "Disposing async logger..."
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
