module EulerHS.Core.Logger.ImplMimicPSBad.TinyLogger
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

import           Control.Concurrent (forkOn, getNumCapabilities)
import qualified Control.Concurrent.Chan.Unagi.Bounded as Chan
import qualified System.Logger as Log
import qualified System.Logger.Message as LogMsg

import qualified EulerHS.Core.Types as D


import qualified Data.Aeson as Json
import qualified Data.Binary.Builder as BinaryBuilder
import qualified Data.ByteString.Builder as ByteStringBuilder
import qualified Data.ByteString.Lazy as BSL
import           Data.Text (Text)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL

-- TODO: remove this along with the whole impl
import qualified Data.Map as Map
import           System.Environment (getEnvironment)

type LogQueue = (Chan.InChan D.PendingMsg, Chan.OutChan D.PendingMsg)

type Loggers = [Log.Logger]

data LoggerHandle = AsyncLoggerHandle [ThreadId] LogQueue Loggers
    | SyncLoggerHandle Loggers
    | VoidLoggerHandle

dispatchLogLevel :: D.LogLevel -> Log.Level
dispatchLogLevel D.Debug   = Log.Debug
dispatchLogLevel D.Info    = Log.Info
dispatchLogLevel D.Warning = Log.Warn
dispatchLogLevel D.Error   = Log.Error

-- TODO: extract formatting from this module.
-- Make it configurable.
strMsg :: ByteString -> LogMsg.Msg -> LogMsg.Msg
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
loggerWorker outChan loggers = do
  pendingMsg <- Chan.readChan outChan
  logPendingMsg loggers pendingMsg

sendPendingMsg :: LoggerHandle -> D.PendingMsg -> IO ()
sendPendingMsg VoidLoggerHandle                    = const (pure ())
sendPendingMsg (SyncLoggerHandle loggers)          = logPendingMsgSync loggers
sendPendingMsg (AsyncLoggerHandle _ (inChan, _) _) = Chan.writeChan inChan

createVoidLogger :: IO LoggerHandle
createVoidLogger = pure VoidLoggerHandle

convBS :: LogMsg.Builder -> BSL.ByteString
convBS = ByteStringBuilder.toLazyByteString . LogMsg.builderBytes

convTextL :: LogMsg.Builder -> Text
convTextL = TL.toStrict . TL.decodeUtf8 . convBS

-- TODO: this is terrible quick version
-- would use Aeson.Value in the end
-- rewrite all this with katip or forked tinylog
jsonRenderer :: String -> String -> String -> Log.Renderer
jsonRenderer hostname env sourceCommit separator dateFormat logLevel fields =
  BinaryBuilder.fromByteString "{ timestamp = " <> quote <> timestamp <> quote <> commaSep <>
  BinaryBuilder.fromByteString "hostname = " <> quote <> fromString hostname <> quote <> commaSep <>
  BinaryBuilder.fromByteString "source_commit = " <> quote <> fromString sourceCommit <> quote <> commaSep <>
  BinaryBuilder.fromByteString invariant <>
  BinaryBuilder.fromByteString "message = " <> quote <> message <> quote <> commaSep <>
  BinaryBuilder.fromByteString "message_type = \"string\" }"
  where
    quote = BinaryBuilder.fromByteString "\""
    (timestamp, message) = case fields of
      [ts, _, t]    -> (elementToBS ts, elementToBS t)
      [ts, _, _, m] -> (elementToBS ts, elementToBS m)
      _             -> error "Malformed log fields."
    commaSep = BinaryBuilder.fromByteString ", "
    invariant = "level = \"info\", txn_uuid = \"null\", order_id = \"null\", x-request-id = \"null\", "
    elementToBS = LogMsg.builderBytes . \case
      LogMsg.Bytes b -> b
      LogMsg.Field k _ -> k
{-
jsonRenderer hostname env separator dateFormat logLevel fields =
  Json.fromEncoding $
    Json.pairs ("timestamp" Json..= (TL.decodeUtf8 timestamp)
               <> "hostname" Json..= hostname
               <> "env" Json..= env
               <> "level" Json..= ("info" :: Text)
               <> "txn_uuid" Json..= ("null" :: Text)
               <> "order_id" Json..= ("null" :: Text)
               <> "x-request-id" Json..= ("null" :: Text)
               <> "message" Json..= (TL.decodeUtf8 message)
               -- <> mconcat entryPairs
               <> "message_type" Json..= ("string" :: Text)
               )
    -- Json.toEncoding
  where
    logFields = map elementToBS $ take 4 fields
    [timestamp, _, tag, message] =
      if length logFields < 4
      then logFields ++ [tag]
      else logFields

    toPairs :: LogMsg.Element -> Json.Series
    toPairs (LogMsg.Bytes builder)   = (convTextL builder) Json..= ("FIXME" :: Text)
    toPairs (LogMsg.Field keyB valB) = (convTextL keyB) Json..= (convTextL valB)

    elementToBS :: LogMsg.Element -> BSL.ByteString
    elementToBS (LogMsg.Bytes builder)   = convBS builder
    elementToBS (LogMsg.Field keyB valB) = convBS keyB
-}

mimicEulerPSSettings :: String -> String -> String -> Log.Settings -> Log.Settings
mimicEulerPSSettings hostname env sourceCommit= Log.setFormat (Just dateFormat) . Log.setRenderer (jsonRenderer hostname env sourceCommit)
  where dateFormat = "%0d-%0m-%Y %0H:%0M:%0S.000"

-- TODO: errors -> stderr
createLogger :: D.LoggerConfig -> IO LoggerHandle
createLogger (D.LoggerConfig _ isAsync _ logFileName isConsoleLog isFileLog maxQueueSize) = do
    -- This is a temporary hack for euler-api-order deployment
    envVars <- Map.fromList <$> getEnvironment
    let hostname = maybe "NA" id $ Map.lookup "HOSTNAME" envVars
    let env = maybe "NA" id $ Map.lookup "NODE_ENV" envVars
    let sourceCommit = maybe "NA" id $ Map.lookup "SOURCE_COMMIT" envVars

    let consoleSettings =
          (mimicEulerPSSettings hostname env sourceCommit) . Log.setBufSize 4096 $
          Log.setOutput Log.StdOut Log.defSettings

    let fileSettings    =
          (mimicEulerPSSettings hostname env sourceCommit) . Log.setBufSize 4096 $
          Log.setOutput (Log.Path logFileName) Log.defSettings

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
