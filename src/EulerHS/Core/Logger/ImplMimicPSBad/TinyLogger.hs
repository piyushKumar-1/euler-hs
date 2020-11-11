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

import           EulerHS.Prelude hiding ((.=))

import           Control.Concurrent (forkOn, getNumCapabilities)
import qualified Control.Concurrent.Chan.Unagi.Bounded as Chan
import           System.Logger ((.=), (~~))
import qualified System.Logger as Log
import qualified System.Logger.Message as LogMsg

import qualified EulerHS.Core.Types as D

import qualified Data.Aeson as Json
import qualified Data.Binary.Builder as BinaryBuilder
import qualified Data.ByteString.Builder as ByteStringBuilder
import qualified Data.ByteString.Lazy as BSL
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL

-- TODO: remove this along with the whole impl
import qualified Data.Map as Map
import           Mason.Builder as Mason
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
logPendingMsg loggers (D.PendingMsg lvl tag msg msgNum ctx1 ctx2 ) = do
  let lvl' = dispatchLogLevel lvl
  let eulerMsg = ("[" +|| lvl ||+ "] <" +| tag |+ "> " +| msg |+ ""); eulerMsg :: Text
  let ctxVal1 = fromMaybe "null" ctx1
  let ctxVal2 = fromMaybe "null" ctx2
  let escapedMsg = T.tail . T.init $ show eulerMsg
  let msg'' =
        (Log.field @ByteString "message" (T.encodeUtf8 escapedMsg))
        ~~ (Log.field @Int "message_number" msgNum)
        ~~ ("x-request-id" .= ctxVal1)
        ~~ ("x-global-request-id" .= ctxVal2)
  mapM_ (\logger -> Log.log logger lvl' msg'') loggers

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

-- type Renderer = ByteString -> DateFormat -> Level -> [Element] -> Builder
-- we've rewritten this to use mason, hopefully it's a performance boost
jsonRenderer :: String -> String -> String -> Log.Renderer
jsonRenderer hostname env sourceCommit _separator dateFormat logLevel loggerFields 
  = mbuilder
  where
    mbuilder :: BinaryBuilder.Builder
    mbuilder = 
      BinaryBuilder.fromLazyByteString "{\"timestamp\": " <> quote <> timestamp <> quote <> commaSep <>
      BinaryBuilder.fromLazyByteString "\"app_framework\": " <> quote <> "euler-hs-application" <> quote <> commaSep <>
      BinaryBuilder.fromLazyByteString "\"hostname\": " <> quote <> fromString hostname <> quote <> commaSep <>
      BinaryBuilder.fromLazyByteString "\"source_commit\": " <> quote <> fromString sourceCommit <> quote <> commaSep <>
      BinaryBuilder.fromLazyByteString invariant <>
      mconcat (map elementToBS fields) <>
      -- BinaryBuilder.fromByteString "\"message\" = " <> quote <> message <> quote <> commaSep <>
      BinaryBuilder.fromLazyByteString "\"message_type\": \"string\"}"
    colon = BinaryBuilder.fromLazyByteString ": "
    quote = BinaryBuilder.fromLazyByteString "\""
    commaSep = BinaryBuilder.fromLazyByteString ", "
    jsonField k v = quote <> k <> quote <> colon <> quote <> v <> quote <> commaSep
    (timestamp, fields) = case loggerFields of
      (ts:_lvl:rest) -> (elementToBS ts, rest)
      _              -> error "Malformed log fields."
    invariant = "\"level\": \"info\", \"txn_uuid\": \"null\", \"order_id\": \"null\", \"refund_id\": \"null\", \"refund_unique_request_id\": \"null\", \"merchant_id\": \"null\", " -- x-request-id = \"null\", "
    elementToBS = \case
      LogMsg.Bytes b -> LogMsg.builderBytes b
      LogMsg.Field k v -> jsonField (LogMsg.builderBytes k) (LogMsg.builderBytes v)

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
mimicEulerPSSettings hostname env sourceCommit = Log.setFormat (Just dateFormat) . Log.setRenderer (jsonRenderer hostname env sourceCommit)
  where dateFormat = "%0d-%0m-%Y %0H:%0M:%0S.000"

-- TODO: errors -> stderr
createLogger :: D.LoggerConfig -> IO LoggerHandle
createLogger (D.LoggerConfig _ isAsync _ logFileName isConsoleLog isFileLog maxQueueSize _) = do
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

    let loggersH           = [Log.new fileSettings    | isFileLog, Log.new consoleSettings | isConsoleLog]
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
