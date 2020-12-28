module PSMimicLogger where

import           EulerHS.Prelude


import qualified EulerHS.Types      as T
import qualified EulerHS.Runtime    as R

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.Binary.Builder as BinaryBuilder
import qualified Data.ByteString.Builder as ByteStringBuilder

import qualified System.Logger as Log
import qualified System.Logger.Message as LogMsg

import Fmt ((|++|))

-- This renderer and formatter from euler-hs / master

originalPSMimicRenderer
  :: String -> String -> String
  -> ByteString -> Log.DateFormat -> Log.Level      -- Not used in this renderer
  -> [Log.Element] -> BinaryBuilder.Builder
originalPSMimicRenderer hostname env sourceCommit _ _ _ loggerFields = res
  where
    bldRes =
        BinaryBuilder.fromByteString "{\"timestamp\": " <> quote <> timestamp <> quote <> commaSep <>
        BinaryBuilder.fromByteString "\"app_framework\": " <> quote <> "euler-hs-application" <> quote <> commaSep <>
        BinaryBuilder.fromByteString "\"hostname\": " <> quote <> fromString hostname <> quote <> commaSep <>
        BinaryBuilder.fromByteString "\"source_commit\": " <> quote <> fromString sourceCommit <> quote <> commaSep <>
        BinaryBuilder.fromByteString invariant <>
        mconcat (map elementToBS fields) <>
        BinaryBuilder.fromByteString "\"message\": " <> quote <> pMsg <> quote <> commaSep <>
        BinaryBuilder.fromByteString "\"message_type\": \"string\"}"
    res = bldRes
    quote = BinaryBuilder.fromByteString "\""
    jsonField k v = quote <> k <> quote <> ": " <> quote <> v <> quote <> commaSep
    (timestamp, pMsg, fields) = case loggerFields of
      (tsEl:pMsgEl:rest) -> (elementToBS tsEl, processMsgStr pMsgEl, rest)
      _ -> error "Malformed log fields."
    commaSep = BinaryBuilder.fromByteString ", "
    invariant = "\"level\": \"info\", \"txn_uuid\": \"null\", \"order_id\": \"null\", \"refund_id\": \"null\", \"refund_unique_request_id\": \"null\", \"merchant_id\": \"null\", " -- x-request-id = \"null\", "
    processMsgStr = \case
      LogMsg.Bytes b   -> LogMsg.builderBytes b
      LogMsg.Field _ v -> LogMsg.builderBytes v
    elementToBS = \case
      LogMsg.Bytes b -> LogMsg.builderBytes b
      LogMsg.Field k v -> jsonField (LogMsg.builderBytes k) (LogMsg.builderBytes v)

originalPSMimicFormatter :: Maybe Text -> T.MessageFormatter
originalPSMimicFormatter ctx2 (T.PendingMsg mbFlowGuid lvl tag msg msgNum) = res
  where
    eulerMsg :: Text
    eulerMsg = "[" +|| lvl ||+ "] <" +| tag |+ "> " +| msg |+ ""
    escapedMsg = T.tail . T.init $ show eulerMsg
    ctxVal1 = fromMaybe "null" mbFlowGuid
    ctxVal2 = fromMaybe "null" ctx2
    logEntry = PSLogEntry escapedMsg (show msgNum) ctxVal1 ctxVal2

    f = (Log.field @ByteString "message" (T.encodeUtf8 escapedMsg))
      . (Log.field @Int "message_number" msgNum)
      . (Log.field "x-request-id" ctxVal1)
      . (Log.field "x-global-request-id" ctxVal2)

    res = T.MsgTransformer f

-- Custom formatter #1, fmt based

fmtPSMimicFormatterString
  :: String
  -> String
  -> String
  -> String
  -> String
  -> Maybe Text
  -> T.MessageFormatter
fmtPSMimicFormatterString
  appFramework
  timestamp
  hostname env sourceCommit
  mbCtx2
  (T.PendingMsg mbFlowGuid lvl tag msg msgNum) = res
  where
    eulerMsg :: String
    eulerMsg = "[" +|| lvl ||+ "] <" +| tag |+ "> " +| msg |+ ""
    escapedMsg = T.unpack $ T.tail $ T.init $ show eulerMsg
    ctxVal1, ctxVal2 :: String
    ctxVal1 = fromMaybe "null" (T.unpack <$> mbFlowGuid)
    ctxVal2 = fromMaybe "null" (T.unpack <$> mbCtx2)

    invariant = ", \"level\": \"info\", \"txn_uuid\": \"null\", \"order_id\": \"null\", \"refund_id\": \"null\", \"refund_unique_request_id\": \"null\", \"merchant_id\": \"null\""

    msgCustomFmt

          = "{\"timestamp\": \""            +| timestamp    |+ "\""
         +| ", \"app_framework\": \""       +| appFramework |+ "\""
         +| ", \"hostname\": \""            +| hostname     |+ "\""
         +| ", \"source_commit\": \""       +| sourceCommit |+ "\""
         +| invariant
         +| ", \"message_number\": \""      +|| msgNum      ||+ "\""
         +| ", \"x-request-id\": \""        +| ctxVal1      |+ "\""
         +| ", \"x-global-request-id\": \"" +| ctxVal2      |+ "\""
         +| ", \"message\": \""             +| escapedMsg   |+ "\""
         +| ", \"message_type\": \"string\"}\n"

    res = T.SimpleString msgCustomFmt



fmtPSMimicFormatterText
  :: Text
  -> Text
  -> Text
  -> Text
  -> Text
  -> Maybe Text
  -> T.MessageFormatter
fmtPSMimicFormatterText
  appFramework
  timestamp
  hostname env sourceCommit
  mbCtx2
  (T.PendingMsg mbFlowGuid lvl tag msg msgNum) = res
  where
    eulerMsg :: Text
    eulerMsg = "[" +|| lvl ||+ "] <" +| tag |+ "> " +| msg |+ ""
    escapedMsg = T.unpack $ T.tail $ T.init $ show eulerMsg
    ctxVal1, ctxVal2 :: Text
    ctxVal1 = fromMaybe "null" mbFlowGuid
    ctxVal2 = fromMaybe "null" mbCtx2

    invariant = ", \"level\": \"info\", \"txn_uuid\": \"null\", \"order_id\": \"null\", \"refund_id\": \"null\", \"refund_unique_request_id\": \"null\", \"merchant_id\": \"null\""

    msgCustomFmt

          = "{\"timestamp\": \""            +| timestamp    |+ "\""
         +| ", \"app_framework\": \""       +| appFramework |+ "\""
         +| ", \"hostname\": \""            +| hostname     |+ "\""
         +| ", \"source_commit\": \""       +| sourceCommit |+ "\""
         +| invariant
         +| ", \"message_number\": \""      +|| msgNum      ||+ "\""
         +| ", \"x-request-id\": \""        +| ctxVal1      |+ "\""
         +| ", \"x-global-request-id\": \"" +| ctxVal2      |+ "\""
         +| ", \"message\": \""             +| escapedMsg   |+ "\""
         +| ", \"message_type\": \"string\"}\n"

    res = T.SimpleText msgCustomFmt

-- Custom formatter #3, aeson based, TODO

data PSLogEntry = PSLogEntry
  { message :: Text
  , message_number :: Text
  , x_request_id :: Text
  , x_global_request_id :: Text
  }

instance A.ToJSON PSLogEntry where
  toJSON (PSLogEntry {..}) =
    A.object [ "message" A..= message
             , "message_number" A..= message_number
             , "x-request-id"  A..= x_request_id
             , "x-global-request-id"  A..= x_global_request_id
             ]

-- Initial experiments
-- customPSMimicFormatter :: BuilderType -> Maybe Text -> T.MessageFormatter
-- customPSMimicFormatter bldType ctx2 (T.PendingMsg mbFlowGuid lvl tag msg msgNum) = res
--   where
--     eulerMsg :: Text
--     eulerMsg = "[" +|| lvl ||+ "] <" +| tag |+ "> " +| msg |+ ""
--     escapedMsg = T.tail . T.init $ show eulerMsg
--     ctxVal1 = fromMaybe "null" mbFlowGuid
--     ctxVal2 = fromMaybe "null" ctx2
--     logEntry = PSLogEntry escapedMsg (show msgNum) ctxVal1 ctxVal2
--
--     msgAeson = BS.unpack $ A.encode logEntry
--     msgCustomFmt = "message" +|| T.encodeUtf8 escapedMsg ||+
--       "message_number" +|| msgNum ||+
--       "x-request-id" +| ctxVal1 |+
--       "x-global-request-id" +| ctxVal2 |+ ""
--
--     f = (Log.field @ByteString "message" (T.encodeUtf8 escapedMsg))
--       . (Log.field @Int "message_number" msgNum)
--       . (Log.field "x-request-id" ctxVal1)
--       . (Log.field "x-global-request-id" ctxVal2)
--
--     msg' Aeson     = T.SimpleString msgAeson
--     msg' CustomFmt = T.SimpleString msgCustomFmt
--     msg' PSMimic   = T.MsgTransformer f
