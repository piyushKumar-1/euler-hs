module PSMimicLogger where

import           EulerHS.Prelude


import qualified EulerHS.Types      as T
import qualified EulerHS.Runtime    as R

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.ByteString.Char8 as BS
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
    -- Why level is info??
    invariant = "\"level\": \"info\", \"txn_uuid\": \"null\", \"order_id\": \"null\", \"refund_id\": \"null\", \"refund_unique_request_id\": \"null\", \"merchant_id\": \"null\", " -- x-request-id = \"null\", "
    escapeMsg m = BinaryBuilder.fromByteString (T.encodeUtf8 $ T.tail $ T.init $ show $ BinaryBuilder.toLazyByteString m)
    processMsgStr = \case
      LogMsg.Bytes b   -> escapeMsg $ LogMsg.builderBytes b
      LogMsg.Field _ v -> escapeMsg $ LogMsg.builderBytes v
    elementToBS = \case
      LogMsg.Bytes b -> LogMsg.builderBytes b
      LogMsg.Field k v -> jsonField (LogMsg.builderBytes k) (LogMsg.builderBytes v)

originalPSMimicFormatter :: Maybe Text -> T.MessageFormatter
originalPSMimicFormatter ctx2 (T.PendingMsg mbFlowGuid lvl tag msg msgNum) = res
  where
    eulerMsg :: Text
    eulerMsg = "[" +|| lvl ||+ "] <" +| tag |+ "> " +| msg |+ ""
    ctxVal1 = fromMaybe "null" mbFlowGuid
    ctxVal2 = fromMaybe "null" ctx2

    f = (Log.field "message" eulerMsg)
      . (Log.field @String "message_number" (show msgNum))
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

    -- Why level is info??
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

    -- Why level is info??
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

data PSLogEntry strType = PSLogEntry
  { _timestamp                :: strType
  , _app_framework            :: strType
  , _hostname                 :: strType
  , _source_commit            :: strType

  , _level                    :: strType
  , _txn_uuid                 :: strType
  , _order_id                 :: strType
  , _refund_id                :: strType
  , _refund_unique_request_id :: strType
  , _merchant_id              :: strType

  , _message_number           :: strType
  , _x_request_id             :: strType
  , _x_global_request_id      :: strType
  , _message                  :: strType
  , _message_type             :: strType
  }

instance A.ToJSON (PSLogEntry Text) where
  toJSON (PSLogEntry {..}) =
    A.object [ "timestamp"                A..= _timestamp
             , "app_framework"            A..= _app_framework
             , "hostname"                 A..= _hostname
             , "source_commit"            A..= _source_commit
             , "level"                    A..= _level
             , "txn_uuid"                 A..= _txn_uuid
             , "order_id"                 A..= _order_id
             , "refund_id"                A..= _refund_id
             , "refund_unique_request_id" A..= _refund_unique_request_id
             , "merchant_id"              A..= _merchant_id
             , "message_number"           A..= _message_number
             , "x-request-id"             A..= _x_request_id
             , "x-global-request-id"      A..= _x_global_request_id
             , "message"                  A..= _message
             ]

aesonPSMimicFormatterText
  :: Text
  -> Text
  -> Text
  -> Text
  -> Text
  -> Maybe Text
  -> T.MessageFormatter
aesonPSMimicFormatterText
  appFramework
  timestamp
  hostname env sourceCommit
  mbCtx2
  (T.PendingMsg mbFlowGuid lvl tag msg msgNum) = res
  where
    eulerMsg :: Text
    eulerMsg = "[" +|| lvl ||+ "] <" +| tag |+ "> " +| msg |+ ""
    ctxVal1, ctxVal2 :: Text
    ctxVal1 = fromMaybe "null" mbFlowGuid
    ctxVal2 = fromMaybe "null" mbCtx2

    -- Why level is info??
    logEntry = PSLogEntry
          { _timestamp                = timestamp
          , _app_framework            = appFramework
          , _hostname                 = hostname
          , _source_commit            = sourceCommit
          , _level                    = "info"
          , _txn_uuid                 = "null"
          , _order_id                 = "null"
          , _refund_id                = "null"
          , _refund_unique_request_id = "null"
          , _merchant_id              = "null"
          , _message_number           = show msgNum
          , _x_request_id             = ctxVal1
          , _x_global_request_id      = ctxVal2
          , _message                  = eulerMsg
          , _message_type             = "string"
          }

    res = T.SimpleLBS $ A.encode logEntry
