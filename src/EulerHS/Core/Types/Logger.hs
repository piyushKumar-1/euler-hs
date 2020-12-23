{-# LANGUAGE DeriveAnyClass #-}

module EulerHS.Core.Types.Logger
    (
    -- * Core Logger
    -- ** Types
      LogLevel(..)
    , BufferSize
    , MessageBuilder (..)
    , MessageFormatter
    , FlowFormatter
    , LoggerConfig(..)
    , Message
    , Tag
    , PendingMsg(..)
    , LogEntry (..)
    , Log
    , LogCounter
    -- ** defaults
    , defaultLoggerConfig
    , defaultMessageFormatter
    , showingMessageFormatter
    , defaultFlowFormatter
    , builderToByteString
    ) where

import           EulerHS.Prelude

import qualified EulerHS.Core.Types.Common as T

-- Currently, TinyLogger is highly coupled with the interface.
-- Reason: unclear current practice of logging that affects design and performance.
import qualified System.Logger.Message as LogMsg
import qualified Data.ByteString.Lazy as LBS

-- | Logging level.
data LogLevel = Debug | Info | Warning | Error
    deriving (Generic, Eq, Ord, Show, Read, Enum, ToJSON, FromJSON)

data MessageBuilder
  = SimpleString String
  | Builder LogMsg.Builder

type LogCounter = IORef Int         -- No race condition: atomicModifyIORef' is used.
type Message = Text
type Tag = Text
type MessageNumber = Int
type BufferSize = Int
type MessageFormatter = PendingMsg -> MessageBuilder
type FlowFormatter = Maybe T.FlowGUID -> IO MessageFormatter

data LoggerConfig
  = LoggerConfig
    { _isAsync      :: Bool
    , _logLevel     :: LogLevel
    , _logFilePath  :: FilePath
    , _logToConsole :: Bool
    , _logToFile    :: Bool
    , _maxQueueSize :: Word
    , _logRawSql    :: Bool
    } deriving (Generic, Show, Read)

data PendingMsg = PendingMsg
  !(Maybe T.FlowGUID)
  !LogLevel
  !Tag
  !Message
  !MessageNumber
  deriving (Show)

data LogEntry = LogEntry !LogLevel !Message
type Log = [LogEntry]

defaultMessageFormatter :: MessageFormatter
defaultMessageFormatter (PendingMsg _ lvl tag msg _) =
  SimpleString $ "[" +|| lvl ||+ "] <" +| tag |+ "> " +| msg |+ ""

showingMessageFormatter :: MessageFormatter
showingMessageFormatter = SimpleString . show

defaultLoggerConfig :: LoggerConfig
defaultLoggerConfig = LoggerConfig
    { _isAsync = False
    , _logLevel = Debug
    , _logFilePath = ""
    , _logToConsole = True
    , _logToFile = False
    , _maxQueueSize = 1000
    , _logRawSql = True
    }

defaultFlowFormatter :: FlowFormatter
defaultFlowFormatter _ = pure defaultMessageFormatter

builderToByteString :: LogMsg.Builder -> LBS.ByteString
builderToByteString = LogMsg.eval
