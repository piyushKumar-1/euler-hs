{-# LANGUAGE DeriveAnyClass #-}

module EulerHS.Core.Types.Logger
    (
    -- * Core Logger
    -- ** Types
      LogLevel(..)
    , BufferSize
    , MessageFormatter
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
    ) where

import           EulerHS.Prelude

-- | Logging level.
data LogLevel = Debug | Info | Warning | Error
    deriving (Generic, Eq, Ord, Show, Read, Enum, ToJSON, FromJSON)

type LogCounter = IORef Int         -- No race condition: atomicModifyIORef' is used.
type Message = Text
type Tag = Text
type MessageNumber = Int
type BufferSize = Int
type MessageFormatter = PendingMsg -> String

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
  !LogLevel
  !Tag
  !Message
  !MessageNumber
  deriving (Show)

data LogEntry = LogEntry !LogLevel !Message
type Log = [LogEntry]

defaultMessageFormatter :: MessageFormatter
defaultMessageFormatter (PendingMsg lvl tag msg _) =
  "[" +|| lvl ||+ "] <" +| tag |+ "> " +| msg |+ ""

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
