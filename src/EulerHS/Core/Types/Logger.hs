{-# LANGUAGE DeriveAnyClass #-}

module EulerHS.Core.Types.Logger
    (
    -- * Core Logger
    -- ** Types
      LogLevel(..)
    , Format
    , LoggerConfig(..)
    , Message
    , Tag
    , PendingMsg(..)
    , LogEntry (..)
    , Log
    , TransientLoggerContext
    , LogCounter
    -- ** defaults
    , defaultLoggerConfig
    , mkMemoryLoggerConfig
    , nullLoger
    ) where

import           EulerHS.Prelude

-- | Logging level.
data LogLevel = Debug | Info | Warning | Error
    deriving (Generic, Eq, Ord, Show, Read, Enum, ToJSON, FromJSON)

type MessageFormat = String
type DateFormat = String
type LogCounter = IORef Int         -- No race condition: atomicModifyIORef' is used.
type Message = Text
type Tag = Text
type MessageNumber = Int
type BufferSize = Int
type MessageFormatter = PendingMsg -> String

data LoggerConfig
  = MemoryLoggerConfig MessageFormatter LogLevel
  | LoggerConfig
    { _msgFormatter :: MessageFormatter
    , _isAsync      :: Bool
    , _level        :: LogLevel
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

defaultLoggerFormatter :: Formatter
defaultLoggerFormatter lvl tag msg _ _ _ =
  "[" +|| lvl ||+ "] <" +| tag |+ "> " +| msg |+ ""


defaultLoggerConfig :: LoggerConfig
defaultLoggerConfig = LoggerConfig
    { _format = ""
    , _formatter = defaultLoggerFormatter
    , _isAsync = False
    , _level = Debug
    , _logFilePath = ""
    , _logToConsole = True
    , _logToFile = False
    , _maxQueueSize = 1000
    , _logRawSql = True
    }

mkMemoryLoggerConfig :: LogLevel -> LoggerConfig
mkMemoryLoggerConfig = MemoryLoggerConfig

nullLoger :: LoggerConfig
nullLoger = defaultLoggerConfig
    { _logFilePath = "/dev/null"
    , _logToConsole = False
    }
