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
    -- ** defaults
    , defaultLoggerConfig
    , mkMemoryLoggerConfig
    , nullLoger
    ) where

import EulerHS.Prelude

-- | Logging level.
data LogLevel = Debug | Info | Warning | Error
    deriving (Generic, Eq, Ord, Show, Read, Enum, ToJSON, FromJSON)

-- | Logging format.
type Format = String

data LoggerConfig
  = MemoryLoggerConfig LogLevel
  | LoggerConfig
  { _format       :: Format         -- TODO: FIXME: Not used for tiny logger
  , _isAsync      :: Bool
  , _level        :: LogLevel
  , _logFilePath  :: FilePath
  , _logToConsole :: Bool
  , _logToFile    :: Bool
  } deriving (Generic, Show, Read)

type Message = Text
type Tag = Text
data PendingMsg = PendingMsg !LogLevel !Tag !Message

data LogEntry = LogEntry !LogLevel !Message
type Log = [LogEntry]


defaultLoggerConfig :: LoggerConfig
defaultLoggerConfig = LoggerConfig
    { _format = ""
    , _isAsync = False
    , _level = Debug
    , _logFilePath = ""
    , _logToConsole = True
    , _logToFile = False
    }

mkMemoryLoggerConfig :: LogLevel -> LoggerConfig
mkMemoryLoggerConfig = MemoryLoggerConfig

nullLoger :: LoggerConfig
nullLoger = defaultLoggerConfig
    { _logFilePath = "/dev/null"
    , _logToConsole = False
    }