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
    , LogMaskingConfig (..)
    -- ** defaults
    , defaultLoggerConfig
    , mkMemoryLoggerConfig
    , nullLoger
    ) where

import           EulerHS.Prelude
import           Data.HashSet(HashSet)
-- | Logging level.
data LogLevel = Debug | Info | Warning | Error
    deriving (Generic, Eq, Ord, Show, Read, Enum, ToJSON, FromJSON)

-- | Logging format.
type Format = String

-- TODO: FIXME
-- this currently only stores session id
type TransientLoggerContext = Maybe Text

type LogCounter = IORef Int

data LogMaskingConfig = 
  LogMaskingConfig
    { _whiteListKey :: HashSet Text
    , _blackListKey :: HashSet Text
    , _mask         :: Text
    } deriving (Generic, Show, Read)

data LoggerConfig
  = MemoryLoggerConfig LogLevel
  | LoggerConfig
  { _format       :: Format         -- TODO: FIXME: Not used for tiny logger
  , _isAsync      :: Bool
  , _level        :: LogLevel
  , _logFilePath  :: FilePath
  , _logToConsole :: Bool
  , _logToFile    :: Bool
  , _maxQueueSize :: Word
  , _logRawSql    :: Bool
  , _logMaskingConfig :: Maybe LogMaskingConfig
  } deriving (Generic, Show, Read)

type Message = Text
type Tag = Text
type MessageNumber = Int
data PendingMsg = PendingMsg !LogLevel !Tag !Message !MessageNumber !TransientLoggerContext !TransientLoggerContext

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
