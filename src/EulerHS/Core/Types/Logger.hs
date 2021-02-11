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
    , ShouldLogSQL(..)
    , LogEntry (..)
    , Log
    , TransientLoggerContext
    , LogCounter
    , LogMaskingConfig (..)
    , MaskKeyType (..)
    -- ** defaults
    , defaultLoggerConfig
    , mkMemoryLoggerConfig
    , nullLoger
    ) where

import           EulerHS.Prelude


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
    { _maskKeys      :: HashSet Text -- Check : Better to make this case insensitive
    , _maskText      :: Maybe Text
    , _keyType       :: MaskKeyType
    } deriving (Generic, Show, Read)

data MaskKeyType =
    WhiteListKey
  | BlackListKey
  deriving (Generic, Show, Read)

data LoggerConfig
  = MemoryLoggerConfig LogLevel
  | LoggerConfig
  { _format           :: Format         -- TODO: FIXME: Not used for tiny logger
  , _isAsync          :: Bool
  , _level            :: LogLevel
  , _logFilePath      :: FilePath
  , _logToConsole     :: Bool
  , _logToFile        :: Bool
  , _maxQueueSize     :: Word
  , _logRawSql        :: ShouldLogSQL
  , _logMaskingConfig :: Maybe LogMaskingConfig
  } deriving (Generic, Show, Read)

data ShouldLogSQL
  -- Log SQL queries, including sensitive data and API keys. Do NOT PR code
  -- with this enabled, and make sure this doesn't make it into production
  = UnsafeLogSQL_DO_NOT_USE_IN_PRODUCTION
  -- omit SQL logs
  | SafelyOmitSqlLogs
  deriving (Generic, Show, Read)

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
    , _logRawSql = SafelyOmitSqlLogs
    , _logMaskingConfig = Nothing
    }

mkMemoryLoggerConfig :: LogLevel -> LoggerConfig
mkMemoryLoggerConfig = MemoryLoggerConfig

nullLoger :: LoggerConfig
nullLoger = defaultLoggerConfig
    { _logFilePath = "/dev/null"
    , _logToConsole = False
    }
