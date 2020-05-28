{-# LANGUAGE DeriveAnyClass #-}

module EulerHS.Core.Types.HttpAPI
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
    ) where

import EulerHS.Prelude

import qualified Data.Text as Text
import qualified Data.ByteString.Lazy as ByteString

data HTTPRequest
  = HTTPRequest
    { method :: HTTPMethod
    , url    :: Text.Text
    }
    deriving (Show, Eq, Ord, ToJSON, FromJSON)

data HTTPResponse
  = HTTPResponse
    { status :: Int
    , body   :: ByteString.ByteString
    }
