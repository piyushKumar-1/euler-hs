{-# LANGUAGE GADTs           #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

module EulerHS.Logger.Language
  (
    Logger
  , LoggerMethod(..)
  , logMessage'
  , logMessageFormatted
  ) where

import qualified EulerHS.Logger.Types as T
import           EulerHS.Prelude
import           Type.Reflection

-- | Language for logging.
data LoggerMethod next where
  -- | Log message with a predefined level.
  LogMessage :: T.LogLevel -> !T.VersionLoggerMessage -> (() -> next) -> LoggerMethod next 
--  LogMessage :: T.LogLevel -> !T.Tag -> !T.Message -> (() -> next) -> LoggerMethod next

  --T.LogLevel !Category !Action !Entity !(Maybe Error) !(Maybe Latency) !(Maybe RespCode) !Message

instance Functor LoggerMethod where
  fmap f (LogMessage lvl vMsg next) = LogMessage lvl vMsg $ f . next

type Logger = F LoggerMethod

logMessage' :: forall tag . (Typeable tag, Show tag) => T.LogLevel -> tag -> T.Message -> Logger ()
logMessage' lvl tag msg = liftFC $ LogMessage lvl (T.Ver1 textTag msg) id
  where
    textTag :: Text
    textTag
      | Just HRefl <- eqTypeRep (typeRep @tag) (typeRep @Text  ) = tag
      | Just HRefl <- eqTypeRep (typeRep @tag) (typeRep @String) = toText tag
      | otherwise = show tag

logMessageFormatted :: T.LogLevel -> T.Category -> T.Action -> T.Entity -> Maybe T.ErrorL -> Maybe T.Latency -> Maybe T.RespCode -> T.Message -> Logger ()
logMessageFormatted logLevel category action entity maybeError maybeLatency maybeRespCode message =
  liftFC $ LogMessage logLevel (T.Ver2 category action entity maybeError maybeLatency maybeRespCode message) id