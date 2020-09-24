{-# LANGUAGE GADTs           #-}
{-# LANGUAGE TemplateHaskell #-}

module EulerHS.Core.Logger.Language
  (
    Logger
  , LoggerMethod(..)
  , logMessage'
  , logCallStack
  ) where

import           EulerHS.Prelude

import qualified EulerHS.Core.Types.Logger as T

-- | Language for logging.
data LoggerMethod next where
  -- | Log message with a predefined level.
  LogMessage :: T.LogLevel -> !T.Tag -> !T.Message -> (() -> next) -> LoggerMethod next

instance Functor LoggerMethod where
  fmap f (LogMessage lvl tag msg next) = LogMessage lvl tag msg $ f . next

type Logger = F LoggerMethod

logMessage' :: Show tag => T.LogLevel -> tag -> T.Message -> Logger ()
logMessage' lvl tag msg = liftFC $ LogMessage lvl (show tag) msg id
-- {-# NOINLINE logMessage' #-}
-- {-# RULES
--
--     "Specialise Text Tag logMessage'" forall (tag :: Text) (lvl :: T.LogLevel) (msg :: T.Message) .
--        logMessage' lvl tag msg = liftFC $ LogMessage lvl tag msg id ;
--
--     "Specialise String Tag logMessage'" forall (tag :: String) (lvl :: T.LogLevel) (msg :: T.Message) .
--        logMessage' lvl tag msg = liftFC $ LogMessage lvl (toText tag) msg id
-- #-}

-- Doubts:
-- Is it the right place to put it?
-- Is using putStrLn okay?
-- Should the type be something other that IO ()?
-- The output will be multiple lines. Is it better to put it in a single line? Will it be okay till it reaches kibana if it is multiline?
logCallStack :: HasCallStack => IO ()
logCallStack = putStrLn . prettyCallStack $ callStack
