{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables #-}

module EulerHS.Core.Logger.Entries where


import EulerHS.Prelude
import EulerHS.Types (RRItem(..), MockedResult(..), encodeToStr, decodeFromStr)
import qualified EulerHS.Types as T
import qualified Servant.Client as S


data LogMessageEntry = LogMessageEntry
  { level :: T.LogLevel
  , tag   :: T.Tag
  , msg   :: T.Message
  } deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

mkLogMessageEntry
  :: T.LogLevel
  -> T.Tag
  -> T.Message
  -> a
  -> LogMessageEntry
mkLogMessageEntry level tag msg _ = LogMessageEntry level tag msg

instance RRItem LogMessageEntry where
  getTag _ = "LogMessageEntry"

instance MockedResult LogMessageEntry () where
  getMock _ = Just ()

