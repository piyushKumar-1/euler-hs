{-# LANGUAGE DeriveAnyClass #-}

module Euler.Playback.Types
  ( FlowTag
  , RecorderParams(..)
  , ResponseCheckMode(..)
  , PlayerParams(..)
  , RecordingsStorage(..)
  , MethodRecordingSource(..)
  , MethodConfigs(..)
  , MethodRecording(..)
  , MethodRecordingDescription(..)
  , MethodPlayerError(..)
  , ResponseCheckResult(..)
  , MethodPlaybackResult(..)
  , MethodPlayerResult
  , PlayerError(..)
  , PlayerResult
  , MethodFailure(..)
  ) where

import EulerHS.Prelude

import           EulerHS.Types

import qualified Data.Aeson as A


type FlowTag = Text

data RecorderParams = RecorderParams
  { disableEntries      :: [String]
  , recordingsStorage   :: RecordingsStorage
  }
  deriving (Eq,  Show, Generic, ToJSON, FromJSON)

data ResponseCheckMode
  = VerifyResponse
  | NoVerifyResponse
  deriving (Eq, Read, Show, Generic, ToJSON, FromJSON)

data PlayerParams = PlayerParams
  { ppDisableVerify          :: [String]
  , ppDisableMocking         :: [String]
  , ppSkipEntries            :: [String]
  , ppResponseCheckMode      :: ResponseCheckMode
  , ppForcedRealDBConns      :: Map String String -- Connection
  , ppForcedRealKVDBConns    :: Map String String -- Connection
  }
  deriving (Eq,  Show, Generic, ToJSON, FromJSON)

data RecordingsStorage = FSStorage String -- DirName
                       | DBStorage String -- ConnectionName or Connection
  deriving (Eq,  Show, Generic, ToJSON, FromJSON)

data MethodRecordingSource
  = FileSource String
  deriving (Eq,  Show, Generic, ToJSON, FromJSON)


-- type PlayerAff eff res = Aff (PlayerEffects eff) res

data MethodConfigs = MethodConfigs
  { mcRawBody         :: String
  , mcQueryParams     :: Map String String
  , mcRouteParams     :: Map Text Text
 -- , headers         :: EitherEx String Headers  -- TODO
  , mcRawHeaders      :: Map String String
  -- , httpMethod      :: Method  -- TODO
  , mcMethodUrl       :: String
 -- , trackers        :: StrMap MockedMetric
  , mcSourceIP        :: String
  , mcUserAgent       :: String
  }
  deriving (Eq,  Show, Generic, ToJSON, FromJSON)

data MethodRecording = MethodRecording
  { mrJsonRequest       :: A.Value -- Foreign
  , mrJsonResponse      :: A.Value -- Foreign
  , mrEntries           :: ResultRecording
 -- , mrForkedFlowEntries :: Map String RecordingEntries
  , mrMethodConfigs     :: MethodConfigs
  , mrSessionId         :: String
 -- , mockedSqlConns    :: StrMap MockedSqlConn
 -- , mockedRedisConns  :: StrMap MockedKVDBConn
  , mrParameters        :: Map String String
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

data MethodRecordingDescription = MethodRecordingDescription
  { methodName      :: String
  , methodRecording :: MethodRecording
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

data MethodPlayerError
  = RequestDecodingError String
  | RecordingsDecodingError String
  | MethodNotSupported String
  | RecordingsReadError String
  | ExceptionInPlayer String
  | ForkedFlowsFailed [PlaybackError] -- (Map Text ReplayErrors)
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

data ResponseCheckResult
  = ResponseOk
  | ResponseSkipped
  | ResponseMismatch String String
  | ResponseDecodingError String
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

data MethodPlaybackResult
  = PlaybackSucceeded ResponseCheckResult
  | PlaybackFailed PlaybackError
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

type MethodPlayerResult = Either MethodPlayerError MethodPlaybackResult


data PlayerError
  = DirectoryReadError String
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

type PlayerResult = Either PlayerError  [(String, MethodPlayerResult)]

data MethodFailure = MethodFailure String
  deriving (Eq, Show, Generic, ToJSON, FromJSON)