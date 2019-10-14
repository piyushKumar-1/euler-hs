{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables #-}

module EulerHS.Framework.Playback.Types where

import EulerHS.Prelude
import qualified Data.Aeson as A
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BSL


type EntryIndex = Int
type EntryName = String
type EntryPayload = String
data RecordingEntry = RecordingEntry EntryIndex EntryReplayingMode EntryName EntryPayload
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

type RecordingEntries = Vector RecordingEntry
newtype Recording = Recording RecordingEntries

data GlobalReplayingMode = GlobalNormal | GlobalNoVerify | GlobalNoMocking | GlobalSkip

data EntryReplayingMode = Normal | NoVerify | NoMock
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)



class (Eq rrItem, ToJSON rrItem, FromJSON rrItem)
  => RRItem rrItem where
  toRecordingEntry   :: rrItem -> Int -> EntryReplayingMode -> RecordingEntry
  fromRecordingEntry :: RecordingEntry -> Maybe rrItem
  getTag             :: Proxy rrItem -> String

class RRItem rrItem => MockedResult rrItem native where
  getMock :: rrItem -> Maybe native


data PlaybackErrorType
  = UnexpectedRecordingEnd
  | UnknownRRItem
  | MockDecodingFailed
  | ItemMismatch
  | UnknownPlaybackError
  | ForkedFlowRecordingsMissed
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

data PlaybackError = PlaybackError
  { errorType    :: PlaybackErrorType
  , errorMessage :: String
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

data ReplayingException = ReplayingException PlaybackError
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)
instance Exception ReplayingException

data RecorderRuntime = RecorderRuntime
  { flowGUID            :: String
  , recordingMVar       :: MVar RecordingEntries
  , forkedRecordingsVar :: MVar (Map String (MVar RecordingEntries))
  , disableEntries      :: [String]
  }

data PlayerRuntime = PlayerRuntime
  { recording            :: RecordingEntries
  , stepMVar             :: MVar Int
  , errorMVar            :: MVar (Maybe PlaybackError)
  , disableVerify        :: [String]
  , disableMocking       :: [String]
  , skipEntries          :: [String]
  , entriesFiltered      :: Bool
  , flowGUID             :: String
  , forkedFlowRecordings :: Map String RecordingEntries
  , forkedFlowErrorsVar  :: MVar (Map String (Maybe PlaybackError))
  }

data RunMode
  = RegularMode
  | RecordingMode RecorderRuntime
  | ReplayingMode PlayerRuntime


encodeToStr :: ToJSON a => a -> String
encodeToStr = BS.unpack . BSL.toStrict . A.encode

decodeFromStr :: FromJSON a => String -> Maybe a
decodeFromStr = A.decode . BSL.fromStrict . BS.pack

note :: forall a b. a -> Maybe b -> Either a b
note a Nothing = Left a
note _ (Just b) = Right b
