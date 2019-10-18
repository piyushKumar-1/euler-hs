{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables #-}

module EulerHS.Core.Types.Playback where


import EulerHS.Prelude
import qualified Data.Aeson as A
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BSL
-- import qualified Data.Binary.Builder as B
-- import qualified GHC.Generics as G


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



class (Eq rrItem, ToJSON rrItem, FromJSON rrItem) => RRItem rrItem where
  toRecordingEntry   :: rrItem -> Int -> EntryReplayingMode -> RecordingEntry
  toRecordingEntry rrItem idx mode = RecordingEntry idx mode (getTag (Proxy :: Proxy rrItem)) $ encodeToStr rrItem

  fromRecordingEntry :: RecordingEntry -> Maybe rrItem
  fromRecordingEntry (RecordingEntry _ _ _ payload) = decodeFromStr payload

  getTag :: Proxy rrItem -> String
  {-# MINIMAL getTag #-}

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
  , forkedFlowErrorsVar  :: MVar (Map String (MVar (Maybe PlaybackError)))
  }

data RunMode
  = RegularMode
  | RecordingMode RecorderRuntime
  | ReplayingMode PlayerRuntime


encodeToStr :: ToJSON a => a -> String
encodeToStr = BS.unpack . BSL.toStrict . A.encode

decodeFromStr :: FromJSON a => String -> Maybe a
decodeFromStr = A.decode . BSL.fromStrict . BS.pack

-- gEncodeToStr :: (Generic a, A.GToEncoding A.Zero (G.Rep a)) => a -> String
-- gEncodeToStr = BS.unpack . BSL.toStrict . B.toLazyByteString . A.fromEncoding . A.genericToEncoding A.defaultOptions

note :: forall a b. a -> Maybe b -> Either a b
note a Nothing = Left a
note _ (Just b) = Right b


