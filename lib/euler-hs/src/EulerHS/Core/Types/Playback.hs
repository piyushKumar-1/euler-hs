{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables #-}

module EulerHS.Core.Types.Playback where


import EulerHS.Prelude
import qualified Data.Aeson                      as A
import qualified Data.ByteString.Char8           as BS
import qualified Data.ByteString.Lazy            as BSL
import qualified EulerHS.Core.Types.Serializable as S
import qualified Data.Map                        as Map
-- import qualified Data.Binary.Builder as B
-- import qualified GHC.Generics as G


data RecordingEntry = RecordingEntry
  { _entryIndex      :: Int
  , _entryReplayMode :: EntryReplayingMode
  , _entryName       :: String
  , _entryPayload    :: A.Value
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

type RecordingEntries = Vector RecordingEntry


data GlobalReplayingMode = GlobalNormal | GlobalNoVerify | GlobalNoMocking | GlobalSkip

data EntryReplayingMode = Normal | NoVerify | NoMock
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)



class (Eq rrItem, ToJSON rrItem, FromJSON rrItem) => RRItem rrItem where
  toRecordingEntry   :: rrItem -> Int -> EntryReplayingMode -> RecordingEntry
  toRecordingEntry rrItem idx mode = RecordingEntry idx mode (getTag (Proxy :: Proxy rrItem)) $
    toJSON rrItem

  fromRecordingEntry :: RecordingEntry -> Maybe rrItem
  fromRecordingEntry (RecordingEntry _ _ _ payload) = S.fromJSONMaybe payload

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
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON, Exception)

data PlaybackError = PlaybackError
  { errorType    :: PlaybackErrorType
  , errorMessage :: String
  } deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

data ReplayingException = ReplayingException PlaybackError
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

instance Exception ReplayingException


----------------------------------------------------------------------

data ResultRecording = ResultRecording
  { recording        :: RecordingEntries
  , forkedRecordings :: Map Text ResultRecording
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

data Recording = Recording
  { recordingMVar       :: MVar RecordingEntries
  , forkedRecordingsVar :: MVar (Map Text Recording)
  }

awaitRecording :: Recording -> IO ResultRecording
awaitRecording Recording{..}= do
  recording        <- readMVar recordingMVar
  forkedRecordings <- traverse awaitRecording =<< readMVar forkedRecordingsVar
  pure ResultRecording{..}

----------------------------------------------------------------------

data ReplayErrors = ReplayErrors
  { errorMVar           :: MVar (Maybe PlaybackError)
  , forkedFlowErrorsVar :: MVar (Map Text ReplayErrors)
  }

data ResultReplayError = ResultReplayError
  { rerror      :: Maybe PlaybackError
  , forkedError :: Map Text ResultReplayError
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

awaitErrors :: ReplayErrors -> IO ResultReplayError
awaitErrors ReplayErrors{..}= do
  rerror      <- readMVar errorMVar
  forkedError <- traverse awaitErrors =<< readMVar forkedFlowErrorsVar
  pure ResultReplayError{..}

flattenErrors :: ResultReplayError -> [PlaybackError]
flattenErrors = catMaybes . flattenErrors_
  where
    flattenErrors_ ResultReplayError{..} = rerror : (Map.elems forkedError >>= flattenErrors_)

----------------------------------------------------------------------


data RecorderRuntime = RecorderRuntime
  { flowGUID            :: Text
  , recording           :: Recording
  , disableEntries      :: [String]
  }


data PlayerRuntime = PlayerRuntime
  { resRecording         :: ResultRecording
  , rerror               :: ReplayErrors
  , stepMVar             :: MVar Int
  , disableVerify        :: [String]
  , disableMocking       :: [String]
  , skipEntries          :: [String]
  , entriesFiltered      :: Bool
  , flowGUID             :: Text
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


