{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables #-}

module EulerHS.Core.Types.Playback
  (
  -- * Core Playback
  -- ** Types
    RRItem(..)
  , MockedResult(..)
  , RecordingEntry(..)
  , RecordingEntries
  , GlobalReplayingMode(..)
  , EntryReplayingMode(..)
  , PlaybackErrorType(..)
  , PlaybackError(..)
  , ReplayingException(..)
  , ResultRecording(..)
  , Recording(..)
  , ReplayErrors(..)
  , ResultReplayError
  , RecorderRuntime(..)
  , PlayerRuntime(..)
  , RunMode (..)
  -- ** Methods
  , awaitRecording
  , awaitErrors
  , flattenErrors
  , note
  , encodeToStr
  , decodeFromStr
  ) where


import EulerHS.Prelude hiding (note)
import qualified Data.Aeson                      as A
import qualified Data.ByteString.Char8           as BS
import qualified Data.ByteString.Lazy            as BSL
import qualified EulerHS.Core.Types.Serializable as S
import qualified Data.Map                        as Map
-- import qualified Data.Binary.Builder as B
-- import qualified GHC.Generics as G

-- Represents ART single entry, saved in recordings
data RecordingEntry = RecordingEntry
  { _entryIndex      :: Int
  -- ^ Index in entries array
  , _entryReplayMode :: EntryReplayingMode
  -- ^ entry replay mode, could be one of 'Normal' 'NoVerify' 'NoMock'
  , _entryName       :: String
  -- ^ name of the method that this entry represents
  , _entryPayload    :: A.Value
  -- ^ method result value
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Represents method entries from the flow
type RecordingEntries = Vector RecordingEntry

-- | Global replaying mode to be applied to all entries
data GlobalReplayingMode = GlobalNormal | GlobalNoVerify | GlobalNoMocking | GlobalSkip

-- | Entry individual replaying mode settings
data EntryReplayingMode
  = Normal
  -- ^ (default) Verifying enabled. Mocking enabled.
  | NoVerify
  -- ^ Verifying disabled. Mocking enabled.
  | NoMock
  -- ^ Verifying disabled. Mocking disabled (real effect will be evaluated).
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

-- | Playback errors
data PlaybackErrorType
  -- | Player successfully replayed all recordings, but current flow have
  --   some additional steps.
  = UnexpectedRecordingEnd
  -- | Current flow step and recorded step is different. Signals about changes in the code
  --   (removed or added new steps, changed logic/behavior of a function - flow go to another branch, etc.)
  | UnknownRRItem
  -- | Mistakes in Encode/Decode instances, changes in types (another fields, different types of fields, etc.)
  | MockDecodingFailed
  -- | Results of execution of current flow step and recorded step is different. Something in code was changed
  --   (output format, order of values in result, etc), compare results to see what exactly is different.
  | ItemMismatch
  -- | Something went wrong.
  | UnknownPlaybackError
  -- |  Flow is forked on this step, but there are no forked flow recordings. Check difference in the code.
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
-- | Final recordings from main flow and forked flows
data ResultRecording = ResultRecording
  { recording        :: RecordingEntries
  , forkedRecordings :: Map Text ResultRecording
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Thread safe Recording representation that used in record process
data Recording = Recording
  { recordingMVar       :: MVar RecordingEntries
  , forkedRecordingsVar :: MVar (Map Text Recording)
  }

-- | Transform 'Recording' to 'ResultRecording' in safe way
awaitRecording :: Recording -> IO ResultRecording
awaitRecording Recording{..}= do
  recording        <- readMVar recordingMVar
  forkedRecordings <- traverse awaitRecording =<< readMVar forkedRecordingsVar
  pure ResultRecording{..}

----------------------------------------------------------------------
-- | Thread safe ReplayErrors representation used in replay process 
data ReplayErrors = ReplayErrors
  { errorMVar           :: MVar (Maybe PlaybackError)
  , forkedFlowErrorsVar :: MVar (Map Text ReplayErrors)
  }

-- | Final player errors representation
data ResultReplayError = ResultReplayError
  { rerror      :: Maybe PlaybackError
  , forkedError :: Map Text ResultReplayError
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Transform 'ReplayErrors' to 'ResultReplayError' in safe way
awaitErrors :: ReplayErrors -> IO ResultReplayError
awaitErrors ReplayErrors{..}= do
  rerror      <- readMVar errorMVar
  forkedError <- traverse awaitErrors =<< readMVar forkedFlowErrorsVar
  pure ResultReplayError{..}

-- | Extracts all errors from 'ResultReplayError' structure and puts them in the list
flattenErrors :: ResultReplayError -> [PlaybackError]
flattenErrors = catMaybes . flattenErrors_
  where
    flattenErrors_ ResultReplayError{..} = rerror : (Map.elems forkedError >>= flattenErrors_)

----------------------------------------------------------------------

-- | Represents ART recorder state and parameters
data RecorderRuntime = RecorderRuntime
  { flowGUID            :: Text
  , recording           :: Recording
  , disableEntries      :: [String]
  }

-- | Represents ART player state and parameters
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

-- | ART running mode
data RunMode
  = RegularMode
  -- ^ Flow executed as-is, ART has no impact
  | RecordingMode RecorderRuntime
  -- ^ ART collecting recordings for each backend method
  | ReplayingMode PlayerRuntime
  -- ^ ART replaying given recordings on corresponding flow scenario


encodeToStr :: ToJSON a => a -> String
encodeToStr = BS.unpack . BSL.toStrict . A.encode

decodeFromStr :: FromJSON a => String -> Maybe a
decodeFromStr = A.decode . BSL.fromStrict . BS.pack

-- gEncodeToStr :: (Generic a, A.GToEncoding A.Zero (G.Rep a)) => a -> String
-- gEncodeToStr = BS.unpack . BSL.toStrict . B.toLazyByteString . A.fromEncoding . A.genericToEncoding A.defaultOptions

note :: forall a b. a -> Maybe b -> Either a b
note a Nothing = Left a
note _ (Just b) = Right b


