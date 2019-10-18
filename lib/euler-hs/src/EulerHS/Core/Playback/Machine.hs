module EulerHS.Core.Playback.Machine where

import EulerHS.Prelude hiding (note)
import EulerHS.Types
import qualified Data.Vector as V
import Data.Vector as V ((!?))
import Control.Exception (throwIO)


showInfo :: String -> String -> String
showInfo flowStep recordingEntry =
  "\n    Flow step: " ++ flowStep
  ++ "\n    Recording entry: " ++ recordingEntry

unexpectedRecordingEnd :: String -> PlaybackError
unexpectedRecordingEnd flowStep
  = PlaybackError UnexpectedRecordingEnd
  $ "\n    Flow step: " ++ flowStep

unknownRRItem :: String -> String -> PlaybackError
unknownRRItem flowStep recordingEntry
  = PlaybackError UnknownRRItem
  $ showInfo flowStep recordingEntry

mockDecodingFailed :: String -> String -> PlaybackError
mockDecodingFailed flowStep recordingEntry
  = PlaybackError MockDecodingFailed
  $ showInfo flowStep recordingEntry

itemMismatch :: String -> String -> PlaybackError
itemMismatch flowStep recordingEntry
  = PlaybackError ItemMismatch
  $ showInfo flowStep recordingEntry

setReplayingError :: PlayerRuntime -> PlaybackError -> IO e
setReplayingError playerRt err = do
  void $ takeMVar $ errorMVar playerRt
  putMVar (errorMVar playerRt) (Just err)
  throwIO $ ReplayingException err

pushRecordingEntry
  :: RecorderRuntime
  -> RecordingEntry
  -> IO ()
pushRecordingEntry recorderRt (RecordingEntry _ mode n p) = do
  entries <- takeMVar $ recordingMVar recorderRt
  let idx = (V.length entries)
  let re = RecordingEntry idx mode n p
  putMVar (recordingMVar recorderRt) $ V.snoc entries re

popNextRecordingEntry :: PlayerRuntime -> IO (Maybe RecordingEntry)
popNextRecordingEntry PlayerRuntime{..} = do
  cur <- takeMVar stepMVar
  let mbItem = (!?) recording cur
  when (isJust mbItem) $ putMVar stepMVar (cur + 1)
  pure mbItem

popNextRRItem
  :: forall rrItem
   . RRItem rrItem
  => PlayerRuntime
  -> IO (Either PlaybackError (RecordingEntry, rrItem))
popNextRRItem playerRt  = do
  mbRecordingEntry <- popNextRecordingEntry playerRt
  let flowStep = getTag $ Proxy @rrItem
  pure $ do
    recordingEntry <- note (unexpectedRecordingEnd flowStep) mbRecordingEntry
    let unknownErr = unknownRRItem flowStep $ show recordingEntry
    rrItem <- note unknownErr $ fromRecordingEntry recordingEntry
    pure (recordingEntry, rrItem)

popNextRRItemAndResult
  :: forall rrItem native
   . RRItem rrItem
  => MockedResult rrItem native
  => PlayerRuntime
  -> IO (Either PlaybackError (RecordingEntry, rrItem, native))
popNextRRItemAndResult playerRt  = do
  let flowStep = getTag $ Proxy @rrItem
  eNextRRItem <- popNextRRItem playerRt
  pure $ do
    (recordingEntry, rrItem) <- eNextRRItem
    let mbNative = getMock rrItem
    nextResult <- note (mockDecodingFailed flowStep (show recordingEntry)) mbNative
    pure (recordingEntry, rrItem, nextResult)

compareRRItems
  :: RRItem rrItem
  => MockedResult rrItem native
  => PlayerRuntime
  -> (RecordingEntry, rrItem, native)
  -> rrItem
  -> IO ()
compareRRItems playerRt (recordingEntry, rrItem, _) flowRRItem = do
  when (rrItem /= flowRRItem) $ do
    let flowStep = encodeToStr flowRRItem
    setReplayingError playerRt $ itemMismatch flowStep (show recordingEntry)

getCurrentEntryReplayMode :: PlayerRuntime -> IO EntryReplayingMode
getCurrentEntryReplayMode PlayerRuntime{..} = do
  cur <- readMVar stepMVar
  pure $ fromMaybe Normal $ do
    (RecordingEntry _ mode _ _) <- (!?) recording cur
    pure mode

replayWithGlobalConfig
  :: forall rrItem native
   . RRItem rrItem
  => MockedResult rrItem native
  => PlayerRuntime
  -> IO native
  -> (native -> rrItem)
  -> Either PlaybackError (RecordingEntry, rrItem, native)
  -> IO native
replayWithGlobalConfig playerRt  ioAct mkRRItem eNextRRItemRes = do
  let tag = getTag $ Proxy @rrItem
  let config = checkForReplayConfig playerRt tag
  case config of
    GlobalNoVerify -> case eNextRRItemRes of
      Left err -> setReplayingError playerRt err
      Right (_, _, r) -> pure r
    GlobalNormal    -> case eNextRRItemRes of
        Left err -> setReplayingError playerRt err
        Right stepInfo@(_, _, r) -> do
          compareRRItems playerRt stepInfo $ mkRRItem r
          pure r
    GlobalNoMocking -> ioAct
    GlobalSkip -> ioAct

checkForReplayConfig :: PlayerRuntime -> String -> GlobalReplayingMode
checkForReplayConfig  PlayerRuntime{..} tag | tag `elem` disableMocking  = GlobalNoMocking
                                            | tag `elem` disableVerify   = GlobalNoVerify
                                            | otherwise                  = GlobalNormal

replay
  :: forall rrItem native
   . RRItem rrItem
  => MockedResult rrItem native
  => PlayerRuntime
  -> (native -> rrItem)
  -> IO native
  -> IO native
replay playerRt@PlayerRuntime{..} mkRRItem ioAct
  | getTag (Proxy @rrItem) `elem` skipEntries = ioAct
  | otherwise = do
      entryReplayMode <- getCurrentEntryReplayMode playerRt
      eNextRRItemRes <- popNextRRItemAndResult playerRt
      case entryReplayMode of
        Normal -> do
          replayWithGlobalConfig playerRt ioAct mkRRItem eNextRRItemRes
        NoVerify -> case eNextRRItemRes of
          Left err -> setReplayingError playerRt err
          Right (_, _, r) -> pure r
        NoMock -> ioAct

record
  :: forall rrItem native
   . RRItem rrItem
  => RecorderRuntime
  -> (native -> rrItem)
  -> IO native
  -> IO native
record recorderRt@RecorderRuntime{..} mkRRItem ioAct = do
  native <- ioAct
  let tag = getTag $ Proxy @rrItem
  when (tag `notElem` disableEntries)
    $ pushRecordingEntry recorderRt $ toRecordingEntry (mkRRItem native) 0 Normal
  pure native


withRunMode
  :: RRItem rrItem
  => MockedResult rrItem native
  => RunMode
  -> (native -> rrItem)
  -> IO native
  -> IO native
withRunMode RegularMode _ act = act
withRunMode (RecordingMode recorderRt) mkRRItem act =
  record recorderRt mkRRItem act
withRunMode (ReplayingMode playerRt) mkRRItem act =
  replay playerRt mkRRItem act
