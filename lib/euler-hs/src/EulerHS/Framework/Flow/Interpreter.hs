{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}

module EulerHS.Framework.Flow.Interpreter
  (
    -- * Flow Interpreter
    runFlow
  ) where

import           EulerHS.Prelude
import           Control.Exception               (throwIO)
import qualified Data.Aeson                      as A
import qualified Data.ByteString.Lazy            as BSL
import qualified Data.Map                        as Map
import qualified Data.UUID                       as UUID (toText)
import qualified Data.UUID.V4                    as UUID (nextRandom)
import qualified Servant.Client                  as S
import           System.Process (shell, readCreateProcess)

import Data.Coerce (coerce)
import qualified Data.Pool              as DP
import qualified Database.Redis         as DR
import qualified Database.SQLite.Simple as SQLite
import qualified Database.MySQL.Base as MySQL
import qualified Database.Beam.Sqlite as BS
import qualified Database.Beam.Postgres as BP
import           EulerHS.Core.Types.KVDB
import qualified EulerHS.Core.Runtime as R
import qualified EulerHS.Core.Interpreters as R
import qualified EulerHS.Framework.Runtime as R
import qualified EulerHS.Core.Types as T
import qualified EulerHS.Framework.Language as L

-- TODO: no explicit dependencies from languages.
import qualified EulerHS.Core.SqlDB.Language as L
import qualified EulerHS.Core.Logger.Language as L
import qualified EulerHS.Core.Playback.Machine as P
import qualified EulerHS.Core.Playback.Entries as P
import qualified Data.Vector as V
import qualified Data.Text as Text

import qualified Database.PostgreSQL.Simple as PGS
import Data.Generics.Product.Positions (getPosition)

connect :: T.DBConfig be -> IO (T.DBResult (T.SqlConn be))
connect cfg = do
  eConn <- try $ T.mkSqlConn cfg
  case eConn of
    Left (e :: SomeException) -> pure $ Left $ T.DBError T.ConnectionFailed $ show e
    Right conn -> pure $ Right conn

connectRedis :: T.KVDBConfig -> IO (T.KVDBAnswer T.KVDBConn)
connectRedis cfg = do
  eConn <- try $ T.mkRedisConn cfg
  case eConn of
    Left (e :: SomeException) -> pure $ Left $ T.ExceptionMessage $ show e
    Right conn -> pure $ Right conn

disconnect :: T.SqlConn beM ->   IO ()
disconnect (T.MockedConn _)         = pure ()
disconnect (T.PostgresPool _ pool) = DP.destroyAllResources pool
disconnect (T.MySQLPool _ pool)    = DP.destroyAllResources pool
disconnect (T.SQLitePool _ pool)   = DP.destroyAllResources pool

interpretFlowMethod :: R.FlowRuntime -> L.FlowMethod a -> IO a
interpretFlowMethod R.FlowRuntime {..} (L.CallServantAPI bUrl clientAct next) =
  fmap next $ P.withRunMode _runMode (P.mkCallServantAPIEntry bUrl) $ do
    result <- catchAny
      (S.runClientM clientAct (S.mkClientEnv _httpClientManager bUrl))
      (pure . Left . S.ConnectionError)
    pure result


interpretFlowMethod R.FlowRuntime {..} (L.EvalLogger loggerAct next) =
  fmap next $
    R.runLogger _runMode (R._loggerRuntime _coreRuntime) loggerAct

interpretFlowMethod R.FlowRuntime {..} (L.RunIO ioAct next) =
  next <$> P.withRunMode _runMode P.mkRunIOEntry ioAct

interpretFlowMethod R.FlowRuntime {..} (L.GetOption k next) =
  fmap next $ P.withRunMode _runMode (P.mkGetOptionEntry k) $ do
    m <- readMVar _options
    pure $ A.decode . BSL.fromStrict =<< Map.lookup (BSL.toStrict $ A.encode k) m

interpretFlowMethod R.FlowRuntime {..} (L.SetOption k v next) =
  fmap next $ P.withRunMode _runMode (P.mkSetOptionEntry k v) $ do
    m <- takeMVar _options
    let newMap = Map.insert (BSL.toStrict $ A.encode k) (BSL.toStrict $ A.encode v) m
    putMVar _options newMap

interpretFlowMethod R.FlowRuntime {_runMode} (L.GenerateGUID next) = do
  next <$> P.withRunMode _runMode P.mkGenerateGUIDEntry
    (UUID.toText <$> UUID.nextRandom)

interpretFlowMethod R.FlowRuntime {_runMode} (L.RunSysCmd cmd next) =
  next <$> P.withRunMode _runMode
    (P.mkRunSysCmdEntry cmd)
    (readCreateProcess (shell cmd) "")

----------------------------------------------------------------------
interpretFlowMethod rt (L.Fork desc newFlowGUID flow next) = do
  case R._runMode rt of
    T.RegularMode              -> void $ forkIO $ void $ runFlow rt flow
    T.RecordingMode T.RecorderRuntime{recording = T.Recording{..}, ..} -> do
      finalRecordingMVar       <- newEmptyMVar
      finalForkedRecordingsVar <- newEmptyMVar

      forkRecordingMVar        <- newMVar V.empty
      forkForkedRecordingsVar   <- newMVar Map.empty

      let freshRecording = T.Recording forkRecordingMVar  forkForkedRecordingsVar
      let emptyRecording = T.Recording finalRecordingMVar finalForkedRecordingsVar

      let forkRuntime = T.RecorderRuntime
            { flowGUID  = newFlowGUID
            , recording = freshRecording
            , ..
            }

      forkedRecs <- takeMVar $ forkedRecordingsVar
      putMVar forkedRecordingsVar $
        Map.insert newFlowGUID emptyRecording forkedRecs

      let newRt = rt {R._runMode = T.RecordingMode forkRuntime}

      void $ forkIO $ do
        _ <- runFlow newRt flow
        putMVar finalRecordingMVar       =<< readMVar forkRecordingMVar
        putMVar finalForkedRecordingsVar =<< readMVar forkForkedRecordingsVar
        pure ()

----------------------------------------------------------------------

    T.ReplayingMode playerRt -> do
      let
        T.PlayerRuntime
          { rerror       = T.ReplayErrors   {..}
          , resRecording = T.ResultRecording{ forkedRecordings }
          , ..
          } = playerRt

      case Map.lookup newFlowGUID forkedRecordings of
        Nothing -> do
          let
            err =
              T.PlaybackError
                { errorType    = T.ForkedFlowRecordingsMissed
                , errorMessage = "No recordings found for forked flow: " <> Text.unpack newFlowGUID
                , errorFlowGUID = flowGUID }

          takeMVar errorMVar *> putMVar errorMVar (Just err)
          throwIO $ T.ReplayingException err

        Just recording -> do
          stepVar           <- newMVar 0

          finalErrorMVar          <- newEmptyMVar
          finalForkedFlowErrorVar <- newEmptyMVar

          forkErrorMVar           <- newMVar Nothing
          forkForkedFlowErrorVar  <- newMVar Map.empty

          let freshReplayErrors = T.ReplayErrors forkErrorMVar  forkForkedFlowErrorVar
          let finalReplayErrors = T.ReplayErrors finalErrorMVar finalForkedFlowErrorVar

          let forkRuntime = T.PlayerRuntime
                { flowGUID     = newFlowGUID
                , stepMVar     = stepVar
                , resRecording = recording
                , rerror       = freshReplayErrors
                , ..
                }

          forkedFlowErrs <- takeMVar forkedFlowErrorsVar

          putMVar forkedFlowErrorsVar $
            Map.insert newFlowGUID finalReplayErrors forkedFlowErrs

          let newRt = rt {R._runMode = T.ReplayingMode forkRuntime}
          void $ forkIO $ do
            _ <- try @_ @SomeException $ runFlow newRt flow
            putMVar finalErrorMVar          =<< readMVar forkErrorMVar
            putMVar finalForkedFlowErrorVar =<< readMVar forkForkedFlowErrorVar
            pure ()

----------------------------------------------------------------------

  fmap next $
    P.withRunMode (R._runMode rt) (P.mkForkEntry desc newFlowGUID) (pure ())

interpretFlowMethod R.FlowRuntime {_runMode} (L.ThrowException ex next) =
  fmap next $ P.withRunMode _runMode (P.mkThrowExceptionEntry ex) $ throwIO ex

interpretFlowMethod R.FlowRuntime {..} (L.InitSqlDBConnection cfg next) =
  fmap next $ P.withRunMode _runMode (P.mkInitSqlDBConnectionEntry cfg) $ do
    let connTag = getPosition @1 cfg
    connMap <- takeMVar _sqldbConnections
    res <- case (Map.lookup connTag connMap) of
      Just _ -> pure $ Left $ T.DBError T.ConnectionAlreadyExists $ "Connection for " <> connTag <> " already created."
      Nothing -> connect cfg
    case res of
      Right conn -> putMVar _sqldbConnections $ Map.insert connTag (T.bemToNative conn) connMap
      Left _ -> putMVar _sqldbConnections connMap
    pure res

interpretFlowMethod R.FlowRuntime {..} (L.DeInitSqlDBConnection conn next) =
  fmap next $ P.withRunMode _runMode (P.mkDeInitSqlDBConnectionEntry conn) $ do
    let connTag = getPosition @1 conn
    connMap <- takeMVar _sqldbConnections
    case (Map.lookup connTag connMap) of
      Nothing -> putMVar _sqldbConnections connMap
      Just _ -> do
        disconnect conn
        putMVar _sqldbConnections $ Map.delete connTag connMap

interpretFlowMethod R.FlowRuntime {..} (L.GetSqlDBConnection cfg next) =
  fmap next $ P.withRunMode _runMode (P.mkGetSqlDBConnectionEntry cfg) $ do
    let connTag = getPosition @1 cfg
    connMap <- readMVar _sqldbConnections
    pure $ case (Map.lookup connTag connMap) of
      Just conn -> Right $ T.nativeToBem connTag conn
      Nothing   -> Left $ T.DBError T.ConnectionDoesNotExist $ "Connection for " <> connTag <> " does not exists."

interpretFlowMethod R.FlowRuntime {..} (L.InitKVDBConnection cfg next) =
  fmap next $ P.withRunMode _runMode (P.mkInitKVDBConnectionEntry cfg) $ do
    let connTag = getPosition @1 cfg
    let connTagBS = encodeUtf8 connTag
    connections <- readMVar _kvdbConnections
    res <- case Map.lookup connTagBS connections of
      Just _  -> pure $ Left $
        ExceptionMessage $ Text.unpack $ "Connection for " <> connTag <> " already created."
      Nothing -> do
        connectRedis cfg
    case res of
      Left _ -> putMVar _kvdbConnections connections
      Right conn -> putMVar _kvdbConnections $
        Map.insert connTagBS (redisToNative conn) connections
    pure res

interpretFlowMethod R.FlowRuntime {..} (L.DeInitKVDBConnection conn next) =
  fmap next $ P.withRunMode _runMode (P.mkDeInitKVDBConnectionEntry conn) $ do
    let connTagBS = encodeUtf8 $ getPosition @1 conn
    connections <- readMVar _kvdbConnections
    case (Map.lookup connTagBS connections) of
      Nothing -> putMVar _kvdbConnections connections
      Just _ -> do
        R.kvDisconnect $ redisToNative conn
        putMVar _kvdbConnections $ Map.delete connTagBS connections

interpretFlowMethod R.FlowRuntime {..} (L.GetKVDBConnection cfg next) =
  fmap next $ P.withRunMode _runMode (P.mkGetKVDBConnectionEntry cfg) $ do
    let connTag = getPosition @1 cfg
    connMap <- readMVar _kvdbConnections
    pure $ case (Map.lookup (encodeUtf8 connTag) connMap) of
      Just conn -> Right $ T.nativeToRedis connTag conn
      Nothing   -> Left $
        ExceptionMessage $ Text.unpack $ "Connection for " <> connTag <> " does not exists."

interpretFlowMethod flowRt (L.RunDB conn sqlDbMethod next) = do
  let runMode   = R._runMode flowRt
  let dbgLogger = R.runLogger runMode (R._loggerRuntime . R._coreRuntime $ flowRt)
                . L.logMessage' T.Debug ("RUNTIME" :: String)
                . show

  fmap next $ P.withRunMode runMode P.mkRunDBEntry $ case conn of
          -- N.B. Beam runner should correspond to the real runtime.
          -- TODO: check for this correspondence or make it unavoidable.
          -- TODO: MySQL has autocommit mode on by default.
          -- This makes changes to be commited immediately.
          -- TODO: check for what's to do with transactions.

          (T.MockedConn _) -> error $ "MockedSqlConn not implemented"

          _                ->
            map (first $ T.DBError T.SomeError . show) $
              try @_ @SomeException $ R.runSqlDB conn dbgLogger sqlDbMethod

interpretFlowMethod R.FlowRuntime {..} (L.RunKVDB act next) = do
  fmap next $ R.runKVDB _runMode _kvdbConnections act

runFlow :: R.FlowRuntime -> L.Flow a -> IO a
runFlow flowRt = foldF (interpretFlowMethod flowRt)
