{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}

module EulerHS.Framework.Flow.Interpreter where

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
import qualified EulerHS.Framework.Flow.Entries as P
import qualified Data.Vector as V
import qualified Data.Text as Text
-- import Data.ByteString.Builder

import qualified Database.PostgreSQL.Simple as PGS
import Data.Generics.Product.Positions (getPosition)

executeWithLogSQLite :: (String -> IO ()) -> SQLite.Connection -> SQLite.Query -> IO ()
executeWithLogSQLite log conn q = SQLite.execute_ conn q >> (log $ show q)

beginTransactionSQLite :: (String -> IO ()) -> SQLite.Connection -> IO ()
beginTransactionSQLite    log conn = executeWithLogSQLite log conn "BEGIN TRANSACTION"

commitTransactionSQLite :: (String -> IO ()) -> SQLite.Connection -> IO ()
commitTransactionSQLite   log conn = executeWithLogSQLite log conn "COMMIT TRANSACTION"

rollbackTransactionSQLite :: (String -> IO ()) -> SQLite.Connection -> IO ()
rollbackTransactionSQLite log conn = executeWithLogSQLite log conn "ROLLBACK TRANSACTION"

connect :: T.DBConfig be -> IO (T.DBResult (T.SqlConn be))
connect cfg = do
  eConn <- try $ T.mkSqlConn cfg
  case eConn of
    Left (e :: SomeException) -> pure $ Left $ T.DBError T.ConnectionFailed $ show e
    Right conn -> pure $ Right conn


disconnect :: T.SqlConn beM ->   IO ()
disconnect (T.MockedConn _)         = pure ()
disconnect (T.SQLiteConn _ conn)   = SQLite.close conn
disconnect (T.PostgresConn _ conn) = BP.close conn
disconnect (T.MySQLConn _ conn)    = MySQL.close conn
disconnect (T.PostgresPool _ pool) = DP.destroyAllResources pool
disconnect (T.MySQLPool _ pool)    = DP.destroyAllResources pool
disconnect (T.SQLitePool _ pool)   = DP.destroyAllResources pool

interpretFlowMethod :: R.FlowRuntime -> L.FlowMethod a -> IO a
interpretFlowMethod R.FlowRuntime {..} (L.CallServantAPI bUrl clientAct next) =
  fmap next $ P.withRunMode _runMode (P.mkCallServantAPIEntry bUrl) $ do
    manager <- takeMVar _httpClientManager
    result <- catchAny
      (S.runClientM clientAct (S.mkClientEnv manager bUrl))
      (pure . Left . S.ConnectionError)
    putMVar _httpClientManager manager
    pure result


interpretFlowMethod R.FlowRuntime {..} (L.EvalLogger loggerAct next) =
  fmap next $ -- P.withRunMode _runMode P.mkEvalLoggerEntry $
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
                }

          takeMVar errorMVar *> putMVar errorMVar (Just err)
          throwIO T.ForkedFlowRecordingsMissed

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

interpretFlowMethod R.FlowRuntime {..} (L.InitSqlDBConnection cfg next) = do
  let connTag = getPosition @1 cfg --T.getConnTag cfg
  connMap <- takeMVar _sqlConn
  res <- case (Map.lookup connTag connMap) of
    Just _ -> do
      pure $ Left $ T.DBError T.ConnectionAlreadyExists $ "Connection for " <> connTag <> " already created."
    Nothing -> connect cfg
  case res of
    Right conn -> putMVar _sqlConn $ Map.insert connTag (T.bemToNative conn) connMap
    Left _ -> putMVar _sqlConn connMap

  fmap next $ P.withRunMode _runMode (P.mkInitSqlDBConnectionEntry cfg) $ connect cfg

interpretFlowMethod R.FlowRuntime {..} (L.DeInitSqlDBConnection conn next) =
  fmap next $ P.withRunMode _runMode (P.mkDeInitSqlDBConnectionEntry conn) $ do
    let connTag = getPosition @1 conn
    connMap <- takeMVar _sqlConn
    case (Map.lookup connTag connMap) of
      Nothing -> putMVar _sqlConn connMap
      Just _ -> do
        disconnect conn
        putMVar _sqlConn $ Map.delete connTag connMap


interpretFlowMethod flowRt (L.RunDB conn sqlDbMethod next) = do
  let runMode   = (R._runMode flowRt)
  let errLogger = R.runLogger runMode (R._loggerRuntime . R._coreRuntime $ flowRt)
                . L.logMessage' T.Error ("RUNTIME" :: String)
                . show
  let dbgLogger = R.runLogger runMode (R._loggerRuntime . R._coreRuntime $ flowRt)
                . L.logMessage' T.Debug ("RUNTIME" :: String)
                . show

  fmap next $ P.withRunMode runMode P.mkRunDBEntry $ case conn of
          -- N.B. Beam runner should correspond to the real runtime.
          -- TODO: check for this correspondence or make it unavoidable.
          -- TODO: move begin / commit / rollback into the runner
          -- TODO: MySQL has autocommit mode on by default.
          -- This makes changes to be commited immediately.
          -- TODO: check for what's to do with transactions.
          (T.MySQLConn _ mySQLConn) -> do
            let begin    = pure ()              -- Seems no begin transaction in MySQL
            let commit   = MySQL.commit mySQLConn
            let rollback = MySQL.rollback mySQLConn

            map (first $ T.DBError T.SomeError . show) $
              try @_ @SomeException $ bracketOnError begin (const rollback) $ const $ do
                res <- R.runSqlDB conn dbgLogger sqlDbMethod
                commit
                return res

          -- TODO: move begin / commit / rollback into the runner
          (T.SQLiteConn _ sqliteConn) -> do
            let begin    = beginTransactionSQLite      errLogger sqliteConn
            let commit   = commitTransactionSQLite     errLogger sqliteConn
            let rollback = rollbackTransactionSQLite   errLogger sqliteConn

            map (first $ T.DBError T.SomeError . show) $
              try @_ @SomeException $ bracketOnError begin (const rollback) $ const $ do
                res <- R.runSqlDB conn dbgLogger sqlDbMethod
                commit
                return res

          -- TODO: move begin / commit / rollback into the runner
          (T.PostgresConn _ c) -> do
            let begin    = PGS.begin c
            let commit   = PGS.commit c
            let rollback = PGS.rollback c

            map (first $ T.DBError T.SomeError . show) $
              try @_ @SomeException $ bracketOnError begin (const rollback) $ const $ do
                res <- R.runSqlDB conn dbgLogger sqlDbMethod
                commit
                return res

          (T.PostgresPool _ pool) -> DP.withResource pool $ \connection -> do
            let begin    = PGS.begin connection
            let commit   = PGS.commit connection
            let rollback = PGS.rollback connection

            map (first $ T.DBError T.SomeError . show) $
              try @_ @SomeException $ bracketOnError begin (const rollback) $ const $ do
                res <- R.runSqlDB conn dbgLogger sqlDbMethod
                commit
                return res

          (T.MySQLPool _ pool) -> DP.withResource pool $ \connection -> do
            let begin    = pure ()
            let commit   = MySQL.commit connection
            let rollback = MySQL.rollback connection

            map (first $ T.DBError T.SomeError . show) $
              try @_ @SomeException $ bracketOnError begin (const rollback) $ const $ do
                res <- R.runSqlDB conn dbgLogger sqlDbMethod
                commit
                return res

          (T.SQLitePool _ pool) -> DP.withResource pool $ \connection -> do
            let begin    = beginTransactionSQLite      errLogger connection
            let commit   = commitTransactionSQLite     errLogger connection
            let rollback = rollbackTransactionSQLite   errLogger connection

            map (first $ T.DBError T.SomeError . show) $
              try @_ @SomeException $ bracketOnError begin (const rollback) $ const $ do
                res <- R.runSqlDB conn dbgLogger sqlDbMethod
                commit
                return res

          (T.MockedConn _) -> error $ "MockedSqlConn not implemented"

interpretFlowMethod R.FlowRuntime {..} (L.RunKVDB act next) = do
  fmap next $ do
    connections <- readMVar _connections
    case Map.lookup "redis" connections of
      Just (kvdbconn) -> R.runKVDB _runMode kvdbconn act
      Nothing -> pure $ Left $ ExceptionMessage "Can't find redis connection"

runFlow :: R.FlowRuntime -> L.Flow a -> IO a
runFlow flowRt = foldF (interpretFlowMethod flowRt)


forkRecorderRt :: Text -> T.RecorderRuntime -> IO (T.Recording, T.RecorderRuntime)
forkRecorderRt newFlowGUID rrt = do
  let T.RecorderRuntime
        { recording = T.Recording{..}
        , ..
        } = rrt

  freshRecording <- T.Recording <$> newMVar V.empty <*> newMVar Map.empty
  emptyRecording <- T.Recording <$> newEmptyMVar    <*> newEmptyMVar

  let forkRuntime = T.RecorderRuntime
        { flowGUID        = newFlowGUID
        , recording       = freshRecording
        , ..
        }

  forkedRecs <- takeMVar $ forkedRecordingsVar
  putMVar forkedRecordingsVar $
    Map.insert newFlowGUID emptyRecording forkedRecs

  pure
    ( emptyRecording
    , forkRuntime
    )
