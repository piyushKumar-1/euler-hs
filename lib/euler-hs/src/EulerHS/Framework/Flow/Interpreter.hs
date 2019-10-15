{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

module EulerHS.Framework.Flow.Interpreter where

import           EulerHS.Prelude
import           Control.Exception               (throwIO)
import           Data.Aeson                      (encode, decode)
import qualified Data.ByteString.Lazy            as BSL
import qualified Data.Map                        as Map
import qualified Data.UUID                       as UUID (toText)
import qualified Data.UUID.V4                    as UUID (nextRandom)
import qualified Servant.Client                  as S
import           System.Process (shell, readCreateProcess)

import qualified Database.SQLite.Simple as SQLite
import qualified Database.MySQL.Base as MySQL
import qualified Database.Beam.Sqlite as BS
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
import qualified EulerHS.Framework.Playback.Entries as P
import qualified Data.Vector as V
import qualified Data.Text as T

executeWithLogSQLite :: (String -> IO ()) -> SQLite.Connection -> SQLite.Query -> IO ()
executeWithLogSQLite log conn q = SQLite.execute_ conn q >> (log $ show q)

beginTransactionSQLite :: (String -> IO ()) -> SQLite.Connection -> IO ()
beginTransactionSQLite    log conn = executeWithLogSQLite log conn "BEGIN TRANSACTION"

commitTransactionSQLite :: (String -> IO ()) -> SQLite.Connection -> IO ()
commitTransactionSQLite   log conn = executeWithLogSQLite log conn "COMMIT TRANSACTION"

rollbackTransactionSQLite :: (String -> IO ()) -> SQLite.Connection -> IO ()
rollbackTransactionSQLite log conn = executeWithLogSQLite log conn "ROLLBACK TRANSACTION"

connect :: T.DBConfig be -> IO (T.DBResult (T.SqlConn be))
connect T.MockConfig  = pure $ Right T.MockedConn

connect (T.MySQLConf cfg) = do
  eConn <- try $ MySQL.connect $ T.toMySQLConnectInfo cfg
  case eConn of
    Left (e :: SomeException) -> pure $ Left $ T.DBError T.ConnectionFailed $ show e
    Right conn -> pure $ Right $ T.MySQLConn conn

connect (T.SQLiteConfig dbName) = do
  eConn <- try $ SQLite.open dbName
  case eConn of
    Left (e :: SomeException) -> pure $ Left $ T.DBError T.ConnectionFailed $ show e
    Right conn -> pure $ Right $ T.SQLiteConn conn
connect (T.PostgresConf pgConf) = error "Postgres connect not implemented"


interpretFlowMethod :: R.FlowRuntime -> L.FlowMethod a -> IO a
interpretFlowMethod R.FlowRuntime {..} (L.CallServantAPI bUrl clientAct next) =
  fmap next $ withRunMode _runMode (P.mkCallServantAPIEntry bUrl) $ do
    manager <- takeMVar _httpClientManager
    result <- catchAny
      (S.runClientM clientAct (S.mkClientEnv manager bUrl))
      (pure . Left . S.ConnectionError)
    putMVar _httpClientManager manager
    pure result


interpretFlowMethod R.FlowRuntime {..} (L.EvalLogger loggerAct next) =
  fmap next $ withRunMode _runMode P.mkEvalLoggerEntry $
    R.runLogger (R._loggerRuntime _coreRuntime) loggerAct

interpretFlowMethod R.FlowRuntime {..} (L.RunIO ioAct next) =
  next <$> withRunMode _runMode P.mkRunIOEntry ioAct

interpretFlowMethod R.FlowRuntime {..} (L.GetOption k next) =
  fmap next $ withRunMode _runMode (P.mkGetOptionEntry k) $ do
    m <- readMVar _options
    pure $ decode . BSL.fromStrict =<< Map.lookup (BSL.toStrict $ encode k) m

interpretFlowMethod R.FlowRuntime {..} (L.SetOption k v next) =
  fmap next $ withRunMode _runMode (P.mkSetOptionEntry k v) $ do
    m <- takeMVar _options
    let newMap = Map.insert (BSL.toStrict $ encode k) (BSL.toStrict $ encode v) m
    putMVar _options newMap

interpretFlowMethod R.FlowRuntime {_runMode} (L.GenerateGUID next) =
  next <$> withRunMode _runMode P.mkGenerateGUIDEntry
    (UUID.toText <$> UUID.nextRandom)

interpretFlowMethod R.FlowRuntime {_runMode} (L.RunSysCmd cmd next) =
  next <$> withRunMode _runMode
    (P.mkRunSysCmdEntry cmd)
    (readCreateProcess (shell cmd) "")

interpretFlowMethod rt (L.Fork desc flowGUID flow next) = do
  mbForkedRt <- forkBackendRuntime flowGUID rt
  void $ withRunMode (R._runMode rt) (P.mkForkEntry desc flowGUID)
    (case mbForkedRt of
      Nothing -> putStrLn (flowGUID <> " Failed to fork flow.") *> pure ()
      Just forkedBrt -> forkF forkedBrt flow *> pure ())
  pure $ next ()


interpretFlowMethod R.FlowRuntime {_runMode} (L.ThrowException ex next) =
  fmap next $ withRunMode _runMode (P.mkThrowExceptionEntry ex) $ throwIO ex

interpretFlowMethod _ (L.InitSqlDBConnection cfg next) =
  next <$> connect cfg

interpretFlowMethod flowRt (L.RunDB conn sqlDbMethod next) = do
  let errLogger   = R.runLogger (R._loggerRuntime . R._coreRuntime $ flowRt)
                  . L.logMessage' T.Error ("RUNTIME" :: String)
                  . show
  let dbgLogger   = R.runLogger (R._loggerRuntime . R._coreRuntime $ flowRt)
                  . L.logMessage' T.Debug ("RUNTIME" :: String)
                  . show

  fmap next $ withRunMode (R._runMode flowRt) P.mkRunDBEntry $ case conn of
    T.MockedConn -> error "not implemented MockedSql"

    -- N.B. Beam runner should correspond to the real runtime.
    -- TODO: check for this correspondence or make it unavoidable.


    -- TODO: move begin / commit / rollback into the runner
    -- TODO: MySQL has autocommit mode on by default.
    -- This makes changes to be commited immediately.
    -- TODO: check for what's to do with transactions.
    T.MySQLConn mySQLConn -> do
      let begin    = pure ()              -- Seems no begin transaction in MySQL
      let commit   = MySQL.commit mySQLConn
      let rollback = MySQL.rollback mySQLConn

      map (first $ T.DBError T.SomeError . show) $
        try @_ @SomeException $ bracketOnError begin (const rollback) $ const $ do
          res <- R.runSqlDB conn dbgLogger sqlDbMethod
          commit
          return res

    -- TODO: move begin / commit / rollback into the runner
    T.SQLiteConn sqliteConn -> do
      let begin    = beginTransactionSQLite      errLogger sqliteConn
      let commit   = commitTransactionSQLite     errLogger sqliteConn
      let rollback = rollbackTransactionSQLite   errLogger sqliteConn

      map (first $ T.DBError T.SomeError . show) $
        try @_ @SomeException $ bracketOnError begin (const rollback) $ const $ do
          res <- R.runSqlDB conn dbgLogger sqlDbMethod
          commit
          return res

    -- TODO: move begin / commit / rollback into the runner
    T.PostgresConn connection -> error "Postgres not implemented."

interpretFlowMethod R.FlowRuntime {..} (L.RunKVDB act next) = do
  fmap next $ withRunMode _runMode P.mkRunKVDBEntry $ do
    connections <- readMVar _connections
    case Map.lookup "redis" connections of
      Just (kvdbconn) -> R.runKVDB kvdbconn act
      Nothing -> pure $ Left $ ExceptionMessage "Can't find redis connection"

forkF :: R.FlowRuntime -> L.Flow a -> IO ()
forkF rt flow = void $ forkIO $ void $ runFlow rt flow

runFlow :: R.FlowRuntime -> L.Flow a -> IO a
runFlow flowRt = foldF (interpretFlowMethod flowRt)



withRunMode
  :: T.RRItem rrItem
  => T.MockedResult rrItem native
  => T.RunMode
  -> (native -> rrItem)
  -> IO native
  -> IO native

withRunMode T.RegularMode _ act = act

withRunMode (T.RecordingMode recorderRt) mkRRItem act
  = P.record recorderRt mkRRItem act

withRunMode (T.ReplayingMode playerRt) mkRRItem act
  = P.replay playerRt mkRRItem act

forkPlayerRt :: Text -> T.PlayerRuntime -> IO (Maybe T.PlayerRuntime)
forkPlayerRt newFlowGUID_ T.PlayerRuntime{..} =
  case Map.lookup newFlowGUID forkedFlowRecordings of
    Nothing -> do
      let missedRecsErr = Just $ T.PlaybackError
            { errorType    = T.ForkedFlowRecordingsMissed
            , errorMessage = "No recordings found for forked flow: " <> newFlowGUID
            }
      forkedFlowErrors <- takeMVar forkedFlowErrorsVar
      let forkedFlowErrors' = Map.insert newFlowGUID missedRecsErr forkedFlowErrors
      putMVar forkedFlowErrorsVar forkedFlowErrors'
      pure Nothing
    Just recording' -> do
      stepVar'  <- newMVar 0
      errorVar' <- newEmptyMVar
      pure $ Just T.PlayerRuntime
        { flowGUID = newFlowGUID
        , stepMVar = stepVar'
        , errorMVar = errorVar'
        , recording = recording'
        , ..
        }
  where
      newFlowGUID :: String
      newFlowGUID = T.unpack newFlowGUID_

forkRecorderRt :: Text -> T.RecorderRuntime -> IO T.RecorderRuntime
forkRecorderRt newFlowGUID_ T.RecorderRuntime{..} = do
  recordingVar <- newMVar V.empty
  forkedRecs   <- takeMVar forkedRecordingsVar
  let newFlowGUID = T.unpack newFlowGUID_
  let forkedRecs' = Map.insert newFlowGUID recordingVar forkedRecs
  putMVar forkedRecordingsVar forkedRecs'
  pure T.RecorderRuntime
    { flowGUID = newFlowGUID
    , recordingMVar = recordingVar
    , ..
    }

forkBackendRuntime :: Text -> R.FlowRuntime -> IO (Maybe R.FlowRuntime)
forkBackendRuntime flowGUID R.FlowRuntime{..} = do
  mbForkedMode <- case _runMode of
    T.RegularMode              -> pure $ Just T.RegularMode
    T.RecordingMode recorderRt -> Just . T.RecordingMode <$> forkRecorderRt flowGUID recorderRt
    T.ReplayingMode playerRt   -> do
      mbRt <- forkPlayerRt flowGUID playerRt
      pure $ T.ReplayingMode <$> mbRt

  case mbForkedMode of
    Nothing         -> pure Nothing
    Just forkedMode -> pure $ Just $ R.FlowRuntime
          { _runMode = forkedMode
          , ..
          }
