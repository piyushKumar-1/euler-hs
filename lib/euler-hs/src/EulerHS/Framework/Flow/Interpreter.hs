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
interpretFlowMethod _ (L.CallAPI _ _) = error "CallAPI not yet supported."

interpretFlowMethod (R.FlowRuntime _ managerVar _ _) (L.CallServantAPI bUrl clientAct next) = do
  manager <- takeMVar managerVar
  result <- next <$> catchAny (S.runClientM clientAct (S.mkClientEnv manager bUrl)) (pure . Left . S.ConnectionError)
  putMVar managerVar manager
  pure result

interpretFlowMethod (R.FlowRuntime coreRt _ _ _) (L.EvalLogger loggerAct next) =
  next <$> R.runLogger (R._loggerRuntime coreRt) loggerAct

interpretFlowMethod _ (L.RunIO ioAct next) =
  next <$> ioAct

interpretFlowMethod R.FlowRuntime {..} (L.GetOption k next) =
  next <$> maybeValue
  where
    maybeValue = do
      m <- readMVar _options
      pure $ decode . BSL.fromStrict =<< Map.lookup (BSL.toStrict $ encode k) m

interpretFlowMethod R.FlowRuntime {..} (L.SetOption k v next) =
  next <$> set
  where
    set = do
      m <- takeMVar _options
      let newMap = Map.insert (BSL.toStrict $ encode k) (BSL.toStrict $ encode v) m
      putMVar _options newMap

interpretFlowMethod _ (L.GenerateGUID next) =
  next . UUID.toText <$> UUID.nextRandom

interpretFlowMethod _ (L.RunSysCmd cmd next) =
  next <$> (readCreateProcess (shell cmd) "")

interpretFlowMethod rt (L.Fork _ _ flow next) =
  next <$> forkF rt flow

interpretFlowMethod _ (L.ThrowException ex next) =
  next <$> throwIO ex

interpretFlowMethod _ (L.InitSqlDBConnection cfg next) =
  next <$> connect cfg

interpretFlowMethod flowRt (L.RunDB conn sqlDbMethod next) = do
  let errLogger   = R.runLogger (R._loggerRuntime . R._coreRuntime $ flowRt)
                  . L.logMessage' T.Error ("RUNTIME" :: String)
                  . show
  let dbgLogger   = R.runLogger (R._loggerRuntime . R._coreRuntime $ flowRt)
                  . L.logMessage' T.Debug ("RUNTIME" :: String)
                  . show

  next <$> case conn of
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
  next <$> do
    connections <- readMVar _connections
    case Map.lookup "redis" connections of
      Just (kvdbconn) -> R.runKVDB kvdbconn act
      Nothing -> pure $ Left $ ExceptionMessage "Can't find redis connection"

forkF :: R.FlowRuntime -> L.Flow a -> IO ()
forkF rt flow = void $ forkIO $ void $ runFlow rt flow

runFlow :: R.FlowRuntime -> L.Flow a -> IO a
runFlow flowRt = foldF (interpretFlowMethod flowRt)
