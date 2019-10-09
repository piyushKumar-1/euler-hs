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
import qualified Database.Beam.Sqlite as BS
import           EulerHS.Core.Types.KVDB
import qualified EulerHS.Core.Language as L
import qualified EulerHS.Core.Runtime as R
import qualified EulerHS.Core.Interpreters as R
import qualified EulerHS.Framework.Runtime as R
import qualified EulerHS.Core.Types as T
import qualified EulerHS.Framework.Language as L

executeWithLog :: (String -> IO ()) -> SQLite.Connection -> SQLite.Query -> IO()
executeWithLog log conn q = SQLite.execute_ conn q >> (log $ show q)

beginTransaction :: (String -> IO ()) -> SQLite.Connection -> IO ()
beginTransaction    log conn = executeWithLog log conn "BEGIN TRANSACTION"

commitTransaction :: (String -> IO ()) -> SQLite.Connection -> IO ()
commitTransaction   log conn = executeWithLog log conn "COMMIT TRANSACTION"

rollbackTransaction :: (String -> IO ()) -> SQLite.Connection -> IO ()
rollbackTransaction log conn = executeWithLog log conn "ROLLBACK TRANSACTION"


connect :: T.DBConfig -> IO (T.DBResult T.SqlConn)
connect T.MockConfig  = pure $ Right $ T.MockedConn

connect (T.SQLiteConfig dbName) = do
  eConn <- try $ SQLite.open dbName
  case eConn of
    Left (e :: SomeException) -> pure $ Left $ T.DBError T.ConnectionFailed $ show e
    Right conn -> pure $ Right $ T.SQLiteConn conn


interpretFlowMethod :: R.FlowRuntime -> L.FlowMethod a -> IO a
interpretFlowMethod _ (L.CallAPI _ _) = error "CallAPI not yet supported."

interpretFlowMethod (R.FlowRuntime _ managerVar _) (L.CallServantAPI bUrl clientAct next) = do
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

interpretFlowMethod _ (L.Connect cfg next) =
  next <$> connect cfg

interpretFlowMethod _ (L.RunDB conn dbm next) =
  next <$> case conn of
    T.MockedConn -> error "not implemented MockedSql"

    T.SQLiteConn connection ->
      let
        begin    = beginTransaction      putStrLn connection
        commit   = commitTransaction     putStrLn connection
        rollback = rollbackTransaction   putStrLn connection
        run      = BS.runBeamSqliteDebug putStrLn connection
      in map (first $ T.DBError T.SomeError . show) $
        try @_ @SomeException $ bracketOnError begin (const rollback) $ const $ do
          res <- run $ R.runSqlDB dbm
          commit
          return res

interpretFlowMethod R.FlowRuntime {..} (L.RunKVDB act next) = do
  next <$> do
    connections <- readMVar _connections
    case Map.lookup "redis" connections of
      Just (R.Redis a) -> R.runKVDB a act
      Nothing -> pure $ Left $ ExceptionMessage "No such key"

forkF :: R.FlowRuntime -> L.Flow a -> IO ()
forkF rt flow = void $ forkIO $ void $ runFlow rt flow

runFlow :: R.FlowRuntime -> L.Flow a -> IO a
runFlow flowRt = foldF (interpretFlowMethod flowRt)
