{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE DeriveAnyClass        #-}

module EulerHS.Framework.Flow.Language where

import           EulerHS.Prelude

import           Servant.Client (ClientM, ClientError, BaseUrl)

import qualified EulerHS.Core.Types as T
import          EulerHS.Core.Language (Logger, logMessage', KVDB)
import qualified EulerHS.Core.Language as L
import qualified EulerHS.Framework.Types as T
import qualified Database.Beam as B
import qualified Database.Beam.Backend.SQL as B

type Description = Text

type ForkGUID = Text

-- | Flow language.
data FlowMethod next where
  CallServantAPI
    :: T.JSONEx a
    => BaseUrl
    -> ClientM a
    -> (Either ClientError a -> next)
    -> FlowMethod next

  EvalLogger
    :: Logger a
    -> (a -> next)
    -> FlowMethod next

  RunIO
    :: T.JSONEx a
    => IO a
    -> (a -> next)
    -> FlowMethod next

  GetOption
    :: T.OptionEntity k v
    => k
    -> (Maybe v -> next)
    -> FlowMethod next

  SetOption
    :: T.OptionEntity k v
    => k
    -> v
    -> (() -> next)
    -> FlowMethod next

  GenerateGUID
    :: (Text -> next)
    -> FlowMethod next

  RunSysCmd
    :: String
    -> (String -> next)
    -> FlowMethod next

  Fork
    :: Description
    -> ForkGUID
    -> Flow s
    -> (() -> next)
    -> FlowMethod next

  ThrowException
    :: forall a e next
     . Exception e
    => e
    -> (a -> next)
    -> FlowMethod next

  -- TODO: DeInitSqlDBConnection :: _ -> FlowMethod next

  InitSqlDBConnection
    :: T.DBConfig beM
    -> (T.DBResult (T.SqlConn beM) -> next)
    -> FlowMethod next

  DeInitSqlDBConnection
    :: T.SqlConn beM
    -> (() -> next)
    -> FlowMethod next

  RunDB
    :: T.JSONEx a
    => T.SqlConn beM
    -> L.SqlDB beM a
    -> (T.DBResult a -> next)
    -> FlowMethod next

  RunKVDB
    :: KVDB a
    -> (T.KVDBAnswer a -> next)
    -> FlowMethod next

instance Functor FlowMethod where
  fmap f (CallServantAPI bUrl clientAct next) = CallServantAPI bUrl clientAct (f . next)

  fmap f (EvalLogger logAct next)             = EvalLogger logAct (f . next)

  fmap f (GenerateGUID next)                  = GenerateGUID (f . next)

  fmap f (RunSysCmd cmd next)                 = RunSysCmd cmd (f . next)

  fmap f (Fork desc guid fflow next)          = Fork desc guid fflow (f . next)

  fmap f (ThrowException message next)        = ThrowException message (f . next)

  fmap f (RunIO ioAct next)                   = RunIO ioAct (f . next)

  fmap f (GetOption k next)                   = GetOption k (f . next)

  fmap f (SetOption k v next)                 = SetOption k v (f . next)

  fmap f (InitSqlDBConnection cfg next)       = InitSqlDBConnection cfg (f . next)

  fmap f (DeInitSqlDBConnection conn next)    = DeInitSqlDBConnection conn (f.next)

  fmap f (RunDB conn sqlDbAct next)           = RunDB conn sqlDbAct (f . next)

  fmap f (RunKVDB act next)                   = RunKVDB act (f . next)

type Flow = F FlowMethod

callServantAPI
  :: T.JSONEx a
  => BaseUrl
  -> ClientM a
  -> Flow (Either ClientError a)
callServantAPI url cl = liftFC $ CallServantAPI url cl id

-- callAPI :: BaseUrl -> ClientM a -> Flow (Either ClientError a)
-- callAPI = callServantAPI

evalLogger' :: (ToJSON a, FromJSON a) => Logger a -> Flow a
evalLogger' logAct = liftFC $ EvalLogger logAct id

-- | Log message with Info level.
logInfo :: Show tag => tag -> T.Message -> Flow ()
logInfo tag msg = evalLogger' $ logMessage' T.Info tag msg

-- | Log message with Error level.
logError :: Show tag => tag -> T.Message -> Flow ()
logError tag msg = evalLogger' $ logMessage' T.Error tag msg

-- | Log message with Debug level.
logDebug :: Show tag => tag -> T.Message -> Flow ()
logDebug tag msg = evalLogger' $ logMessage' T.Debug tag msg

-- | Log message with Warning level.
logWarning :: Show tag => tag -> T.Message -> Flow ()
logWarning tag msg = evalLogger' $ logMessage' T.Warning tag msg

runIO :: T.JSONEx a => IO a -> Flow a
runIO ioAct = liftFC $ RunIO ioAct id

getOption :: T.OptionEntity k v => k -> Flow (Maybe v)
getOption k = liftFC $ GetOption k id

setOption :: T.OptionEntity k v => k -> v -> Flow ()
setOption k v = liftFC $ SetOption k v id

generateGUID :: Flow Text
generateGUID = liftFC $ GenerateGUID id

runSysCmd :: String -> Flow String
runSysCmd cmd = liftFC $ RunSysCmd cmd id

initSqlDBConnection :: T.DBConfig beM -> Flow (T.DBResult (T.SqlConn beM))
initSqlDBConnection cfg = liftFC $ InitSqlDBConnection cfg id

deinitSqlDBConnection :: T.SqlConn beM -> Flow ()
deinitSqlDBConnection conn = liftFC $ DeInitSqlDBConnection conn id

runDB
  ::
    ( T.JSONEx a
    , T.BeamRunner beM
    , T.BeamRuntime be beM
    , B.FromBackendRow be a
    )
  => T.SqlConn beM
  -> L.SqlDB beM a
  -> Flow (T.DBResult a)
runDB conn dbAct = liftFC $ RunDB conn dbAct id



forkFlow :: T.JSONEx a => Text -> Flow a -> Flow ()
forkFlow description flow = do
  flowGUID <- generateGUID
  unless (null description) $ logInfo tag $ "Flow forked. Description: " <> description <> " GUID: " <> flowGUID
  when   (null description) $ logInfo tag $ "Flow forked. GUID: " <> flowGUID
  void $ liftFC $ Fork description flowGUID flow id
  where
    tag :: Text
    tag = "ForkFlow"

throwException :: forall a e. Exception e => e -> Flow a
throwException ex = liftFC $ ThrowException ex id

runKVDB
  :: KVDB a
  -> Flow (T.KVDBAnswer a)
runKVDB act = liftFC $ RunKVDB act id

-- TODO: port
-- callAPI
--   :: forall st rt a b
--    . RestEndpoint a b
--   => Headers -> a -> BackendFlow st rt (APIResult b)
-- callAPI headers a = wrap $ CallAPI
--   (apiInteract a headers)
--   (Playback.mkEntryDict
--     (encodeJSON $ makeRequest a headers)
--     (Playback.mkCallAPIEntry (\_ -> encode $ makeRequest a headers)))
--   id
