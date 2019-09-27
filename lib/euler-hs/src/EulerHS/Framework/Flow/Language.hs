{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

module EulerHS.Framework.Flow.Language where

import           EulerHS.Prelude

import           Servant.Client (ClientM, ClientError, BaseUrl)

import qualified EulerHS.Core.Types as T
import           EulerHS.Core.Language (Logger, SqlDB, logMessage', KVDB, KVDBAnswer)
import qualified EulerHS.Framework.Types as T
import qualified EulerHS.Core.KVDB.Language as KVDB

type Description = Text

type ForkGUID = Text

-- | Flow language.
data FlowMethod next where
  CallAPI :: T.RestEndpoint req resp => req -> (T.APIResult resp -> next) -> FlowMethod next

  CallServantAPI :: BaseUrl -> ClientM a -> (Either ClientError a -> next) -> FlowMethod next

  EvalLogger :: Logger a -> (a -> next) -> FlowMethod next

  RunIO :: (ToJSON s, FromJSON s) => IO s -> (s -> next) -> FlowMethod next

  GetOption :: T.OptionEntity k v => k -> (Maybe v -> next) -> FlowMethod next

  SetOption :: T.OptionEntity k v => k -> v -> (() -> next) -> FlowMethod next

  GenerateGUID :: (Text -> next) -> FlowMethod next

  RunSysCmd :: String -> (String -> next) -> FlowMethod next

  Fork :: Description -> ForkGUID -> Flow s -> (() -> next) -> FlowMethod next

  ThrowException ::forall a e next. Exception e => e -> (a -> next) -> FlowMethod next

  -- TODO: Disconnect :: _ -> FlowMethod next

  Connect :: T.DBConfig -> (T.DBResult T.SqlConn -> next) -> FlowMethod next

  RunDB
    :: T.SqlConn
    -> SqlDB T.DbBackend a
    -> (T.DBResult a -> next)
    -> FlowMethod next

  RunKVDBEither :: KVDB (KVDBAnswer a) -> (KVDBAnswer a -> next) -> FlowMethod next

instance Functor FlowMethod where
  fmap f (CallAPI req next) = CallAPI req (f . next)

  fmap f (CallServantAPI bUrl clientAct next) = CallServantAPI bUrl clientAct (f . next)

  fmap f (EvalLogger logAct next)             = EvalLogger logAct (f . next)

  fmap f (GenerateGUID next)                  = GenerateGUID (f . next)

  fmap f (RunSysCmd cmd next)                 = RunSysCmd cmd (f . next)

  fmap f (Fork desc guid fflow next)          = Fork desc guid fflow (f . next)

  fmap f (ThrowException message next)        = ThrowException message (f . next)

  fmap f (RunIO ioAct next)                   = RunIO ioAct (f . next)

  fmap f (GetOption k next)                   = GetOption k (f . next)

  fmap f (SetOption k v next)                 = SetOption k v (f . next)

  fmap f (Connect cfg next)                   = Connect cfg (f . next)

  fmap f (RunDB conn dbAct next)              = RunDB conn dbAct (f . next)

  fmap f (RunKVDBEither act next)             = RunKVDBEither act (f . next)

type Flow = F FlowMethod

callServantAPI :: BaseUrl -> ClientM a -> Flow (Either ClientError a)
callServantAPI url cl = liftFC $ CallServantAPI url cl id

evalLogger' :: Logger a -> Flow a
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

runIO :: (ToJSON s, FromJSON s) => IO s -> Flow s
runIO ioAct = liftFC $ RunIO ioAct id

getOption :: T.OptionEntity k v => k -> Flow (Maybe v)
getOption k = liftFC $ GetOption k id

setOption :: T.OptionEntity k v => k -> v -> Flow ()
setOption k v = liftFC $ SetOption k v id

generateGUID :: Flow Text
generateGUID = liftFC $ GenerateGUID id

runSysCmd :: String -> Flow String
runSysCmd cmd = liftFC $ RunSysCmd cmd id

connect :: T.DBConfig -> Flow (T.DBResult T.SqlConn)
connect cfg = liftFC $ Connect cfg id

runDB :: T.SqlConn -> SqlDB T.DbBackend a -> Flow (T.DBResult a)
runDB conn dbAct = liftFC $ RunDB conn dbAct id

forkFlow :: (ToJSON s, FromJSON s) => Text -> Flow s -> Flow ()
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

runKVDBEither :: KVDB (KVDBAnswer a) -> Flow (KVDBAnswer a)
runKVDBEither act = liftFC $ RunKVDBEither act id

setKV :: ByteString -> ByteString -> Flow (KVDBAnswer KVDB.Status)
setKV key value = runKVDBEither $ KVDB.set key value

getKV :: ByteString -> Flow (KVDBAnswer (Maybe ByteString))
getKV key = runKVDBEither $ KVDB.get key

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
