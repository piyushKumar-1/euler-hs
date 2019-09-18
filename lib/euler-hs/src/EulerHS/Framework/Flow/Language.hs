{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

module EulerHS.Framework.Flow.Language where

import           EulerHS.Prelude

import           Servant.Client (ClientM, ClientError, BaseUrl)

import qualified EulerHS.Core.Types as T
import           EulerHS.Core.Language (Logger, logMessage')
import qualified EulerHS.Framework.Types as T

-- | Flow language.
data FlowMethod next where
  CallAPI :: T.RestEndpoint req resp => req -> (T.APIResult resp -> next) -> FlowMethod next
  CallServantAPI :: BaseUrl -> ClientM a -> (Either ClientError a -> next) -> FlowMethod next

  EvalLogger :: Logger a -> (a -> next) -> FlowMethod next

  RunIO :: (ToJSON s, FromJSON s) => IO s -> (s -> next) -> FlowMethod next

  GetOption :: T.OptionEntity k v => k -> (Maybe v -> next) -> FlowMethod next
  SetOption :: T.OptionEntity k v => k -> v -> (() -> next) -> FlowMethod next

instance Functor FlowMethod where
  fmap f (CallAPI req next) = CallAPI req (f . next)
  fmap f (CallServantAPI bUrl clientAct next) = CallServantAPI bUrl clientAct (f . next)

  fmap f (EvalLogger logAct next) = EvalLogger logAct (f . next)

  fmap f (RunIO ioAct next)                   = RunIO ioAct (f . next)

  fmap f (GetOption k next)                   = GetOption k (f.next)
  fmap f (SetOption k v next)                 = SetOption k v (f.next)


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
