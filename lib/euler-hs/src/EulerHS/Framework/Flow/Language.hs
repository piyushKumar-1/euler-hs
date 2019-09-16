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

  EvalLogger :: Logger () -> (() -> next) -> FlowMethod next

instance Functor FlowMethod where
  fmap f (CallAPI req next) = CallAPI req (f . next)
  fmap f (CallServantAPI bUrl clientAct next) = CallServantAPI bUrl clientAct (f . next)

  fmap f (EvalLogger logAct next) = EvalLogger logAct (f . next)


type Flow = F FlowMethod

callServantAPI :: BaseUrl -> ClientM a -> Flow (Either ClientError a)
callServantAPI url cl = liftFC $ CallServantAPI url cl id

evalLogger' :: Logger () -> Flow ()
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
