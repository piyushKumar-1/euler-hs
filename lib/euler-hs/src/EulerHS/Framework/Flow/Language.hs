{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE AllowAmbiguousTypes   #-}

module EulerHS.Framework.Flow.Language
  (
  -- * Flow language
    Flow
  , FlowMethod(..)
  -- ** Methods
  -- *** SQLDB
  , initSqlDBConnection
  , deinitSqlDBConnection
  , getSqlDBConnection
  , runDB
  -- *** KVDB
  , initKVDBConnection
  , deinitKVDBConnection
  , getKVDBConnection
  , runKVDB
  -- *** Logging
  , logInfo
  , logError
  , logDebug
  , logWarning
    -- *** Typed options
  -- | Store given key/option pair in the storage
  -- getOption and setOption are provides interface for runtime typed key-value storage.
  -- You can create special types for some options and use it as keys for stored data.
  -- It's safer than using strings as keys, because the compiler will let you know if you made typo.
  , getOption
  , setOption
  , delOption
  -- *** Other
  , callServantAPI
  , callAPI
  , callAPI'
  , callHttpAPI
  , runIO
  , runIO'
  , runUntracedIO
  , generateGUID
  , runSysCmd
  , forkFlow
  , forkFlow'
  , await
  , throwException
  , runSafeFlow

  -- *** PublishSubscribe
  , publish
  , subscribe
  , psubscribe
  , unpackLanguagePubSub
  ) where

import           EulerHS.Prelude hiding (getOption)

import qualified Data.ByteString.Lazy as ByteString
import qualified Network.HTTP.Client  as HTTP
import           Servant.Client (ClientError, BaseUrl)
import qualified EulerHS.Core.Types as T
import           EulerHS.Core.Language (Logger, logMessage', KVDB)
import qualified EulerHS.Core.Language as L
import qualified EulerHS.Core.PubSub.Language as PSL

-- | Flow language.
data FlowMethod next where
  CallServantAPI
    :: T.JSONEx a
    => Maybe T.ManagerSelector
    -> BaseUrl
    -> T.EulerClient a
    -> (Either ClientError a -> next)
    -> FlowMethod next

  CallHttpAPI
    :: T.JSONEx a
    => Text
    -> (Either ClientError a -> next)
    -> FlowMethod next

  EvalLogger
    :: Logger a
    -> (a -> next)
    -> FlowMethod next

  RunIO
    :: T.JSONEx a
    => Text
    -> IO a
    -> (a -> next)
    -> FlowMethod next

  RunUntracedIO
    :: Text
    -> IO a
    -> (a -> next)
    -> FlowMethod next

  GetOption
    :: (ToJSON a, FromJSON a)
    => T.KVDBKey
    -> (Maybe a -> next)
    -> FlowMethod next

  SetOption
    :: (ToJSON a, FromJSON a)
    => T.KVDBKey
    -> a
    -> (() -> next)
    -> FlowMethod next

  DelOption
    :: T.KVDBKey
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
    :: (FromJSON a, ToJSON a)
    => T.Description
    -> T.ForkGUID
    -> Flow a
    -> (T.Awaitable (Either Text a) -> next)
    -> FlowMethod next

  Await
    :: (FromJSON a, ToJSON a)
    => Maybe T.Microseconds
    -> T.Awaitable (Either Text a)
    -> (Either T.AwaitingError a -> next)
    -> FlowMethod next

  ThrowException
    :: forall a e next
     . Exception e
    => e
    -> (a -> next)
    -> FlowMethod next

  RunSafeFlow
    :: (FromJSON a, ToJSON a)
    => T.SafeFlowGUID
    -> Flow a
    -> ((Either Text a) -> next)
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

  GetSqlDBConnection
    :: T.DBConfig beM
    -> (T.DBResult (T.SqlConn beM) -> next)
    -> FlowMethod next

  InitKVDBConnection
    :: T.KVDBConfig
    -> (T.KVDBAnswer T.KVDBConn -> next)
    -> FlowMethod next

  DeInitKVDBConnection
    :: T.KVDBConn
    -> (() -> next)
    -> FlowMethod next

  GetKVDBConnection
    :: T.KVDBConfig
    -> (T.KVDBAnswer T.KVDBConn -> next)
    -> FlowMethod next

  RunDB
    :: T.JSONEx a
    => T.SqlConn beM
    -> L.SqlDB beM a
    -> (T.DBResult a -> next)
    -> FlowMethod next

  RunKVDB
    :: Text
    -> KVDB a
    -> (T.KVDBAnswer a -> next)
    -> FlowMethod next

  RunPubSub
    :: PubSub a
    -> (a -> next)
    -> FlowMethod next

instance Functor FlowMethod where
  fmap f (CallServantAPI mngSlc bUrl clientAct next) = CallServantAPI mngSlc bUrl clientAct (f . next)

  fmap f (CallHttpAPI url next)               = CallHttpAPI url (f . next)

  fmap f (EvalLogger logAct next)             = EvalLogger logAct (f . next)

  fmap f (GenerateGUID next)                  = GenerateGUID (f . next)

  fmap f (RunSysCmd cmd next)                 = RunSysCmd cmd (f . next)

  fmap f (Fork desc guid fflow next)          = Fork desc guid fflow (f . next)

  fmap f (Await timeout avaitable next)       = Await timeout avaitable (f . next)

  fmap f (ThrowException message next)        = ThrowException message (f . next)

  fmap f (RunSafeFlow guid flow next)         = RunSafeFlow guid flow (f . next)

  fmap f (RunIO descr ioAct next)             = RunIO descr ioAct (f . next)

  fmap f (RunUntracedIO descr ioAct next)     = RunUntracedIO descr ioAct (f . next)

  fmap f (GetOption k next)                   = GetOption k (f . next)

  fmap f (SetOption k v next)                 = SetOption k v (f . next)

  fmap f (DelOption k next)                   = DelOption k (f . next)

  fmap f (InitSqlDBConnection cfg next)       = InitSqlDBConnection cfg (f . next)

  fmap f (DeInitSqlDBConnection conn next)    = DeInitSqlDBConnection conn (f.next)

  fmap f (GetSqlDBConnection cfg next)        = GetSqlDBConnection cfg (f . next)

  fmap f (InitKVDBConnection cfg next)        = InitKVDBConnection cfg (f . next)

  fmap f (DeInitKVDBConnection conn next)     = DeInitKVDBConnection conn (f.next)

  fmap f (GetKVDBConnection cfg next)         = GetKVDBConnection cfg (f . next)

  fmap f (RunDB conn sqlDbAct next)           = RunDB conn sqlDbAct (f . next)

  fmap f (RunKVDB cName act next)             = RunKVDB cName act (f . next)

  fmap f (RunPubSub act next)                 = RunPubSub act (f . next)

type Flow = F FlowMethod


-- | Method for calling external HTTP APIs using the facilities of servant-client.
-- Allows to specify what manager should be used. If no manager found,
-- `HttpManagerNotFound` will be returne (as part of `ClientError.ConnectionError`).
--
-- Thread safe, exception free.
--
-- Takes remote url, servant client for this endpoint
-- and returns either client error or result.
--
-- > data User = User { firstName :: String, lastName :: String , userGUID :: String}
-- >   deriving (Generic, Show, Eq, ToJSON, FromJSON )
-- >
-- > data Book = Book { author :: String, name :: String }
-- >   deriving (Generic, Show, Eq, ToJSON, FromJSON )
-- >
-- > type API = "user" :> Get '[JSON] User
-- >       :<|> "book" :> Get '[JSON] Book
-- >
-- > api :: Proxy API
-- > api = Proxy
-- >
-- > getUser :: EulerClient User
-- > getBook :: EulerClient Book
-- > (getUser :<|> getBook) = client api
-- >
-- > url = BaseUrl Http "localhost" port ""
-- >
-- >
-- > myFlow = do
-- >   book <- callServantAPI url getBook
-- >   user <- callServantAPI url getUser

callServantAPI
  :: T.JSONEx a
  => Maybe T.ManagerSelector     -- ^ name of the connection manager to be used
  -> BaseUrl                     -- ^ remote url 'BaseUrl'
  -> T.EulerClient a             -- ^ servant client 'EulerClient'
  -> Flow (Either ClientError a) -- ^ result
callServantAPI mbMgrSel url cl = liftFC $ CallServantAPI mbMgrSel url cl id


-- | Method for calling external HTTP APIs without bothering with types.
--
-- Thread safe, exception free.
--
-- Takes remote url and returns either client error or result.
--
-- > myFlow = do
-- >   book <- callHttpAPI url

callHttpAPI
  :: T.JSONEx a
  => T.Request                             -- ^ remote url 'Text'
  -> Flow (Either ClientError T.Response)  -- ^ result
callHttpAPI url = liftFC $ CallHttpAPI url id


-- | Method for calling external HTTP APIs using the facilities of servant-client.
-- Allows to specify what manager should be used. If no manager found,
-- `HttpManagerNotFound` will be returne (as part of `ClientError.ConnectionError`).
--
-- Thread safe, exception free.
--
-- Alias for callServantAPI.
--
-- | Takes remote url, servant client for this endpoint
-- and returns either client error or result.
--
-- > data User = User { firstName :: String, lastName :: String , userGUID :: String}
-- >   deriving (Generic, Show, Eq, ToJSON, FromJSON )
-- >
-- > data Book = Book { author :: String, name :: String }
-- >   deriving (Generic, Show, Eq, ToJSON, FromJSON )
-- >
-- > type API = "user" :> Get '[JSON] User
-- >       :<|> "book" :> Get '[JSON] Book
-- >
-- > api :: Proxy API
-- > api = Proxy
-- >
-- > getUser :: EulerClient User
-- > getBook :: EulerClient Book
-- > (getUser :<|> getBook) = client api
-- >
-- > url = BaseUrl Http "localhost" port ""
-- >
-- >
-- > myFlow = do
-- >   book <- callAPI url getBook
-- >   user <- callAPI url getUser

callAPI' :: T.JSONEx a => Maybe T.ManagerSelector -> BaseUrl -> T.EulerClient a -> Flow (Either ClientError a)
callAPI' = callServantAPI

-- | The same as `callAPI'` but with default manager to be used.
callAPI :: T.JSONEx a => BaseUrl -> T.EulerClient a -> Flow (Either ClientError a)
callAPI = callServantAPI Nothing

-- | Evaluates a logging action.
evalLogger' :: (ToJSON a, FromJSON a) => Logger a -> Flow a
evalLogger' logAct = liftFC $ EvalLogger logAct id

-- | Log message with Info level.
--
-- Thread safe.
logInfo :: Show tag => tag -> T.Message -> Flow ()
logInfo tag msg = evalLogger' $ logMessage' T.Info tag msg

-- | Log message with Error level.
--
-- Thread safe.
logError :: Show tag => tag -> T.Message -> Flow ()
logError tag msg = evalLogger' $ logMessage' T.Error tag msg

-- | Log message with Debug level.
--
-- Thread safe.
logDebug :: Show tag => tag -> T.Message -> Flow ()
logDebug tag msg = evalLogger' $ logMessage' T.Debug tag msg

-- | Log message with Warning level.
--
-- Thread safe.
logWarning :: Show tag => tag -> T.Message -> Flow ()
logWarning tag msg = evalLogger' $ logMessage' T.Warning tag msg

-- | Run some IO operation, result should have 'ToJSONEx' instance (extended 'ToJSON'),
-- because we have to collect it in recordings for ART system.
--
-- Warning. This method is dangerous and should be used wisely.
--
-- > myFlow = do
-- >   content <- runIO $ readFromFile file
-- >   logDebugT "content id" $ extractContentId content
-- >   pure content
runIO :: T.JSONEx a => IO a -> Flow a
runIO = runIO' ""

-- | The same as runIO, but accepts a description which will be written into the ART recordings
-- for better clarity.
--
-- Warning. This method is dangerous and should be used wisely.
--
-- > myFlow = do
-- >   content <- runIO' "reading from file" $ readFromFile file
-- >   logDebugT "content id" $ extractContentId content
-- >   pure content
runIO' :: T.JSONEx a => Text -> IO a -> Flow a
runIO' descr ioAct = liftFC $ RunIO descr ioAct id

-- | The same as runIO, but do not record IO outputs in the ART recordings.
--   For example, this can be useful to implement things like STM or use mutable
--   state.
--
-- Warning. This method is dangerous and should be used wisely.
--
-- > myFlow = do
-- >   content <- runUntracedIO $ readFromFile file
-- >   logDebugT "content id" $ extractContentId content
-- >   pure content
runUntracedIO :: IO a -> Flow a
runUntracedIO = runUntracedIO' ""

-- | The same as runUntracedIO, but accepts a description which will be written into
-- the ART recordings for better clarity.
--
-- Warning. This method is dangerous and should be used wisely.
--
-- > myFlow = do
-- >   content <- runUntracedIO' "reading secret data" $ readFromFile secret_file
-- >   logDebugT "content id" $ extractContentId content
-- >   pure content
runUntracedIO' :: Text -> IO a -> Flow a
runUntracedIO' descr ioAct = liftFC $ RunUntracedIO descr ioAct id

-- | Gets stored a typed option by a typed key.
--
-- Thread safe, exception free.
getOption :: forall k v. T.OptionEntity k v => k -> Flow (Maybe v)
getOption k = liftFC $ GetOption (T.mkOptionKey @k @v k) id

-- Sets a typed option using a typed key (a mutable destructive operation)
--
-- Be aware that it's possible to overflow the runtime with options
-- created uncontrollably.
--
-- Also please keep in mind the options are runtime-bound and if you have
-- several API methods working with the same option key, you'll get a race.
--
-- Thread safe, exception free.
--
-- >  data MerchantIdKey = MerchantIdKey
-- >
-- >  instance OptionEntity MerchantIdKey Text
-- >
-- >  myFlow = do
-- >    _ <- setOption MerchantIdKey "abc1234567"
-- >    mKey <- getOption MerchantIdKey
-- >    runIO $ putTextLn mKey
-- >    delOption MerchantIdKey
setOption :: forall k v. T.OptionEntity k v => k -> v -> Flow ()
setOption k v = liftFC $ SetOption (T.mkOptionKey @k @v k) v id

-- Deletes a typed option using a typed key.
--

delOption :: forall k v. T.OptionEntity k v => k -> Flow ()
delOption k = liftFC $ DelOption (T.mkOptionKey @k @v k) id


-- | Generate a version 4 UUIDs as specified in RFC 4122
-- e.g. 25A8FC2A-98F2-4B86-98F6-84324AF28611.
--
-- Thread safe, exception free.
generateGUID :: Flow Text
generateGUID = liftFC $ GenerateGUID id

-- | Runs system command and returns its output.
--
-- Warning. This method is dangerous and should be used wisely.
--
-- > myFlow = do
-- >   currentDir <- runSysCmd "pwd"
-- >   logInfoT "currentDir" $ toText currentDir
-- >   ...
runSysCmd :: String -> Flow String
runSysCmd cmd = liftFC $ RunSysCmd cmd id

-- | Inits an SQL connection using a config.
--
-- Returns an error (Left $ T.DBError T.ConnectionAlreadyExists msg)
-- if the connection already exists for this config.
--
-- Thread safe, exception free.
initSqlDBConnection :: T.DBConfig beM -> Flow (T.DBResult (T.SqlConn beM))
initSqlDBConnection cfg = liftFC $ InitSqlDBConnection cfg id

-- | Deinits an SQL connection.
-- Does nothing if the connection is not found (might have been closed earlier).
--
-- Thread safe, exception free.
deinitSqlDBConnection :: T.SqlConn beM -> Flow ()
deinitSqlDBConnection conn = liftFC $ DeInitSqlDBConnection conn id

-- | Gets the existing connection.
--
-- Returns an error (Left $ T.DBError T.ConnectionDoesNotExist)
-- if the connection does not exist.
--
-- Thread safe, exception free.
getSqlDBConnection ::T.DBConfig beM -> Flow (T.DBResult (T.SqlConn beM))
getSqlDBConnection cfg = liftFC $ GetSqlDBConnection cfg id

-- | Inits a KV DB connection using a config.
--
-- Returns an error (Left $ KVDBError KVDBConnectionAlreadyExists msg)
-- if the connection already exists.
--
-- Thread safe, exception free.
initKVDBConnection :: T.KVDBConfig -> Flow (T.KVDBAnswer T.KVDBConn)
initKVDBConnection cfg = liftFC $ InitKVDBConnection cfg id

-- | Deinits the given KV DB connection.
-- Does nothing if the connection is not found (might have been closed earlier).
--
-- Thread safe, exception free.
deinitKVDBConnection :: T.KVDBConn  -> Flow ()
deinitKVDBConnection conn = liftFC $ DeInitKVDBConnection conn id

-- | Get the existing connection.

-- Returns an error (Left $ KVDBError KVDBConnectionDoesNotExist)
-- if the connection does not exits for this config.
--
-- Thread safe, exception free.
getKVDBConnection :: T.KVDBConfig -> Flow (T.KVDBAnswer T.KVDBConn)
getKVDBConnection cfg = liftFC $ GetKVDBConnection cfg id

-- | Evaluates SQL DB operations transactionally.
-- It's possible to have a chain of SQL DB calls (within the SqlDB language).
-- These chains will be executed as a single transaction.
--
-- Thread safe, exception free.
--
-- The underlying library is beam which allows to access 3 different SQL backends.
-- See TUTORIAL.md, README.md and QueryExamplesSpec.hs for more info.
--
-- > myFlow :: L.Flow (T.DBResult (Maybe User))
-- > myFlow = do
-- >   connection <- L.initSqlDBConnection postgresCfg
-- >
-- >   res <- L.runDB connection $ do
-- >     let predicate1 User {..} = _userFirstName ==. B.val_ "John"
-- >
-- >     L.updateRows $ B.update (_users eulerDb)
-- >       (\User {..} -> mconcat
-- >         [ _userFirstName <-. B.val_ "Leo"
-- >         , _userLastName  <-. B.val_ "San"
-- >         ]
-- >       )
-- >       predicate1
-- >
-- >     let predicate2 User {..} = _userFirstName ==. B.val_ "Leo"
-- >     L.findRow
-- >       $ B.select
-- >       $ B.limit_ 1
-- >       $ B.filter_ predicate2
-- >       $ B.all_ (_users eulerDb)
-- >
-- >   L.deinitSqlDBConnection connection
-- >   pure res
runDB
  ::
    ( T.JSONEx a
    , T.BeamRunner beM
    , T.BeamRuntime be beM
    )
  => T.SqlConn beM
  -> L.SqlDB beM a
  -> Flow (T.DBResult a)
runDB conn dbAct = liftFC $ RunDB conn dbAct id

-- | Fork a flow.
--
-- Warning. With forked flows, race coniditions and dead / live blocking become possible.
-- All the rules applied to forked threads in Haskell can be applied to forked flows.
--
-- Generally, the method is thread safe. Doesn't do anything to bookkeep the threads.
-- There is no possibility to kill a thread on the moment.
--
-- Thread safe, exception free.
--
-- > myFlow1 = do
-- >   logInfoT "myflow1" "logFromMyFlow1"
-- >   someAction
-- >
-- > myFlow2 = do
-- >   res <- runIO someAction
-- >   forkFlow "myFlow1 fork" myFlow1
-- >   pure res
forkFlow :: (FromJSON a, ToJSON a) => T.Description -> Flow a -> Flow ()
forkFlow description flow = void $ forkFlow' description flow

-- | Same as fork a flow but returns an Awaitable value which can be used
-- to await for the results from the flow.
--
-- > myFlow1 = do
-- >   logInfoT "myflow1" "logFromMyFlow1"
-- >   pure 10
-- >
-- > myFlow2 = do
-- >   awaitable <- forkFlow' "myFlow1 fork" myFlow1
-- >   await Nothing awaitable
forkFlow' :: (FromJSON a, ToJSON a) => T.Description -> Flow a -> Flow (T.Awaitable (Either Text a))
forkFlow' description flow = do
  flowGUID <- generateGUID
  unless (null description) $ logInfo tag $ "Flow forked. Description: " <> description <> " GUID: " <> flowGUID
  when   (null description) $ logInfo tag $ "Flow forked. GUID: " <> flowGUID
  liftFC $ Fork description flowGUID flow id
  where
    tag :: Text
    tag = "ForkFlow"

-- | Await for some a result from the flow.
-- If the timeout is Nothing than the operation is blocking.
-- If the timeout is set then the internal mechanism tries to do several (10) checks for the result.
-- Can return earlier if the result became available.
-- Returns either an Awaiting error or a result.
--
-- Warning. There are no guarantees of a proper thread delaying here.
--
-- Thread safe, exception free.
--
-- | mbMcs == Nothing: infinite awaiting.
-- | mbMcs == Just (Microseconds n): await for approximately n seconds.
--     Awaiting may succeed ealier.
--
-- > myFlow1 = do
-- >   logInfoT "myflow1" "logFromMyFlow1"
-- >   pure 10
-- >
-- > myFlow2 = do
-- >   awaitable <- forkFlow' "myFlow1 fork" myFlow1
-- >   await Nothing awaitable
await
  :: (FromJSON a, ToJSON a)
  => Maybe T.Microseconds
  -> T.Awaitable (Either Text a)
  -> Flow (Either T.AwaitingError a)
await mbMcs awaitable = liftFC $ Await mbMcs awaitable id

-- | Throw a given exception.
--
-- It's possible to catch this exception using runSafeFlow method.
--
-- Thread safe. Exception throwing.
--
-- > myFlow = do
-- >   res <- authAction
-- >   case res of
-- >     Failure reason -> throwException err403 {errBody = reason}
-- >     Success -> ...
throwException :: forall a e. Exception e => e -> Flow a
throwException ex = liftFC $ ThrowException ex id

-- | Run a flow safely with catching all the exceptions from it.
-- Returns either a result or the exception turned into a text message.
--
-- This includes ususal instances of the Exception type class,
-- `error` exception and custom user exceptions thrown by the `throwException` method.
--
-- Thread safe, exception free.
--
-- > myFlow = runSafeFlow $ throwException err403 {errBody = reason}
--
-- > myFlow = do
-- >   eitherContent <- runSafeFlow $ runIO $ readFromFile file
-- >   case eitherContent of
-- >     Left err -> ...
-- >     Right content -> ...
runSafeFlow :: (FromJSON a, ToJSON a) => Flow a -> Flow (Either Text a)
runSafeFlow flow = do
  safeFlowGUID <- generateGUID
  liftFC $ RunSafeFlow safeFlowGUID flow id

-- | Execute kvdb actions.
--
-- Thread safe, exception free.
--
-- > myFlow = do
-- >   kvres <- L.runKVDB $ do
-- >     set "aaa" "bbb"
-- >     res <- get "aaa"
-- >     del ["aaa"]
-- >     pure res
runKVDB
  :: Text
  -> KVDB a -- ^ KVDB action
  -> Flow (T.KVDBAnswer a)
runKVDB cName act = liftFC $ RunKVDB cName act id

---- Experimental Pub Sub implementation using Redis Pub Sub.

newtype PubSub a = PubSub { unpackLanguagePubSub :: (forall b . Flow b -> IO b) -> PSL.PubSub a }

type MessageCallback
    =  ByteString  -- ^ Message payload
    -> Flow ()

type PMessageCallback
    =  ByteString  -- ^ Channel name
    -> ByteString  -- ^ Message payload
    -> Flow ()

runPubSub
  :: PubSub a
  -> Flow a
runPubSub act = liftFC $ RunPubSub act id

-- | Publish payload to channel.
publish
  :: PSL.Channel                        -- ^ Channel in which payload will be send
  -> PSL.Payload                        -- ^ Payload
  -> Flow (Either T.KVDBReply Integer)  -- ^ Number of subscribers received payload
publish channel payload = runPubSub $ PubSub $ const $ PSL.publish channel payload


-- | Subscribe to all channels from list.
-- Note: Subscription won't be unsubscribed automatically on thread end.
-- Use canceller explicitly to cancel subscription
subscribe
  :: [PSL.Channel]    -- ^ List of channels to subscribe
  -> MessageCallback  -- ^ Callback function.
  -> Flow (Flow ())   -- ^ Inner flow is a canceller of current subscription
subscribe channels cb = fmap (runIO' "subscribe") $
  runPubSub $ PubSub $ \runFlow -> PSL.subscribe channels (runFlow . cb)


-- | Subscribe to all channels from list. Respects redis pattern syntax.
-- Note: Subscription won't be unsubscribed automatically on thread end.
-- Use canceller explicitly to cancel subscription
psubscribe
  :: [PSL.ChannelPattern] -- ^ List of channels to subscribe (wit respect to patterns supported by redis)
  -> PMessageCallback     -- ^ Callback function
  -> Flow (Flow ())       -- ^ Inner flow is a canceller of current subscription
psubscribe channels cb = fmap (runIO' "psubscribe") $
  runPubSub $ PubSub $ \runFlow -> PSL.psubscribe channels (\ch -> runFlow . cb ch)
