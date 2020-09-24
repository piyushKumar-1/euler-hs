{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module EulerHS.Framework.Flow.Language
  (
  -- * Flow language
    Flow
  , FlowMethod(..)
  , MonadFlow(..)
  , ReaderFlow
  -- ** Methods
  -- *** SQLDB
  , initSqlDBConnection
  , deinitSqlDBConnection
  , getSqlDBConnection
  , runDB
  , runTransaction
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
  , callHTTP
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

import           Control.Monad.Trans.RWS.Strict (RWST)
import           Control.Monad.Trans.Writer (WriterT)
import qualified Data.Text as Text
import           EulerHS.Core.Language (KVDB, Logger, logMessage')
import qualified EulerHS.Core.Language as L
import qualified EulerHS.Core.PubSub.Language as PSL
import qualified EulerHS.Core.Types as T
import           EulerHS.Prelude hiding (getOption)
import           Servant.Client (BaseUrl, ClientError)

-- | Flow language.
data FlowMethod next where
  CallServantAPI
    :: (HasCallStack, T.JSONEx a)
    => Maybe T.ManagerSelector
    -> BaseUrl
    -> T.EulerClient a
    -> (Either ClientError a -> next)
    -> FlowMethod next

  CallHTTP
    :: HasCallStack
    => T.HTTPRequest
    -> Maybe T.HTTPCert
    -> (Either Text T.HTTPResponse -> next)
    -> FlowMethod next

  EvalLogger
    :: HasCallStack
    => Logger a
    -> (a -> next)
    -> FlowMethod next

  RunIO
    :: (HasCallStack, T.JSONEx a)
    => Text
    -> IO a
    -> (a -> next)
    -> FlowMethod next

  RunUntracedIO
    :: HasCallStack
    => Text
    -> IO a
    -> (a -> next)
    -> FlowMethod next

  GetOption
    :: (HasCallStack, ToJSON a, FromJSON a)
    => T.KVDBKey
    -> (Maybe a -> next)
    -> FlowMethod next

  SetOption
    :: (HasCallStack, ToJSON a, FromJSON a)
    => T.KVDBKey
    -> a
    -> (() -> next)
    -> FlowMethod next

  DelOption
    :: HasCallStack
    => T.KVDBKey
    -> (() -> next)
    -> FlowMethod next

  GenerateGUID
    ::  HasCallStack
    => (Text -> next)
    -> FlowMethod next

  RunSysCmd
    :: HasCallStack
    => String
    -> (String -> next)
    -> FlowMethod next

  Fork
    :: (HasCallStack, FromJSON a, ToJSON a)
    => T.Description
    -> T.ForkGUID
    -> Flow a
    -> (T.Awaitable (Either Text a) -> next)
    -> FlowMethod next

  Await
    :: (HasCallStack, FromJSON a, ToJSON a)
    => Maybe T.Microseconds
    -> T.Awaitable (Either Text a)
    -> (Either T.AwaitingError a -> next)
    -> FlowMethod next

  ThrowException
    :: forall a e next
     . (HasCallStack, Exception e)
    => e
    -> (a -> next)
    -> FlowMethod next

  RunSafeFlow
    :: (HasCallStack, FromJSON a, ToJSON a)
    => T.SafeFlowGUID
    -> Flow a
    -> (Either Text a -> next)
    -> FlowMethod next

  -- TODO: DeInitSqlDBConnection :: _ -> FlowMethod next

  InitSqlDBConnection
    :: HasCallStack
    => T.DBConfig beM
    -> (T.DBResult (T.SqlConn beM) -> next)
    -> FlowMethod next

  DeInitSqlDBConnection
    :: HasCallStack
    => T.SqlConn beM
    -> (() -> next)
    -> FlowMethod next

  GetSqlDBConnection
    :: HasCallStack
    => T.DBConfig beM
    -> (T.DBResult (T.SqlConn beM) -> next)
    -> FlowMethod next

  InitKVDBConnection
    :: HasCallStack
    => T.KVDBConfig
    -> (T.KVDBAnswer T.KVDBConn -> next)
    -> FlowMethod next

  DeInitKVDBConnection
    :: HasCallStack
    => T.KVDBConn
    -> (() -> next)
    -> FlowMethod next

  GetKVDBConnection
    :: HasCallStack
    => T.KVDBConfig
    -> (T.KVDBAnswer T.KVDBConn -> next)
    -> FlowMethod next

  RunDB
    :: (HasCallStack, T.JSONEx a)
    => T.SqlConn beM
    -> L.SqlDB beM a
    -> Bool
    -> (T.DBResult a -> next)
    -> FlowMethod next

  RunKVDB
    :: HasCallStack
    => Text
    -> KVDB a
    -> (T.KVDBAnswer a -> next)
    -> FlowMethod next

  RunPubSub
    :: HasCallStack
    => PubSub a
    -> (a -> next)
    -> FlowMethod next

instance Functor FlowMethod where
  fmap f (CallServantAPI mngSlc bUrl clientAct next) = CallServantAPI mngSlc bUrl clientAct (f . next)

  fmap f (CallHTTP req cert next)               = CallHTTP req cert (f . next)

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

  fmap f (RunDB conn sqlDbAct runInTransaction next) = RunDB conn sqlDbAct runInTransaction (f . next)

  fmap f (RunKVDB cName act next)             = RunKVDB cName act (f . next)

  fmap f (RunPubSub act next)                 = RunPubSub act (f . next)

type Flow = F FlowMethod

type ReaderFlow r = ReaderT r Flow

newtype PubSub a = PubSub { unpackLanguagePubSub :: HasCallStack => (forall b . Flow b -> IO b) -> PSL.PubSub a }

type MessageCallback
    =  ByteString  -- ^ Message payload
    -> Flow ()

type PMessageCallback
    =  ByteString  -- ^ Channel name
    -> ByteString  -- ^ Message payload
    -> Flow ()

-- instance Monad.MonadThrow Flow where
--   throwM = throwException

-- | Fork a unit-returning flow.
--
-- __Note__: to fork a flow which yields a value use 'forkFlow\'' instead.
--
-- __Warning__: With forked flows, race coniditions and dead / live blocking become possible.
-- All the rules applied to forked threads in Haskell can be applied to forked flows.
--
-- Generally, the method is thread safe. Doesn't do anything to bookkeep the threads.
-- There is no possibility to kill a thread at the moment.
--
-- Thread safe, exception free.
--
-- > myFlow1 = do
-- >   logInfoT "myflow1" "logFromMyFlow1"
-- >   someAction
-- >
-- > myFlow2 = do
-- >   _ <- runIO someAction
-- >   forkFlow "myFlow1 fork" myFlow1
-- >   pure ()
--
forkFlow :: HasCallStack => T.Description -> Flow () -> Flow ()
forkFlow description flow = void $ forkFlow' description $ do
  eitherResult <- runSafeFlow flow
  case eitherResult of
    Left msg -> logError "forkFlow" msg
    Right x  -> pure ()

-- | Same as 'forkFlow', but takes @Flow a@ and returns an 'T.Awaitable' which can be used
-- to reap results from the flow being forked.
--
-- > myFlow1 = do
-- >   logInfoT "myflow1" "logFromMyFlow1"
-- >   pure 10
-- >
-- > myFlow2 = do
-- >   awaitable <- forkFlow' "myFlow1 fork" myFlow1
-- >   await Nothing awaitable
--
forkFlow' :: (HasCallStack, FromJSON a, ToJSON a) => T.Description -> Flow a -> Flow (T.Awaitable (Either Text a))
forkFlow' description flow = do
    flowGUID <- generateGUID
    unless (null description) $ logInfo tag $ "Flow forked. Description: " <> description <> " GUID: " <> flowGUID
    when   (null description) $ logInfo tag $ "Flow forked. GUID: " <> flowGUID
    liftFC $ Fork description flowGUID flow id
    where
      tag :: HasCallStack => Text
      tag = "ForkFlow"


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
-- > api :: HasCallStack => Proxy API
-- > api = Proxy
-- >
-- > getUser :: HasCallStack => EulerClient User
-- > getBook :: HasCallStack => EulerClient Book
-- > (getUser :<|> getBook) = client api
-- >
-- > url = BaseUrl Http "localhost" port ""
-- >
-- >
-- > myFlow = do
-- >   book <- callAPI url getBook
-- >   user <- callAPI url getUser
callAPI' :: (HasCallStack, T.JSONEx a, MonadFlow m) => Maybe T.ManagerSelector -> BaseUrl -> T.EulerClient a -> m (Either ClientError a)
callAPI' = callServantAPI

-- | The same as `callAPI'` but with default manager to be used.
callAPI :: (HasCallStack, T.JSONEx a, MonadFlow m) => BaseUrl -> T.EulerClient a -> m (Either ClientError a)
callAPI = callServantAPI Nothing

-- | Log message with Info level.
--
-- Thread safe
logInfo :: forall tag m . (HasCallStack, MonadFlow m, Show tag) => tag -> T.Message -> m ()
logInfo tag msg = evalLogger' $ logMessage' T.Info tag msg

-- | Log message with Error level.
--
-- Thread safe.
logError :: forall tag m . (HasCallStack, MonadFlow m, Show tag) => tag -> T.Message -> m ()
logError tag msg = do
  runIO L.logCallStack  -- it will be multiline, will that be a problem?
  evalLogger' $ logMessage' T.Error tag msg

-- | Log message with Debug level.
--
-- Thread safe.
logDebug :: forall tag m . (HasCallStack, MonadFlow m, Show tag) => tag -> T.Message -> m ()
logDebug tag msg = evalLogger' $ logMessage' T.Debug tag msg

-- | Log message with Warning level.
--
-- Thread safe.
logWarning :: forall tag m . (HasCallStack, MonadFlow m, Show tag) => tag -> T.Message -> m ()
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
runIO :: (HasCallStack, MonadFlow m, T.JSONEx a, HasCallStack) => IO a -> m a
runIO = runIO' ""

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
runUntracedIO :: (HasCallStack, MonadFlow m) => IO a -> m a
runUntracedIO = runUntracedIO' ""

-- | The same as callHTTPWithCert but does not need certificate data.
--
-- Thread safe, exception free.
--
-- Takes remote url and returns either client error or result.
--
-- > myFlow = do
-- >   book <- callHTTP url
callHTTP :: (HasCallStack, MonadFlow m) => T.HTTPRequest -> m (Either Text.Text T.HTTPResponse)
callHTTP url = callHTTPWithCert url Nothing


-- | MonadFlow implementation for the `Flow` Monad. This allows implementation of MonadFlow for
-- `ReaderT` and other monad transformers.
--
-- Omit `forkFlow` as this will break some monads like StateT (you can lift this manually if you
-- know what you're doing)
class Monad m => MonadFlow m where

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
  -- > api :: HasCallStack => Proxy API
  -- > api = Proxy
  -- >
  -- > getUser :: HasCallStack => EulerClient User
  -- > getBook :: HasCallStack => EulerClient Book
  -- > (getUser :<|> getBook) = client api
  -- >
  -- > url = BaseUrl Http "localhost" port ""
  -- >
  -- >
  -- > myFlow = do
  -- >   book <- callServantAPI url getBook
  -- >   user <- callServantAPI url getUser
  callServantAPI
    :: (HasCallStack, T.JSONEx a)
    => Maybe T.ManagerSelector     -- ^ name of the connection manager to be used
    -> BaseUrl                     -- ^ remote url 'BaseUrl'
    -> T.EulerClient a             -- ^ servant client 'EulerClient'
    -> m (Either ClientError a) -- ^ result

  -- | Method for calling external HTTP APIs without bothering with types.
  --
  -- Thread safe, exception free.
  --
  -- Takes remote url, optional certificate data and returns either client error or result.
  --
  -- > myFlow = do
  -- >   book <- callHTTPWithCert url cert
  callHTTPWithCert
    :: HasCallStack
    => T.HTTPRequest                        -- ^ remote url 'Text'
    -> Maybe T.HTTPCert                     -- ^ TLS certificate data
    -> m (Either Text.Text T.HTTPResponse)  -- ^ result

  -- | Evaluates a logging action.
  evalLogger' :: (HasCallStack, ToJSON a, FromJSON a) => Logger a -> m a

  -- | The same as runIO, but accepts a description which will be written into the ART recordings
  -- for better clarity.
  --
  -- Warning. This method is dangerous and should be used wisely.
  --
  -- > myFlow = do
  -- >   content <- runIO' "reading from file" $ readFromFile file
  -- >   logDebugT "content id" $ extractContentId content
  -- >   pure content
  runIO' :: (HasCallStack, T.JSONEx a) => Text -> IO a -> m a

  -- | The same as runUntracedIO, but accepts a description which will be written into
  -- the ART recordings for better clarity.
  --
  -- Warning. This method is dangerous and should be used wisely.
  --
  -- > myFlow = do
  -- >   content <- runUntracedIO' "reading secret data" $ readFromFile secret_file
  -- >   logDebugT "content id" $ extractContentId content
  -- >   pure content
  runUntracedIO' :: HasCallStack => Text -> IO a -> m a

  -- | Gets stored a typed option by a typed key.
  --
  -- Thread safe, exception free.
  getOption :: forall k v. (HasCallStack, T.OptionEntity k v) => k -> m (Maybe v)

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
  setOption :: forall k v. (HasCallStack, T.OptionEntity k v) => k -> v -> m ()

  -- Deletes a typed option using a typed key.
  --

  delOption :: forall k v. (HasCallStack, T.OptionEntity k v) => k -> m ()


  -- | Generate a version 4 UUIDs as specified in RFC 4122
  -- e.g. 25A8FC2A-98F2-4B86-98F6-84324AF28611.
  --
  -- Thread safe, exception free.
  generateGUID :: HasCallStack => m Text

  -- | Runs system command and returns its output.
  --
  -- Warning. This method is dangerous and should be used wisely.
  --
  -- > myFlow = do
  -- >   currentDir <- runSysCmd "pwd"
  -- >   logInfoT "currentDir" $ toText currentDir
  -- >   ...
  runSysCmd :: HasCallStack => String -> m String

  -- | Inits an SQL connection using a config.
  --
  -- Returns an error (Left $ T.DBError T.ConnectionAlreadyExists msg)
  -- if the connection already exists for this config.
  --
  -- Thread safe, exception free.
  initSqlDBConnection :: HasCallStack => T.DBConfig beM -> m (T.DBResult (T.SqlConn beM))

  -- | Deinits an SQL connection.
  -- Does nothing if the connection is not found (might have been closed earlier).
  --
  -- Thread safe, exception free.
  deinitSqlDBConnection :: HasCallStack => T.SqlConn beM -> m ()

  -- | Gets the existing connection.
  --
  -- Returns an error (Left $ T.DBError T.ConnectionDoesNotExist)
  -- if the connection does not exist.
  --
  -- Thread safe, exception free.
  getSqlDBConnection :: HasCallStack => T.DBConfig beM -> m (T.DBResult (T.SqlConn beM))

  -- | Inits a KV DB connection using a config.
  --
  -- Returns an error (Left $ KVDBError KVDBConnectionAlreadyExists msg)
  -- if the connection already exists.
  --
  -- Thread safe, exception free.
  initKVDBConnection :: HasCallStack => T.KVDBConfig -> m (T.KVDBAnswer T.KVDBConn)

  -- | Deinits the given KV DB connection.
  -- Does nothing if the connection is not found (might have been closed earlier).
  --
  -- Thread safe, exception free.
  deinitKVDBConnection :: HasCallStack => T.KVDBConn  -> m ()

  -- | Get the existing connection.

  -- Returns an error (Left $ KVDBError KVDBConnectionDoesNotExist)
  -- if the connection does not exits for this config.
  --
  -- Thread safe, exception free.
  getKVDBConnection :: HasCallStack => T.KVDBConfig -> m (T.KVDBAnswer T.KVDBConn)

  -- | Evaluates SQL DB operations outside of any transaction.
  -- It's possible to have a chain of SQL DB calls (within the SqlDB language).
  -- These chains will be executed as a single transaction.
  --
  -- Thread safe, exception free.
  --
  -- The underlying library is beam which allows to access 3 different SQL backends.
  -- See TUTORIAL.md, README.md and QueryExamplesSpec.hs for more info.
  --
  -- > myFlow :: HasCallStack => L.Flow (T.DBResult (Maybe User))
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
      ( HasCallStack
      , T.JSONEx a
      , T.BeamRunner beM
      , T.BeamRuntime be beM
      )
    => T.SqlConn beM
    -> L.SqlDB beM a
    -> m (T.DBResult a)

  -- | Like `runDB` but runs inside a SQL transaction.
  runTransaction
    ::
      ( HasCallStack
      , T.JSONEx a
      , T.BeamRunner beM
      , T.BeamRuntime be beM
      )
    => T.SqlConn beM
    -> L.SqlDB beM a
    -> m (T.DBResult a)

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
    :: (HasCallStack, FromJSON a, ToJSON a)
    => Maybe T.Microseconds
    -> T.Awaitable (Either Text a)
    -> m (Either T.AwaitingError a)

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
  throwException :: forall a e. (HasCallStack, Exception e) => e -> m a

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
  runSafeFlow :: (HasCallStack, FromJSON a, ToJSON a) => Flow a -> m (Either Text a)

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
    :: HasCallStack
    => Text
    -> KVDB a -- ^ KVDB action
    -> m (T.KVDBAnswer a)

  ---- Experimental Pub Sub implementation using Redis Pub Sub.

  runPubSub
    :: HasCallStack
    => PubSub a
    -> m a

  -- | Publish payload to channel.
  publish
    :: HasCallStack
    => PSL.Channel                        -- ^ Channel in which payload will be send
    -> PSL.Payload                        -- ^ Payload
    -> m (Either T.KVDBReply Integer)  -- ^ Number of subscribers received payload

  -- | Subscribe to all channels from list.
  -- Note: Subscription won't be unsubscribed automatically on thread end.
  -- Use canceller explicitly to cancel subscription
  subscribe
    :: HasCallStack
    => [PSL.Channel]    -- ^ List of channels to subscribe
    -> MessageCallback  -- ^ Callback function.
    -> m (Flow ())   -- ^ Inner flow is a canceller of current subscription

  -- | Subscribe to all channels from list. Respects redis pattern syntax.
  -- Note: Subscription won't be unsubscribed automatically on thread end.
  -- Use canceller explicitly to cancel subscription
  psubscribe
    :: HasCallStack
    => [PSL.ChannelPattern] -- ^ List of channels to subscribe (wit respect to patterns supported by redis)
    -> PMessageCallback     -- ^ Callback function
    -> m (Flow ())       -- ^ Inner flow is a canceller of current subscription


instance MonadFlow Flow where
  callServantAPI mbMgrSel url cl = liftFC $ CallServantAPI mbMgrSel url cl id
  callHTTPWithCert url cert = liftFC $ CallHTTP url cert id
  evalLogger' logAct = liftFC $ EvalLogger logAct id

  runIO' descr ioAct = liftFC $ RunIO descr ioAct id
  runUntracedIO' descr ioAct = liftFC $ RunUntracedIO descr ioAct id

  getOption :: forall k v. (HasCallStack, T.OptionEntity k v) => k -> Flow (Maybe v)
  getOption k = liftFC $ GetOption (T.mkOptionKey @k @v k) id

  setOption :: forall k v. (HasCallStack, T.OptionEntity k v) => k -> v -> Flow ()
  setOption k v = liftFC $ SetOption (T.mkOptionKey @k @v k) v id

  delOption :: forall k v. (HasCallStack, T.OptionEntity k v) => k -> Flow ()
  delOption k = liftFC $ DelOption (T.mkOptionKey @k @v k) id

  generateGUID = liftFC $ GenerateGUID id

  runSysCmd cmd = liftFC $ RunSysCmd cmd id

  initSqlDBConnection cfg = liftFC $ InitSqlDBConnection cfg id
  deinitSqlDBConnection conn = liftFC $ DeInitSqlDBConnection conn id
  getSqlDBConnection cfg = liftFC $ GetSqlDBConnection cfg id

  initKVDBConnection cfg = liftFC $ InitKVDBConnection cfg id
  deinitKVDBConnection conn = liftFC $ DeInitKVDBConnection conn id
  getKVDBConnection cfg = liftFC $ GetKVDBConnection cfg id

  runDB conn dbAct = liftFC $ RunDB conn dbAct False id
  runTransaction conn dbAct = liftFC $ RunDB conn dbAct True id

  await mbMcs awaitable = liftFC $ Await mbMcs awaitable id
  throwException ex = do
    -- Doubt: Should we just print the exception details without the
    -- contextual details that logError prints. As finding the message inside logError is a bit
    -- cumbersome. Just printing the exception details will be much cleaner if we don't need the
    -- contextual details.
    logError "Exception" $ Text.pack $ displayException ex
    liftFC $ ThrowException ex id

  runSafeFlow flow = do
    safeFlowGUID <- generateGUID
    liftFC $ RunSafeFlow safeFlowGUID flow id

  runKVDB cName act = liftFC $ RunKVDB cName act id

  runPubSub act = liftFC $ RunPubSub act id

  publish channel payload = runPubSub $ PubSub $ const $ PSL.publish channel payload

  subscribe channels cb = fmap (runIO' "subscribe") $
    runPubSub $ PubSub $ \runFlow -> PSL.subscribe channels (runFlow . cb)

  psubscribe channels cb = fmap (runIO' "psubscribe") $
    runPubSub $ PubSub $ \runFlow -> PSL.psubscribe channels (\ch -> runFlow . cb ch)


-- instance (MonadTrans t, MonadFlow m, Monad m, Monad (t m)) => MonadFlow (t m) where
instance MonadFlow m => MonadFlow (ReaderT r m) where
  callServantAPI mbMgrSel url = lift . callServantAPI mbMgrSel url
  callHTTPWithCert url = lift . callHTTPWithCert url
  evalLogger' = lift . evalLogger'
  runIO' descr = lift . runIO' descr
  runUntracedIO' descr = lift . runUntracedIO' descr
  getOption = lift . getOption
  setOption k = lift . setOption k
  delOption = lift . delOption
  generateGUID = lift generateGUID
  runSysCmd = lift . runSysCmd
  initSqlDBConnection = lift . initSqlDBConnection
  deinitSqlDBConnection = lift . deinitSqlDBConnection
  getSqlDBConnection = lift . getSqlDBConnection
  initKVDBConnection = lift . initKVDBConnection
  deinitKVDBConnection = lift . deinitKVDBConnection
  getKVDBConnection = lift . getKVDBConnection
  runDB conn = lift . runDB conn
  runTransaction conn = lift . runTransaction conn
  await mbMcs = lift . await mbMcs
  throwException =  lift . throwException
  runSafeFlow = lift . runSafeFlow
  runKVDB cName = lift . runKVDB cName
  runPubSub = lift . runPubSub
  publish channel = lift . publish channel
  subscribe channels = lift . subscribe channels
  psubscribe channels = lift . psubscribe channels

instance MonadFlow m => MonadFlow (StateT s m) where
  callServantAPI mbMgrSel url = lift . callServantAPI mbMgrSel url
  callHTTPWithCert url = lift . callHTTPWithCert url
  evalLogger' = lift . evalLogger'
  runIO' descr = lift . runIO' descr
  runUntracedIO' descr = lift . runUntracedIO' descr
  getOption = lift . getOption
  setOption k = lift . setOption k
  delOption = lift . delOption
  generateGUID = lift generateGUID
  runSysCmd = lift . runSysCmd
  initSqlDBConnection = lift . initSqlDBConnection
  deinitSqlDBConnection = lift . deinitSqlDBConnection
  getSqlDBConnection = lift . getSqlDBConnection
  initKVDBConnection = lift . initKVDBConnection
  deinitKVDBConnection = lift . deinitKVDBConnection
  getKVDBConnection = lift . getKVDBConnection
  runDB conn = lift . runDB conn
  runTransaction conn = lift . runTransaction conn
  await mbMcs = lift . await mbMcs
  throwException =  lift . throwException
  runSafeFlow = lift . runSafeFlow
  runKVDB cName = lift . runKVDB cName
  runPubSub = lift . runPubSub
  publish channel = lift . publish channel
  subscribe channels = lift . subscribe channels
  psubscribe channels = lift . psubscribe channels

instance (MonadFlow m, Monoid w) => MonadFlow (WriterT w m) where
  callServantAPI mbMgrSel url = lift . callServantAPI mbMgrSel url
  callHTTPWithCert url = lift . callHTTPWithCert url
  evalLogger' = lift . evalLogger'
  runIO' descr = lift . runIO' descr
  runUntracedIO' descr = lift . runUntracedIO' descr
  getOption = lift . getOption
  setOption k = lift . setOption k
  delOption = lift . delOption
  generateGUID = lift generateGUID
  runSysCmd = lift . runSysCmd
  initSqlDBConnection = lift . initSqlDBConnection
  deinitSqlDBConnection = lift . deinitSqlDBConnection
  getSqlDBConnection = lift . getSqlDBConnection
  initKVDBConnection = lift . initKVDBConnection
  deinitKVDBConnection = lift . deinitKVDBConnection
  getKVDBConnection = lift . getKVDBConnection
  runDB conn = lift . runDB conn
  runTransaction conn = lift . runTransaction conn
  await mbMcs = lift . await mbMcs
  throwException =  lift . throwException
  runSafeFlow = lift . runSafeFlow
  runKVDB cName = lift . runKVDB cName
  runPubSub = lift . runPubSub
  publish channel = lift . publish channel
  subscribe channels = lift . subscribe channels
  psubscribe channels = lift . psubscribe channels

instance MonadFlow m => MonadFlow (ExceptT e m) where
  callServantAPI mbMgrSel url = lift . callServantAPI mbMgrSel url
  callHTTPWithCert url = lift . callHTTPWithCert url
  evalLogger' = lift . evalLogger'
  runIO' descr = lift . runIO' descr
  runUntracedIO' descr = lift . runUntracedIO' descr
  getOption = lift . getOption
  setOption k = lift . setOption k
  delOption = lift . delOption
  generateGUID = lift generateGUID
  runSysCmd = lift . runSysCmd
  initSqlDBConnection = lift . initSqlDBConnection
  deinitSqlDBConnection = lift . deinitSqlDBConnection
  getSqlDBConnection = lift . getSqlDBConnection
  initKVDBConnection = lift . initKVDBConnection
  deinitKVDBConnection = lift . deinitKVDBConnection
  getKVDBConnection = lift . getKVDBConnection
  runDB conn = lift . runDB conn
  runTransaction conn = lift . runTransaction conn
  await mbMcs = lift . await mbMcs
  throwException =  lift . throwException
  runSafeFlow = lift . runSafeFlow
  runKVDB cName = lift . runKVDB cName
  runPubSub = lift . runPubSub
  publish channel = lift . publish channel
  subscribe channels = lift . subscribe channels
  psubscribe channels = lift . psubscribe channels

instance (MonadFlow m, Monoid w) => MonadFlow (RWST r w s m) where
  callServantAPI mbMgrSel url = lift . callServantAPI mbMgrSel url
  callHTTPWithCert url = lift . callHTTPWithCert url
  evalLogger' = lift . evalLogger'
  runIO' descr = lift . runIO' descr
  runUntracedIO' descr = lift . runUntracedIO' descr
  getOption = lift . getOption
  setOption k = lift . setOption k
  delOption = lift . delOption
  generateGUID = lift generateGUID
  runSysCmd = lift . runSysCmd
  initSqlDBConnection = lift . initSqlDBConnection
  deinitSqlDBConnection = lift . deinitSqlDBConnection
  getSqlDBConnection = lift . getSqlDBConnection
  initKVDBConnection = lift . initKVDBConnection
  deinitKVDBConnection = lift . deinitKVDBConnection
  getKVDBConnection = lift . getKVDBConnection
  runDB conn = lift . runDB conn
  runTransaction conn = lift . runTransaction conn
  await mbMcs = lift . await mbMcs
  throwException =  lift . throwException
  runSafeFlow = lift . runSafeFlow
  runKVDB cName = lift . runKVDB cName
  runPubSub = lift . runPubSub
  publish channel = lift . publish channel
  subscribe channels = lift . subscribe channels
  psubscribe channels = lift . psubscribe channels

