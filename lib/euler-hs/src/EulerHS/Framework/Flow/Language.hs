{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE DeriveAnyClass        #-}

module EulerHS.Framework.Flow.Language
  (
  -- * Flow language
  -- ** Types
    Description
  , Flow
  , ForkGUID
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
  -- *** Other
  , callServantAPI
  , callAPI
  , runIO
  , generateGUID
  , runSysCmd
  , forkFlow
  , throwException
  ) where

import           EulerHS.Prelude hiding (getOption)

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

  fmap f (GetSqlDBConnection cfg next)        = GetSqlDBConnection cfg (f . next)

  fmap f (RunDB conn sqlDbAct next)           = RunDB conn sqlDbAct (f . next)

  fmap f (RunKVDB act next)                   = RunKVDB act (f . next)

type Flow = F FlowMethod


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
-- > getUser :: ClientM User
-- > getBook :: ClientM Book
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
  => BaseUrl                     -- ^ remote url 'BaseUrl'
  -> ClientM a                   -- ^ servant client 'ClientM'
  -> Flow (Either ClientError a) -- ^ result
callServantAPI url cl = liftFC $ CallServantAPI url cl id

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
-- > getUser :: ClientM User
-- > getBook :: ClientM Book
-- > (getUser :<|> getBook) = client api
-- >
-- > url = BaseUrl Http "localhost" port ""
-- >
-- >
-- > myFlow = do
-- >   book <- callAPI url getBook
-- >   user <- callAPI url getUser

callAPI :: T.JSONEx a => BaseUrl -> ClientM a -> Flow (Either ClientError a)
callAPI = callServantAPI

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

-- | Run some IO operation, result should have 'ToJSONEx' instance (extended 'ToJSON'),
-- because we have to collect it in recordings for ART system.
--
-- > myFlow = do
-- >   content <- runIO $ readFromFile file
-- >   logDebug "content id" $ extractContentId content
-- >   pure content
runIO :: T.JSONEx a => IO a -> Flow a
runIO ioAct = liftFC $ RunIO ioAct id


-- | Get stored option by typed key.
getOption :: T.OptionEntity k v => k -> Flow (Maybe v)
getOption k = liftFC $ GetOption k id


-- >  data MerchantIdKey = MerchantIdKey
-- >
-- >  instance OptionEntity MerchantIdKey Text
-- >
-- >  myFlow = do
-- >    _ <- setOption MerchantIdKey "abc1234567"
-- >    mKey <- getOption MerchantKey
-- >    runIO $ putTextLn mKey
setOption :: T.OptionEntity k v => k -> v -> Flow ()
setOption k v = liftFC $ SetOption k v id

-- | Just generate version 4 UUIDs as specified in RFC 4122
-- e.g. 25A8FC2A-98F2-4B86-98F6-84324AF28611.
-- Universally unique identifier (UUID).
-- The term globally unique identifier (GUID) is also used, typically in software created by Microsoft.
generateGUID :: Flow Text
generateGUID = liftFC $ GenerateGUID id

-- | Run system command end return output.
--
-- > myFlow = do
-- >   currentDir <- runSysCmd "pwd"
-- >   logInfo "currentDir" $ toText currentDir
-- >   ...
runSysCmd :: String -> Flow String
runSysCmd cmd = liftFC $ RunSysCmd cmd id

-- | Takes SQL DB config and create connection that can be used in queries.
initSqlDBConnection :: T.DBConfig beM -> Flow (T.DBResult (T.SqlConn beM))
initSqlDBConnection cfg = liftFC $ InitSqlDBConnection cfg id

-- | Deinit the given connection if you want to deny access over that connection.
deinitSqlDBConnection :: T.SqlConn beM -> Flow ()
deinitSqlDBConnection conn = liftFC $ DeInitSqlDBConnection conn id

-- | Get existing connection.
-- If there is no such connection, returns error.
getSqlDBConnection ::T.DBConfig beM -> Flow (T.DBResult (T.SqlConn beM))
getSqlDBConnection cfg = liftFC $ GetSqlDBConnection cfg id

-- | Takes Redis DB config and create connection that can be used in queries.
initKVDBConnection :: T.KVDBConfig -> Flow (T.KVDBAnswer T.KVDBConn)
initKVDBConnection cfg = liftFC $ InitKVDBConnection cfg id

-- | Deinit the given connection if you want to deny access over that connection.
deinitKVDBConnection :: T.KVDBConn  -> Flow ()
deinitKVDBConnection conn = liftFC $ DeInitKVDBConnection conn id

-- | Takes connection, sql query (described using BEAM syntax) and make request.
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

-- | Fork given flow.
--
-- > myFlow1 = do
-- >   logInfo "myflow1" "logFromMyFlow1"
-- >   someAction
-- >
-- > myFlow2 = do
-- >   res <- runIO someAction
-- >   forkFlow "myFlow1 fork" myFlow1
-- >   pure res
forkFlow :: T.JSONEx a => Text -> Flow a -> Flow ()
forkFlow description flow = do
  flowGUID <- generateGUID
  unless (null description) $ logInfo tag $ "Flow forked. Description: " <> description <> " GUID: " <> flowGUID
  when   (null description) $ logInfo tag $ "Flow forked. GUID: " <> flowGUID
  void $ liftFC $ Fork description flowGUID flow id
  where
    tag :: Text
    tag = "ForkFlow"


-- | Throw given exception.
--   In module Servant.Server you can find alot of predefined HTTP exceptions
--   for different status codes.
--
-- > myFlow = do
-- >   res <- authAction
-- >   case res of
-- >     Failure reason -> throwException err403 {errBody = reason}
-- >     Success -> ...
throwException :: forall a e. Exception e => e -> Flow a
throwException ex = liftFC $ ThrowException ex id

-- | Execute given kvdb actions.
--
-- > myFlow = do
-- >   kvres <- L.runKVDB $ do
-- >     set "aaa" "bbb"
-- >     res <- get "aaa"
-- >     del ["aaa"]
-- >     pure res
runKVDB
  :: KVDB a -- ^ KVDB action
  -> Flow (T.KVDBAnswer a)
runKVDB act = liftFC $ RunKVDB act id
