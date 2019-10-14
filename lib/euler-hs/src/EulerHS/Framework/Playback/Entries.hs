{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables #-}

module EulerHS.Framework.Playback.Entries where


import EulerHS.Prelude
import EulerHS.Framework.Playback.Types
import qualified EulerHS.Framework.Types as T
import qualified EulerHS.Core.Types as T

  -- CallAPI
  --   :: T.RestEndpoint req resp
  --   => req
  --   -> (T.APIResult resp -> next)
  --   -> FlowMethod next

  -- CallServantAPI
  --   :: BaseUrl
  --   -> ClientM a
  --   -> (Either ClientError a -> next)
  --   -> FlowMethod next

  -- EvalLogger
  --   :: Logger a
  --   -> (a -> next)
  --   -> FlowMethod next


  -- Fork
  --   :: Description
  --   -> ForkGUID
  --   -> Flow s
  --   -> (() -> next)
  --   -> FlowMethod next

  -- ThrowException
  --   :: forall a e next
  --    . Exception e
  --   => e
  --   -> (a -> next)
  --   -> FlowMethod next

  -- -- TODO: Disconnect :: _ -> FlowMethod next

  -- RunDB
  --   :: T.SqlConn
  --   -> SqlDB T.DbBackend a
  --   -> (T.DBResult a -> next)
  --   -> FlowMethod next

  -- RunKVDB
  --   :: KVDB a
  --   -> (T.KVDBAnswer a -> next)
  --   -> FlowMethod next


    --


data SetOptionEntry = SetOptionEntry
  { key   :: String
  , value :: String
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

mkSetOptionEntry :: T.OptionEntity k v => k -> v -> () -> SetOptionEntry
mkSetOptionEntry k v _ = SetOptionEntry (encodeToStr k) (encodeToStr v)

data GetOptionEntry = GetOptionEntry
  { key   :: String
  , value :: String
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

mkGetOptionEntry :: T.OptionEntity k v => k -> Maybe v -> GetOptionEntry
mkGetOptionEntry k mv = GetOptionEntry (encodeToStr k) (encodeToStr mv)

data RunSysCmdEntry = RunSysCmdEntry
  { cmd    :: String
  , result :: String
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

mkRunSysCmdEntry :: String -> String -> RunSysCmdEntry
mkRunSysCmdEntry cmd result = RunSysCmdEntry cmd result

data ForkEntry = ForkEntry
  { description :: Text
  , guid        :: Text
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

mkForkEntry :: Text -> Text -> () -> ForkEntry
mkForkEntry desc guid _ = ForkEntry desc guid

data GenerateGUIDEntry = GenerateGUIDEntry
  { guid :: Text
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

mkGenerateGUIDEntry :: Text -> GenerateGUIDEntry
mkGenerateGUIDEntry = GenerateGUIDEntry

data RunIOEntry = RunIOEntry
  { jsonResult :: String
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

mkRunIOEntry :: ToJSON a => a -> RunIOEntry
mkRunIOEntry = RunIOEntry . encodeToStr

-- data LogInfoEntry = LogInfoEntry
--   { message :: String
--   }
--   deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

-- mkLogInfoEntry :: String -> () -> LogInfoEntry
-- mkLogInfoEntry msg _ = LogInfoEntry msg

data ConnectEntry = ConnectEntry
  { dBConfig :: T.DBConfig
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

mkConnectEntry :: T.DBConfig -> ConnectEntry
mkConnectEntry = ConnectEntry

-- data RunDBEntry = RunDBEntry
--   { dbeDBName :: String
--   , dbeDescription :: String
--   , dbeJsonResult :: String
--   }
--   deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

-- mkRunDBEntry
--   :: ToJSON a
--   => Connection
--   -> String
--   -> a
--   -> RunDBEntry
-- mkRunDBEntry (NativeConn dbName _) qInfo dbRes = RunDBEntry dbName qInfo $ encodeToStr dbRes
-- mkRunDBEntry (MockedConn dbName) qInfo dbRes = RunDBEntry dbName qInfo $ encodeToStr dbRes

instance RRItem GetOptionEntry where
  toRecordingEntry rrItem idx mode = RecordingEntry idx mode "GetOptionEntry" $ encodeToStr rrItem
  fromRecordingEntry (RecordingEntry _ _ _ payload) = decodeFromStr payload
  getTag _ = "GetOptionEntry"

instance FromJSON v => MockedResult GetOptionEntry v where
  getMock GetOptionEntry{..} = decodeFromStr value

instance RRItem SetOptionEntry where
  toRecordingEntry rrItem idx mode = RecordingEntry idx mode "SetOptionEntry" $ encodeToStr rrItem
  fromRecordingEntry (RecordingEntry _ _ _ payload) = decodeFromStr payload
  getTag _ = "SetOptionEntry"

instance MockedResult SetOptionEntry () where
  getMock _ = Just ()

instance RRItem RunSysCmdEntry where
  toRecordingEntry rrItem idx mode = RecordingEntry idx mode "RunSysCmdEntry" $ encodeToStr rrItem
  fromRecordingEntry (RecordingEntry _ _ _ payload) = decodeFromStr payload
  getTag _ = "RunSysCmdEntry"

instance MockedResult RunSysCmdEntry String where
  getMock RunSysCmdEntry {..} = Just result

instance RRItem ForkEntry where
  toRecordingEntry rrItem idx mode = RecordingEntry idx mode "ForkEntry" $ encodeToStr  rrItem
  fromRecordingEntry (RecordingEntry _ _ _ payload) = decodeFromStr payload
  getTag _ = "ForkEntry"

instance MockedResult ForkEntry () where
  getMock _ = Just ()

instance RRItem GenerateGUIDEntry where
  toRecordingEntry rrItem idx mode  = RecordingEntry idx mode "GenerateGUIDEntry" $ encodeToStr rrItem
  fromRecordingEntry (RecordingEntry _ _ _ payload) = decodeFromStr payload
  getTag _ = "GenerateGUIDEntry"

instance MockedResult GenerateGUIDEntry Text where
  getMock (GenerateGUIDEntry g) = Just g

instance RRItem RunIOEntry where
  toRecordingEntry rrItem idx mode = RecordingEntry idx mode "RunIOEntry" $ encodeToStr rrItem
  fromRecordingEntry (RecordingEntry _ _ _ payload) = decodeFromStr payload
  getTag _ = "RunIOEntry"

instance FromJSON a => MockedResult RunIOEntry a where
  getMock (RunIOEntry r) = decodeFromStr r

-- instance RRItem LogInfoEntry where
--   toRecordingEntry rrItem idx mode = RecordingEntry idx mode "LogInfoEntry" $ encodeToStr rrItem
--   fromRecordingEntry (RecordingEntry _ _ _ payload) = decodeFromStr payload
--   getTag _ = "LogInfoEntry"

-- instance MockedResult LogInfoEntry () where
--   getMock _ = Just ()



instance RRItem ConnectEntry where
  toRecordingEntry rrItem idx mode = RecordingEntry idx mode "ConnectEntry" $ encodeToStr rrItem
  fromRecordingEntry (RecordingEntry _ _ _ payload) = decodeFromStr payload
  getTag _ = "ConnectEntry"

instance MockedResult ConnectEntry T.SqlConn where
  getMock (ConnectEntry _) = Just $ T.MockedConn

-- instance RRItem RunDBEntry where
--   toRecordingEntry rrItem idx mode = RecordingEntry idx mode "RunDBEntry" $ encodeToStr rrItem
--   fromRecordingEntry (RecordingEntry _ _ _ payload) = decodeFromStr payload
--   getTag _ = "RunDBEntry"

-- instance FromJSON a => MockedResult RunDBEntry a where
--   getMock (RunDBEntry _ _ r) = decodeFromStr r
