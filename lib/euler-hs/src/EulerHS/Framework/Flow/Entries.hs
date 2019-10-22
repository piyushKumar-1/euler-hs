{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables #-}

module EulerHS.Framework.Flow.Entries where


import EulerHS.Prelude
import EulerHS.Types (RRItem(..), MockedResult(..), encodeToStr, decodeFromStr)
import qualified EulerHS.Types as T
import qualified Servant.Client as S
import Data.Generics.Product.Positions (getPosition)


----------------------------------------------------------------------

data RunDBEntry = RunDBEntry
  -- { jsonConnection :: String
  { jsonResult     :: String
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

mkRunDBEntry :: ToJSON a => a -> RunDBEntry
mkRunDBEntry = RunDBEntry . encodeToStr

instance RRItem RunDBEntry where
  getTag _ = "RunDBEntry"

instance FromJSON a => MockedResult RunDBEntry a where
  getMock RunDBEntry {jsonResult} = decodeFromStr jsonResult


----------------------------------------------------------------------

-- data EvalLoggerEntry = EvalLoggerEntry
--   deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

-- mkEvalLoggerEntry :: a -> EvalLoggerEntry
-- mkEvalLoggerEntry _ = EvalLoggerEntry

-- instance RRItem EvalLoggerEntry where
--   getTag _ = "EvalLoggerEntry"

-- instance MockedResult EvalLoggerEntry a where
--   getMock _ = Just $
--     error "Not Implemented MockedResult EvalLoggerEntry"

----------------------------------------------------------------------

-- MOCK, TODO!

data RunKVDBEntry = RunKVDBEntry
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

mkRunKVDBEntry :: a -> RunKVDBEntry
mkRunKVDBEntry _ = RunKVDBEntry

instance RRItem RunKVDBEntry where
  getTag _ = "RunKVDBEntry"

instance MockedResult RunKVDBEntry a where
  getMock _ = Just $
    error "Not Implemented MockedResult RunKVDBEntry"

----------------------------------------------------------------------

data ThrowExceptionEntry = ThrowExceptionEntry
  { exMessage :: String
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

mkThrowExceptionEntry :: Exception e => e -> a -> ThrowExceptionEntry
mkThrowExceptionEntry e _ = ThrowExceptionEntry $ show e

instance RRItem ThrowExceptionEntry where
  getTag _ = "ThrowExceptionEntry"

instance MockedResult ThrowExceptionEntry a where
  getMock _ = Just $
    error "This shold not be evaluated: throw exception result"

----------------------------------------------------------------------

-- MOCK, TODO!

data CallServantAPIEntry = CallServantAPIEntry
  { baseUrl    :: S.BaseUrl
  -- , jsonResult :: String
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

mkCallServantAPIEntry
  :: S.BaseUrl
  -> Either S.ClientError a
  -> CallServantAPIEntry
mkCallServantAPIEntry burl _ = CallServantAPIEntry burl

instance RRItem CallServantAPIEntry where
  getTag _ = "CallServantAPIEntry"

instance MockedResult CallServantAPIEntry (Either S.ClientError a) where
  getMock _ = Just $
    error "Not Implemented MockedResult CallServantAPIEntry"

----------------------------------------------------------------------

data SetOptionEntry = SetOptionEntry
  { key   :: String
  , value :: String
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

mkSetOptionEntry :: T.OptionEntity k v => k -> v -> () -> SetOptionEntry
mkSetOptionEntry k v _ = SetOptionEntry (encodeToStr k) (encodeToStr v)

instance RRItem SetOptionEntry where
  getTag _ = "SetOptionEntry"

instance MockedResult SetOptionEntry () where
  getMock _ = Just ()

----------------------------------------------------------------------

data GetOptionEntry = GetOptionEntry
  { key   :: String
  , value :: String
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

mkGetOptionEntry :: T.OptionEntity k v => k -> Maybe v -> GetOptionEntry
mkGetOptionEntry k mv = GetOptionEntry (encodeToStr k) (encodeToStr mv)

instance RRItem GetOptionEntry where
  getTag _ = "GetOptionEntry"

instance FromJSON v => MockedResult GetOptionEntry v where
  getMock GetOptionEntry{..} = decodeFromStr value

----------------------------------------------------------------------

data RunSysCmdEntry = RunSysCmdEntry
  { cmd    :: String
  , result :: String
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

mkRunSysCmdEntry :: String -> String -> RunSysCmdEntry
mkRunSysCmdEntry cmd result = RunSysCmdEntry cmd result

instance RRItem RunSysCmdEntry where
  getTag _ = "RunSysCmdEntry"

instance MockedResult RunSysCmdEntry String where
  getMock RunSysCmdEntry {..} = Just result

----------------------------------------------------------------------

data ForkEntry = ForkEntry
  { description :: Text
  , guid        :: Text
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

mkForkEntry :: Text -> Text -> () -> ForkEntry
mkForkEntry desc guid _ = ForkEntry desc guid

instance RRItem ForkEntry where
  getTag _ = "ForkEntry"

instance MockedResult ForkEntry () where
  getMock _ = Just ()

----------------------------------------------------------------------

data GenerateGUIDEntry = GenerateGUIDEntry
  { guid :: Text
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

mkGenerateGUIDEntry :: Text -> GenerateGUIDEntry
mkGenerateGUIDEntry = GenerateGUIDEntry

instance RRItem GenerateGUIDEntry where
  getTag _ = "GenerateGUIDEntry"

instance MockedResult GenerateGUIDEntry Text where
  getMock (GenerateGUIDEntry g) = Just g

----------------------------------------------------------------------

data RunIOEntry = RunIOEntry
  { jsonResult :: String
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

mkRunIOEntry :: ToJSON a => a -> RunIOEntry
mkRunIOEntry = RunIOEntry . encodeToStr

instance RRItem RunIOEntry where
  getTag _ = "RunIOEntry"

instance FromJSON a => MockedResult RunIOEntry a where
  getMock (RunIOEntry r) = decodeFromStr r


----------------------------------------------------------------------

data InitSqlDBConnectionEntry beM = InitSqlDBConnectionEntry
  { dBConfig :: T.DBConfig beM
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

mkInitSqlDBConnectionEntry :: T.DBConfig beM -> a -> InitSqlDBConnectionEntry beM
mkInitSqlDBConnectionEntry dbcfg _ = InitSqlDBConnectionEntry dbcfg

instance RRItem (InitSqlDBConnectionEntry beM)  where
  getTag _ = "InitSqlDBConnectionEntry"

instance MockedResult (InitSqlDBConnectionEntry beM) (T.DBResult (T.SqlConn beM)) where
  getMock (InitSqlDBConnectionEntry _) = Just $ Right $ T.MockedConn ""


----------------------------------------------------------------------

data DeInitSqlDBConnectionEntry beM = DeInitSqlDBConnectionEntry
  { connTag :: Text
  } deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

mkDeInitSqlDBConnectionEntry :: T.SqlConn beM -> a -> DeInitSqlDBConnectionEntry beM
mkDeInitSqlDBConnectionEntry cfg _ = DeInitSqlDBConnectionEntry (getPosition @1 cfg)

instance RRItem (DeInitSqlDBConnectionEntry beM) where
  getTag _ = "DeInitSqlDBConnectionEntry"

instance MockedResult (DeInitSqlDBConnectionEntry beM) () where
  getMock (DeInitSqlDBConnectionEntry _) = Just ()
