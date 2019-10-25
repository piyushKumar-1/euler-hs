{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module EulerHS.Framework.Flow.Entries where


import EulerHS.Prelude
import EulerHS.Types (RRItem(..), MockedResult(..))
import qualified Data.Aeson     as A
import qualified EulerHS.Types  as T
import qualified Servant.Client as S
import Data.Generics.Product.Positions (getPosition)

----------------------------------------------------------------------

data RunDBEntry = RunDBEntry
  { jsonResult :: A.Value
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

mkRunDBEntry :: T.JSONEx a => T.DBResult a -> RunDBEntry
mkRunDBEntry = RunDBEntry . T.jsonEncode

instance RRItem RunDBEntry where
  getTag _ = "RunDBEntry"

instance T.JSONEx a => MockedResult RunDBEntry (T.DBResult a) where
  getMock RunDBEntry {jsonResult} = T.jsonDecode jsonResult


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

-- data RunKVDBEntry = RunKVDBEntry
--   deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- mkRunKVDBEntry :: a -> RunKVDBEntry
-- mkRunKVDBEntry _ = RunKVDBEntry

-- instance RRItem RunKVDBEntry where
--   getTag _ = "RunKVDBEntry"

-- instance MockedResult RunKVDBEntry a where
--   getMock _ = Just $
--     error "Not Implemented MockedResult RunKVDBEntry"

----------------------------------------------------------------------

data ThrowExceptionEntry = ThrowExceptionEntry
  { exMessage :: String
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

mkThrowExceptionEntry :: Exception e => e -> a -> ThrowExceptionEntry
mkThrowExceptionEntry e _ = ThrowExceptionEntry $ show e

instance RRItem ThrowExceptionEntry where
  getTag _ = "ThrowExceptionEntry"

instance MockedResult ThrowExceptionEntry a where
  getMock _ = Just $
    error "This shold not be evaluated: throw exception result"

----------------------------------------------------------------------

data CallServantAPIEntry = CallServantAPIEntry
  { baseUrl    :: S.BaseUrl
  , jsonResult :: A.Value
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

mkCallServantAPIEntry
  :: T.JSONEx a
  => S.BaseUrl
  -> Either S.ClientError a
  -> CallServantAPIEntry
mkCallServantAPIEntry burl = CallServantAPIEntry burl . T.jsonEncode

instance RRItem CallServantAPIEntry where
  getTag _ = "CallServantAPIEntry"

instance T.JSONEx a => MockedResult CallServantAPIEntry (Either S.ClientError a) where
    getMock CallServantAPIEntry {jsonResult} = T.jsonDecode jsonResult

-- ----------------------------------------------------------------------

data SetOptionEntry = SetOptionEntry
  { key   :: A.Value
  , value :: A.Value
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

mkSetOptionEntry :: T.OptionEntity k v => k -> v -> () -> SetOptionEntry
mkSetOptionEntry k v _ = SetOptionEntry (toJSON k) (toJSON v)

instance RRItem SetOptionEntry where
  getTag _ = "SetOptionEntry"

instance MockedResult SetOptionEntry () where
  getMock _ = Just ()

-- ----------------------------------------------------------------------

data GetOptionEntry = GetOptionEntry
  { key   :: A.Value
  , value :: A.Value
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

mkGetOptionEntry :: T.OptionEntity k v => k -> Maybe v -> GetOptionEntry
mkGetOptionEntry k mv = GetOptionEntry (toJSON k) (toJSON mv)

instance RRItem GetOptionEntry where
  getTag _ = "GetOptionEntry"

instance FromJSON v => MockedResult GetOptionEntry v where
  getMock GetOptionEntry{value} = T.fromJSONMaybe value

-- ----------------------------------------------------------------------

data RunSysCmdEntry = RunSysCmdEntry
  { cmd    :: String
  , result :: String
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

mkRunSysCmdEntry :: String -> String -> RunSysCmdEntry
mkRunSysCmdEntry cmd result = RunSysCmdEntry cmd result

instance RRItem RunSysCmdEntry where
  getTag _ = "RunSysCmdEntry"

instance MockedResult RunSysCmdEntry String where
  getMock RunSysCmdEntry {..} = Just result

-- ----------------------------------------------------------------------

data ForkEntry = ForkEntry
  { description :: Text
  , guid        :: Text
  } deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

mkForkEntry :: Text -> Text -> () -> ForkEntry
mkForkEntry desc guid _ = ForkEntry desc guid

instance RRItem ForkEntry where
  getTag _ = "ForkEntry"

instance MockedResult ForkEntry () where
  getMock _ = Just ()

-- ----------------------------------------------------------------------

data GenerateGUIDEntry = GenerateGUIDEntry
  { guid :: Text
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

mkGenerateGUIDEntry :: Text -> GenerateGUIDEntry
mkGenerateGUIDEntry = GenerateGUIDEntry

instance RRItem GenerateGUIDEntry where
  getTag _ = "GenerateGUIDEntry"

instance MockedResult GenerateGUIDEntry Text where
  getMock (GenerateGUIDEntry g) = Just g

-- ----------------------------------------------------------------------

data RunIOEntry = RunIOEntry
  { jsonResult :: A.Value
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

mkRunIOEntry
  :: forall a
   . T.JSONEx a
  => a
  -> RunIOEntry
mkRunIOEntry = RunIOEntry .
  T.resolveJSONEx @a T.jsonEncode toJSON

instance RRItem RunIOEntry where
  getTag _ = "RunIOEntry"

instance T.JSONEx a => MockedResult RunIOEntry a where
    getMock (RunIOEntry r) =
      T.resolveJSONEx @a T.jsonDecode T.fromJSONMaybe r


-- ----------------------------------------------------------------------

data InitSqlDBConnectionEntry beM = InitSqlDBConnectionEntry
  { dBConfig :: T.DBConfig beM
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

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





