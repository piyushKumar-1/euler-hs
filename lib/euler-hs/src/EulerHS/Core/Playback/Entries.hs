{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

module EulerHS.Core.Playback.Entries where

import EulerHS.Prelude
import EulerHS.Core.Types.Playback (RRItem(..), MockedResult(..))
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

data ThrowExceptionEntry = ThrowExceptionEntry
  { exMessage :: String
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

mkThrowExceptionEntry :: Exception e => e -> a -> ThrowExceptionEntry
mkThrowExceptionEntry e _ = ThrowExceptionEntry $ show e

instance RRItem ThrowExceptionEntry where
  getTag _ = "ThrowExceptionEntry"

instance MockedResult ThrowExceptionEntry a where
  getMock _ = Just $ error "This shold not be evaluated: throw exception result"

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
  { key   :: Text
  , value :: A.Value
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

mkSetOptionEntry :: ToJSON v => Text -> v -> () -> SetOptionEntry
mkSetOptionEntry k v _ = SetOptionEntry k (toJSON v)

instance RRItem SetOptionEntry where
  getTag _ = "SetOptionEntry"

instance MockedResult SetOptionEntry () where
  getMock _ = Just ()

-- ----------------------------------------------------------------------

data GetOptionEntry = GetOptionEntry
  { key   :: Text
  , value :: A.Value
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

mkGetOptionEntry :: (FromJSON v, ToJSON v) => Text -> Maybe v -> GetOptionEntry
mkGetOptionEntry k mv = GetOptionEntry k (toJSON mv)

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
  { description :: Text
  , jsonResult :: A.Value
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

mkRunIOEntry
  :: forall a
   . T.JSONEx a
  => Text
  -> a
  -> RunIOEntry
mkRunIOEntry descr a = RunIOEntry descr $
  (T.resolveJSONEx @a T.jsonEncode toJSON) a

instance RRItem RunIOEntry where
  getTag _ = "RunIOEntry"

instance T.JSONEx a => MockedResult RunIOEntry a where
    getMock (RunIOEntry _ r) =
      T.resolveJSONEx @a T.jsonDecode T.fromJSONMaybe r


-- ----------------------------------------------------------------------

data InitSqlDBConnectionEntry beM = InitSqlDBConnectionEntry
  { dBConfig :: T.DBConfig beM
  , initConnResult :: Either T.DBError ()
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

mkInitSqlDBConnectionEntry :: T.DBConfig beM -> Either T.DBError a -> InitSqlDBConnectionEntry beM
mkInitSqlDBConnectionEntry dbcfg res = case res of
  Left err -> InitSqlDBConnectionEntry dbcfg (Left err)
  Right _ -> InitSqlDBConnectionEntry dbcfg (Right ())

instance RRItem (InitSqlDBConnectionEntry beM)  where
  getTag _ = "InitSqlDBConnectionEntry"

instance MockedResult (InitSqlDBConnectionEntry beM) (T.DBResult (T.SqlConn beM)) where
  getMock  (InitSqlDBConnectionEntry _ res) =
    case res of
      Left err -> Just $ Left err
      Right _  -> Just $ Right $ T.MockedPool ""


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

------------------------------------------------------------------------

data GetSqlDBConnectionEntry beM = GetSqlDBConnectionEntry
  { dBConfig :: T.DBConfig beM
  , getConnResult :: Either T.DBError ()
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

mkGetSqlDBConnectionEntry :: T.DBConfig beM -> Either T.DBError a -> GetSqlDBConnectionEntry beM
mkGetSqlDBConnectionEntry dbcfg res = case res of
  Left err -> GetSqlDBConnectionEntry dbcfg (Left err)
  Right _ -> GetSqlDBConnectionEntry dbcfg (Right ())

instance RRItem (GetSqlDBConnectionEntry beM)  where
  getTag _ = "GetSqlDBConnectionEntry"

instance MockedResult (GetSqlDBConnectionEntry beM) (T.DBResult (T.SqlConn beM)) where
  getMock (GetSqlDBConnectionEntry _ res) =
    case res of
      Left err -> Just $ Left err
      Right _  -> Just $ Right $ T.MockedPool ""

-------------------------------------------------------------------------

data InitKVDBConnectionEntry = InitKVDBConnectionEntry
  { kvdbConfig :: T.KVDBConfig
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

mkInitKVDBConnectionEntry :: T.KVDBConfig -> a -> InitKVDBConnectionEntry
mkInitKVDBConnectionEntry dbcfg _ = InitKVDBConnectionEntry dbcfg

instance RRItem InitKVDBConnectionEntry  where
  getTag _ = "InitKVDBConnectionEntry"

instance MockedResult InitKVDBConnectionEntry (T.KVDBAnswer T.KVDBConn) where
  getMock (InitKVDBConnectionEntry _) = Just $ Right $ T.Mocked ""

-------------------------------------------------------------------------

data DeInitKVDBConnectionEntry = DeInitKVDBConnectionEntry
  { connTag :: Text
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

mkDeInitKVDBConnectionEntry :: T.KVDBConn -> a -> DeInitKVDBConnectionEntry
mkDeInitKVDBConnectionEntry conn _ = DeInitKVDBConnectionEntry (getPosition @1 conn)

instance RRItem DeInitKVDBConnectionEntry  where
  getTag _ = "DeInitKVDBConnectionEntry"

instance MockedResult DeInitKVDBConnectionEntry () where
  getMock (DeInitKVDBConnectionEntry _) = Just ()

-------------------------------------------------------------------------

data GetKVDBConnectionEntry = GetKVDBConnectionEntry
  { kvdbConfig :: T.KVDBConfig
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

mkGetKVDBConnectionEntry :: T.KVDBConfig -> a -> GetKVDBConnectionEntry
mkGetKVDBConnectionEntry dbcfg _ = GetKVDBConnectionEntry dbcfg

instance RRItem GetKVDBConnectionEntry  where
  getTag _ = "GetKVDBConnectionEntry"

instance MockedResult GetKVDBConnectionEntry (T.KVDBAnswer T.KVDBConn) where
  getMock (GetKVDBConnectionEntry _) = Just $ Right $ T.Mocked ""
