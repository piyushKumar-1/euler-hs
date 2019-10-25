{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module EulerHS.Core.KVDB.Entries where


import EulerHS.Prelude
import EulerHS.Types (RRItem(..), MockedResult(..))
import qualified EulerHS.Types    as T
import qualified Data.Aeson       as A
import qualified Data.Aeson.Types as A

data SetEntry = SetEntry
  { jsonKey    :: A.Value
  , jsonValue  :: A.Value
  , jsonResult :: A.Value
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

instance RRItem SetEntry where
  getTag _ = "SetEntry"

instance MockedResult SetEntry (Either T.KVDBReply T.KVDBStatus) where
  getMock SetEntry {jsonResult} = T.jsonDecode jsonResult

mkSetEntry :: ByteString -> ByteString -> Either T.KVDBReply T.KVDBStatus -> SetEntry
mkSetEntry k v r = SetEntry
  (T.jsonEncode k)
  (T.jsonEncode v)
  (T.jsonEncode r)

----------------------------------------------------------------------

data GetEntry = GetEntry
  { jsonKey    :: A.Value
  , jsonResult :: A.Value
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

instance RRItem GetEntry where
  getTag _ = "GetEntry"

instance MockedResult GetEntry (Either T.KVDBReply (Maybe ByteString)) where
  getMock GetEntry {jsonResult} = T.jsonDecode jsonResult


mkGetEntry :: ByteString -> Either T.KVDBReply (Maybe ByteString) -> GetEntry
mkGetEntry k r = GetEntry
  (T.jsonEncode k)
  (T.jsonEncode r)

----------------------------------------------------------------------

data ExistsEntry = ExistsEntry
  { jsonKey    :: A.Value
  , jsonResult :: A.Value
  } deriving (Show, Eq,  Generic, ToJSON, FromJSON)

instance RRItem ExistsEntry where
  getTag _ = "ExistsEntry"

instance MockedResult ExistsEntry (Either T.KVDBReply Bool) where
  getMock ExistsEntry {jsonResult} = T.jsonDecode jsonResult

mkExistsEntry :: ByteString -> Either T.KVDBReply Bool -> ExistsEntry
mkExistsEntry k r = ExistsEntry
  (T.jsonEncode k)
  (T.jsonEncode r)

-- ----------------------------------------------------------------------

data DelEntry = DelEntry
  { jsonKeys   :: A.Value
  , jsonResult :: A.Value
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

instance RRItem DelEntry where
  getTag _ = "DelEntry"

instance MockedResult DelEntry (Either T.KVDBReply Integer) where
  getMock DelEntry {jsonResult} = T.jsonDecode jsonResult

mkDelEntry :: [ByteString] -> Either T.KVDBReply Integer -> DelEntry
mkDelEntry k r = DelEntry
  (T.jsonEncode k)
  (T.jsonEncode r)


-- ----------------------------------------------------------------------

data ExpireEntry = ExpireEntry
  { jsonKey    :: A.Value
  , duration   :: Integer
  , jsonResult :: A.Value
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

instance RRItem ExpireEntry where
  getTag _ = "ExpireEntry"

instance MockedResult ExpireEntry (Either T.KVDBReply Bool) where
  getMock ExpireEntry {jsonResult} = T.jsonDecode jsonResult

mkExpireEntry :: ByteString -> Integer -> Either T.KVDBReply Bool -> ExpireEntry
mkExpireEntry k d r = ExpireEntry
  (T.jsonEncode k)
  d
  (T.jsonEncode r)

-- ----------------------------------------------------------------------

data IncrEntry = IncrEntry
  { jsonKey    :: A.Value
  , jsonResult :: A.Value
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

instance RRItem IncrEntry where
  getTag _ = "IncrEntry"

instance MockedResult IncrEntry (Either T.KVDBReply Integer) where
  getMock IncrEntry {jsonResult} = T.jsonDecode jsonResult

mkIncrEntry :: ByteString -> Either T.KVDBReply Integer -> IncrEntry
mkIncrEntry k r = IncrEntry
  (T.jsonEncode k)
  (T.jsonEncode r)

-- ----------------------------------------------------------------------

data HSetEntry = HSetEntry
  { jsonKey    :: A.Value
  , jsonField  :: A.Value
  , jsonValue  :: A.Value
  , jsonResult :: A.Value
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

instance RRItem HSetEntry where
  getTag _ = "HSetEntry"

instance MockedResult HSetEntry (Either T.KVDBReply Bool) where
  getMock HSetEntry {jsonResult} = T.jsonDecode jsonResult

mkHSetEntry :: ByteString -> ByteString -> ByteString -> Either T.KVDBReply Bool -> HSetEntry
mkHSetEntry k f v r = HSetEntry
  (T.jsonEncode k)
  (T.jsonEncode f)
  (T.jsonEncode v)
  (T.jsonEncode r)

-- ----------------------------------------------------------------------

data HGetEntry = HGetEntry
  { jsonKey    :: A.Value
  , jsonField  :: A.Value
  , jsonResult :: A.Value
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

instance RRItem HGetEntry where
  getTag _ = "HGetEntry"

instance MockedResult HGetEntry (Either T.KVDBReply (Maybe ByteString)) where
  getMock HGetEntry {jsonResult} = T.jsonDecode jsonResult

mkHGetEntry :: ByteString -> ByteString -> Either T.KVDBReply (Maybe ByteString) -> HGetEntry
mkHGetEntry k f r = HGetEntry
  (T.jsonEncode k)
  (T.jsonEncode f)
  (T.jsonEncode r)

-- ----------------------------------------------------------------------

data MultiExecEntry = MultiExecEntry
  { jsonResult :: A.Value
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

instance RRItem MultiExecEntry where
  getTag _ = "MultiExecEntry"

instance T.JSONEx a => MockedResult MultiExecEntry (T.TxResult a) where
  getMock MultiExecEntry {jsonResult} = case foo of
    Nothing -> Nothing
    Just (T.TxSuccess Nothing)  -> Nothing
    Just (T.TxSuccess (Just a)) -> Just $ T.TxSuccess a
    Just T.TxAborted            -> Just $ T.TxAborted
    Just (T.TxError s)          -> Just $ T.TxError s
    where
      foo :: Maybe (T.TxResult (Maybe a))
      foo = fmap (fmap (T.resolveJSONEx @a T.jsonDecode T.fromJSONMaybe)) $
        A.parseMaybe A.parseJSON1 jsonResult

mkMultiExecEntry :: forall a . T.JSONEx a => T.TxResult a -> MultiExecEntry
mkMultiExecEntry r = MultiExecEntry $
    A.toJSON1 $ fmap (T.resolveJSONEx @a T.jsonEncode toJSON) r

