{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module EulerHS.KVConnector.Types where

import Prelude
import qualified Data.Aeson as A
import           Data.Aeson.Types (Parser)
import           Data.List (sortBy)
import           Data.Function (on)
import           Data.Functor.Identity (Identity)
import qualified Data.Map.Strict as Map
import           Data.Text (Text)
import qualified EulerHS.Language as L
import           Database.Beam.Schema (FieldModification, TableField)
import           Database.Beam.MySQL (MySQL)
import           Sequelize (Clause, Set)
import qualified EulerHS.Types as T

------------ TYPES AND CLASSES ------------

data PrimaryKey = PKey [(Text,Text)]
data SecondaryKey = SKey [(Text,Text)]

class KVConnector table where
  tableName :: table -> Text
  primaryKey :: table -> PrimaryKey
  secondaryKeys:: table -> [SecondaryKey]



------------- UTILS EXPOSED ------------------

keyDelim:: Text
keyDelim = "_"

getLookupKeyByPKey :: forall table. (KVConnector (table Identity)) => table Identity -> Text
getLookupKeyByPKey table = do
  let tName = tableName table
  let (PKey k) = primaryKey table
  let lookupKey = getSortedKey k
  tName <> keyDelim <> lookupKey

getSecondaryLookupKeys :: forall table. (KVConnector (table Identity)) => table Identity -> [Text]
getSecondaryLookupKeys table = do
  let tName = tableName table
  let skeys = secondaryKeys table
  let tupList = Prelude.map (\(SKey s) -> s) skeys 
  let list = Prelude.map (\x -> tName <> (getSortedKey x) ) tupList
  list

------------- UTILS HIDDEN ------------------

getSortedKey :: [(Text,Text)] -> Text
getSortedKey kvTup = do
  let sortArr = sortBy (compare `on` fst) kvTup
  let flatTupArr = (Prelude.map (\(a,b) -> a <> keyDelim <> b ) sortArr)
  let flatArr = (Prelude.foldl (\b a -> b <> (if b == "" then "" else keyDelim ) <> a) "" flatTupArr)
  flatArr

--------------- EXISTING DB MESH ---------------
class MeshState a where
  getShardedHashTag :: a -> Maybe Text
  getKVKey          :: a -> Maybe Text
  getKVDirtyKey     :: a -> Maybe Text
  isDBMeshEnabled   :: a -> Bool

class MeshMeta table where
  meshModelFieldModification :: table (FieldModification (TableField table))
  valueMapper :: Map.Map Text (A.Value -> A.Value)
  parseFieldAndGetClause :: A.Value -> Text -> Parser (Clause MySQL table)
  parseSetClause :: [(Text, A.Value)] -> Parser [Set MySQL table]

type MeshResult a = Either MeshError a

data MeshError
  = MKeyNotFound Text
  | MDBError T.DBError
  | MRedisError T.KVDBReply
  | MDecodingError Text
  | MUpdateFailed Text
  | MMultipleKeysFound Text
  deriving Show

data QueryPath = KVPath | SQLPath

data MeshConfig = MeshConfig
  { meshEnabled     :: Bool
  , memcacheEnabled :: Bool
  , isTrackerTable  :: Text -> Bool
  , isConfigTable   :: Text -> Bool
  , meshDBName      :: Text
  , ecRedisDBStream :: Text
  , kvRedis         :: Text
  , redisTtl        :: L.KVDBDuration
  }

