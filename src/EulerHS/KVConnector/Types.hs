{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# OPTIONS_GHC -Wno-star-is-type #-}

module EulerHS.KVConnector.Types where

import Prelude
import qualified Data.Aeson as A
import           Data.Aeson.Types (Parser)
import           Data.List (sortBy)
import           Data.Function (on)
import           Data.Functor.Identity (Identity)
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as Map
import           Data.Text (Text)
import qualified Data.Text as T
import qualified EulerHS.Language as L
import qualified Database.Beam as B
import qualified Database.Beam.Backend.SQL as B
import           Database.Beam.Schema (FieldModification, TableField)
import           Database.Beam.MySQL (MySQL)
import           GHC.Generics (Generic)
import           Sequelize (Column, Set)
import qualified EulerHS.Types as T

------------ TYPES AND CLASSES ------------

data PrimaryKey = PKey [(Text,Text)]
data SecondaryKey = SKey [(Text,Text)]

class KVConnector table where
  tableName :: Text
  keyMap :: HM.HashMap Text Bool -- True implies it is primary key and False implies secondary
  primaryKey :: table -> PrimaryKey
  secondaryKeys:: table -> [SecondaryKey]



------------- UTILS EXPOSED ------------------

keyDelim:: Text
keyDelim = "_"

getLookupKeyByPKey :: forall table. (KVConnector (table Identity)) => table Identity -> Text
getLookupKeyByPKey table = do
  let tName = tableName @(table Identity)
  let (PKey k) = primaryKey table
  let lookupKey = getSortedKey k
  tName <> keyDelim <> lookupKey

getSecondaryLookupKeys :: forall table. (KVConnector (table Identity)) => table Identity -> [Text]
getSecondaryLookupKeys table = do
  let tName = tableName @(table Identity)
  let skeys = secondaryKeys table
  let tupList = Prelude.map (\(SKey s) -> s) skeys
  let list = Prelude.map (\x -> tName <> keyDelim <> getSortedKey x ) tupList
  list

applyFPair :: (t -> b) -> (t, t) -> (b, b)
applyFPair f (x, y) = (f x, f y)

------------- UTILS HIDDEN ------------------

getSortedKey :: [(Text,Text)] -> Text
getSortedKey kvTup = do
  let sortArr = sortBy (compare `on` fst) kvTup
  let (appendedKeys, appendedValues) = applyFPair (T.intercalate "_") $ unzip sortArr
  appendedKeys <> "_" <> appendedValues

--------------- EXISTING DB MESH ---------------
class MeshState a where
  getShardedHashTag :: a -> Maybe Text
  getKVKey          :: a -> Maybe Text
  getKVDirtyKey     :: a -> Maybe Text
  isDBMeshEnabled   :: a -> Bool

class MeshMeta table where
  meshModelFieldModification :: table (FieldModification (TableField table))
  valueMapper :: Map.Map Text (A.Value -> A.Value)
  parseFieldAndGetClause :: A.Value -> Text -> Parser (TermWrap MySQL table)
  parseSetClause :: [(Text, A.Value)] -> Parser [Set MySQL table]

data TermWrap be (table :: (* -> *) -> *) where
  TermWrap :: (B.BeamSqlBackendCanSerialize be a, A.ToJSON a, Ord a, B.HasSqlEqualityCheck be a, Show a) 
              => Column table a -> a -> TermWrap be table

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
  , meshDBName      :: Text
  , ecRedisDBStream :: Text
  , kvRedis         :: Text
  , redisTtl        :: L.KVDBDuration
  }
  deriving (Generic, Eq, Show, A.ToJSON)

-- meshConfig :: MeshConfig
-- meshConfig = MeshConfig
--   { meshEnabled = True
--   , memcacheEnabled = False
--   , isTrackerTable = const True
--   , isConfigTable = const False
--   , meshDBName = "ECRDB"
--   , ecRedisDBStream = "db-sync-stream"
--   , kvRedis = "KVRedis"
--   , redisTtl = 43200
--   }