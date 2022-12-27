{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# OPTIONS_GHC -Wno-star-is-type #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE DerivingStrategies #-}

module EulerHS.KVConnector.Types where

import EulerHS.Prelude
import qualified Data.Aeson as A
import           Data.Aeson.Types (Parser)
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as Map
import           Data.Time (UTCTime)
import qualified EulerHS.Language as L
import qualified Database.Beam as B
import           Database.Beam.MySQL (MySQL)
import           Database.Beam.Backend (BeamSqlBackend, HasSqlValueSyntax (sqlValueSyntax), autoSqlValueSyntax)
import qualified Database.Beam.Backend.SQL as B
import           Database.Beam.Schema (FieldModification, TableField)
import           Sequelize (Column, Set)
import qualified EulerHS.Types as T
import           Data.Aeson ((.=))

------------ TYPES AND CLASSES ------------

data PrimaryKey = PKey [(Text,Text)]
data SecondaryKey = SKey [(Text,Text)]

class KVConnector table where
  tableName :: Text
  keyMap :: HM.HashMap Text Bool -- True implies it is primary key and False implies secondary
  primaryKey :: table -> PrimaryKey
  secondaryKeys:: table -> [SecondaryKey]

--------------- EXISTING DB MESH ---------------
class MeshState a where
  getShardedHashTag :: a -> Maybe Text
  getKVKey          :: a -> Maybe Text
  getKVDirtyKey     :: a -> Maybe Text
  isDBMeshEnabled   :: a -> Bool

class MeshMeta be table where
  meshModelFieldModification :: table (FieldModification (TableField table))
  valueMapper :: Map.Map Text (A.Value -> A.Value)
  parseFieldAndGetClause :: A.Value -> Text -> Parser (TermWrap be table)
  parseSetClause :: [(Text, A.Value)] -> Parser [Set be table]

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
  | UnexpectedError Text
  deriving (Show, Generic)

instance ToJSON MeshError where
  toJSON (MRedisError r) = A.object
    [
      "contents" A..= (show r :: Text),
      "tag" A..= ("MRedisError" :: Text)
    ]
  toJSON a = A.toJSON a

data QueryPath = KVPath | SQLPath

data MeshConfig = MeshConfig
  { meshEnabled     :: Bool
  , cerealEnabled   :: Bool
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
--   , meshDBName = "ECRDB"
--   , ecRedisDBStream = "db-sync-stream"
--   , kvRedis = "KVRedis"
--   , redisTtl = 43200
--   }

instance HasSqlValueSyntax MySQL String => HasSqlValueSyntax MySQL UTCTime where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend MySQL => B.HasSqlEqualityCheck MySQL UTCTime

instance HasSqlValueSyntax MySQL String => HasSqlValueSyntax MySQL A.Value where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend MySQL => B.HasSqlEqualityCheck MySQL A.Value

instance HasSqlValueSyntax MySQL String => HasSqlValueSyntax MySQL (Vector Int) where
  sqlValueSyntax = autoSqlValueSyntax

instance HasSqlValueSyntax MySQL String => HasSqlValueSyntax MySQL (Vector Text) where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend MySQL => B.HasSqlEqualityCheck MySQL (Vector Int)

instance BeamSqlBackend MySQL => B.HasSqlEqualityCheck MySQL (Vector Text)

data IsKVEnabled = IsKVEnabled
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance T.OptionEntity IsKVEnabled Bool

data KVCEnabledTables = KVCEnabledTables
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance T.OptionEntity KVCEnabledTables FeatureConfig

data MerchantID = MerchantID 
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)
instance T.OptionEntity MerchantID Text

data FeatureConfig = FeatureConfig -- TODO: Move this from here to euler-db
  { enableAll        :: Bool
  , disableAny       :: Maybe [Text]
  , enableAllRollout :: Maybe Int
  , enabledKeys      :: [RolloutConfig]
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

data RolloutConfig = RolloutConfig
  { name        :: Text
  , rollout    :: Int
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)


data DBLogEntry a = DBLogEntry
  { _log_type     :: Text
  , _action       :: Text
  , _data         :: a
  , _latency      :: Int
  , _model        :: Text
  , _cpuLatency   :: Integer
  , _source       :: Text
  , _apiTag       :: Maybe Text
  , _merchant_id  :: Maybe Text
  }
  deriving stock (Generic)
  -- deriving anyclass (ToJSON)
instance (ToJSON a) => ToJSON (DBLogEntry a) where
  toJSON val = A.object [ "log_type" .= _log_type val
                        , "action" .= _action val
                        , "latency" .= _latency val
                        , "model" .= _model val
                        , "cpuLatency" .= _cpuLatency val
                        , "data" .= _data val
                        , "source" .= _source val
                        , "api_tag" .= _apiTag val
                      ]
