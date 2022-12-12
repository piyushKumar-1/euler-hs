module KV.TestSchema.Mesh where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import           Data.Text (Text)
import           Prelude
import           EulerHS.KVConnector.Types (MeshConfig(..))
import           System.Environment (getEnvironment)
import           System.IO.Unsafe (unsafePerformIO)
import           Text.Read (readMaybe)

meshConfig :: MeshConfig
meshConfig = MeshConfig
  { meshEnabled = dbMeshEnabledEnvVar
  , memcacheEnabled = memCacheEnabledEnvVar
  , cerealEnabled = cerealEnabledEnvVar
  , meshDBName = "ECRDB"
  , ecRedisDBStream = "db-sync-stream"
  , kvRedis = "KVRedis"
  , redisTtl = 43200
  }

dbMeshTrackerTables :: Set.Set Text
dbMeshTrackerTables = Set.fromList ["txn_detail", "order_reference", "temp_card", "order_metadata_v2", "order_address"
  , "second_factor", "txn_card_info", "payment_gateway_response"]

dbMeshConfigTables :: Set.Set Text
dbMeshConfigTables = Set.fromList ["service_configuration" , "merchant_account", "merchant_iframe_preferences","merchant_gateway_account", "feature","reseller_account", "merchant_key", "ingress_rule"]

dbMeshEnabledEnvVar :: Bool
dbMeshEnabledEnvVar = fromMaybe True $ readMaybe =<< lookupEnv "DB_MESH_ENABLED"

memCacheEnabledEnvVar :: Bool
memCacheEnabledEnvVar = fromMaybe False $ readMaybe =<< lookupEnv "MEM_CACHE_ENABLED"

cerealEnabledEnvVar :: Bool
cerealEnabledEnvVar = fromMaybe False $ readMaybe =<< lookupEnv "CEREAL_ENABLED"

{-# NOINLINE environmentVars #-}
environmentVars :: Map String String
environmentVars = Map.fromList $ unsafePerformIO getEnvironment

lookupEnv :: String -> Maybe String
lookupEnv k = Map.lookup k environmentVars