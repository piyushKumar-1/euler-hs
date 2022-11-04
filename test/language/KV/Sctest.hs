{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS -fno-warn-unused-imports #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module KV.Sctest where


-- import           Data.Int (Int64)
import qualified Database.Beam as B
import           EulerHS.Prelude hiding (id)
import           Sequelize (ModelMeta (modelFieldModification, modelTableName))
import qualified EulerHS.Language as L
import           EulerHS.CachedSqlDBQuery (findOne')
-- import           Sequelize (Clause (..), Term (Eq))
import           Sequelize (ModelMeta (modelFieldModification, modelTableName, mkExprWithDefault), Clause(..), Term(..),Set (..))
import           KV.FlowHelper
import           KV.ThUtils
import           EulerHS.KVConnector.Types (KVConnector(..), MeshMeta(..), PrimaryKey(..), SecondaryKey(..), TermWrap(..), MeshMeta, tableName, primaryKey, secondaryKeys)
import           KV.Mesh
import qualified Data.Aeson as A
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as Text
import           Data.Scientific (floatingOrInteger)
import Data.Maybe (fromJust)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL

data ServiceConfigurationT f = ServiceConfiguration
  { id      :: B.C f Int64
  , version :: B.C f Int64
  , name    :: B.C f Text
  , value   :: B.C f (Maybe Text)
  }
  deriving stock (Generic)
  deriving anyclass (B.Beamable)

instance B.Table ServiceConfigurationT where
  data PrimaryKey ServiceConfigurationT f =
    ServiceConfigurationId (B.C f Int64)
      deriving stock (Generic)
      deriving anyclass (B.Beamable)
  primaryKey = ServiceConfigurationId . id

instance ModelMeta ServiceConfigurationT where
  modelFieldModification = serviceConfigurationTMod
  modelTableName = "service_configuration"
  mkExprWithDefault t = B.insertExpressions [(B.val_ t){ id = B.default_ }]

serviceConfigurationTModMesh :: ServiceConfigurationT (B.FieldModification (B.TableField ServiceConfigurationT))
serviceConfigurationTModMesh = B.tableModification
     { id = B.fieldNamed "id"
     , version = B.fieldNamed "version"
     , name = B.fieldNamed "name"
     , value = B.fieldNamed "value"
     }

type ServiceConfiguration = ServiceConfigurationT Identity
type ServiceConfigurationId = B.PrimaryKey ServiceConfigurationT Identity

deriving stock instance Show ServiceConfiguration
deriving stock instance Eq ServiceConfiguration
-- deriving anyclass instance ToJSON ServiceConfiguration
-- deriving anyclass instance FromJSON ServiceConfiguration
deriving stock instance Read ServiceConfiguration
deriving stock instance Ord ServiceConfiguration

toHSJSONServiceConfiguration :: A.Value -> A.Value
toHSJSONServiceConfiguration (A.Object hm) = A.Object (modifyField `HM.mapWithKey` hm)
  where modifyField = (\k -> M.findWithDefault _id k serviceConfigurationToHSModifiers )
        _id = \x -> x
toHSJSONServiceConfiguration _ = error "expected ServiceConfiguration to be a object"

instance FromJSON ServiceConfiguration where
  parseJSON = A.genericParseJSON aesonDBMeshOptions . toHSJSONServiceConfiguration


aesonDBMeshOptions :: A.Options
aesonDBMeshOptions = A.defaultOptions
  { A.omitNothingFields = True
  }


-- instance ToJSON ServiceConfiguration where
--   toJSON = toPSJSONOrderReference . A.genericToJSON aesonDBMeshOptions

serviceConfigurationEMod :: B.EntityModification
  (B.DatabaseEntity be db) be (B.TableEntity ServiceConfigurationT)
serviceConfigurationEMod = B.modifyTableFields serviceConfigurationTMod

serviceConfigurationTMod :: ServiceConfigurationT (B.FieldModification (B.TableField ServiceConfigurationT))
serviceConfigurationTMod = B.tableModification
    { id = B.fieldNamed "id"
    , version = B.fieldNamed "version"
    , name = B.fieldNamed "name"
    , value = B.fieldNamed "value"
    }


findServiceConfig :: HasCallStack => Int64 -> L.Flow (Maybe ServiceConfiguration)
findServiceConfig txnDetail_id = do
  dbConf <- getEulerDbConf
  res <- findOne' dbConf meshConfig Nothing [
           Is id (Eq $ txnDetail_id)]
  case res of
    Right (Just a) -> pure $ Just a
    Right (Nothing) -> L.logInfoT "DB_TEST" "NOT_FOUDN" $> Nothing
    Left err -> L.logInfoT "DB_TEST" (show err) $> Nothing


hush :: Either a b -> Maybe b
hush (Left _) = Nothing
hush (Right r) = Just r


serviceConfigurationToPSModifiers :: Map Text (A.Value -> A.Value)
serviceConfigurationToPSModifiers = M.fromList
   [
     ("id", jsonNumberToString)
   ]
 
 -- | Postprocess serialized ServiceConfiguration JSON to be compatible with Purescript.
toPSJSONServiceConfiguration :: A.Value -> A.Value
toPSJSONServiceConfiguration (A.Object hm) = A.Object (modifyField `HM.mapWithKey` hm)
  where modifyField = (\k -> M.findWithDefault _id k serviceConfigurationToPSModifiers )
        _id = \x -> x
toPSJSONServiceConfiguration _ = error "expected ServiceConfiguration to be a object"
 
instance ToJSON ServiceConfiguration where
   toJSON = toPSJSONServiceConfiguration . A.genericToJSON aesonDBMeshOptions
 
serviceConfigurationToHSModifiers :: Map Text (A.Value -> A.Value)
serviceConfigurationToHSModifiers = M.fromList
   [
     ("id", jsonStringToNumber)
   ]
 
psToHs :: HM.HashMap Text Text
psToHs = HM.empty

datePSDateToHSLocal :: Text -> Text
datePSDateToHSLocal = Text.init

updateJSONString :: (Text -> Text) -> A.Value -> A.Value
updateJSONString f (A.String t) = A.String $ f t
updateJSONString _ A.Null = A.Null
updateJSONString _ _ = error "updateJSONString: expected a JSON String"


jsonNumberToString :: A.Value -> A.Value
jsonNumberToString = \case
  -- TODO should be @Int64 I suppose
  (A.Number n) -> case (floatingOrInteger @Double @Int n) of
    Right i -> A.toJSON $ (show i :: Text)
    Left _ -> error "not an integral"
  A.Null -> A.Null
  e -> error $ "jsonNumberToString: not a number" <> (show e)

jsonStringToNumber :: A.Value -> A.Value
jsonStringToNumber = \case
  (A.String n) -> A.toJSON $ fromJust (A.decode $ toLazy $ encodeUtf8 n :: Maybe Int)
  A.Null -> A.Null
  e -> e

dateHSLocalToPSDate :: Text -> Text
dateHSLocalToPSDate t =
  let
    (date, tTail) = Text.span (/='T') t
    (time, _) = Text.span (/='.') $ Text.tail $ tTail
  in date `Text.snoc` 'T' <> time `Text.snoc` 'Z'

toLazy :: BS.ByteString -> BSL.ByteString
toLazy s = BSL.fromChunks [s]

$(meshMetaInstancesD ''ServiceConfigurationT)
$(kvConnectorInstancesD ''ServiceConfiguration ['id] [['name]])

