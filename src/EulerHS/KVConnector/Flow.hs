{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module EulerHS.KVConnector.Flow where

import           EulerHS.Prelude
import           EulerHS.KVConnector.Types (KVConnector, MeshConfig, MeshResult,MeshError(..), tableName, getLookupKeyByPKey, getSecondaryLookupKeys)
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
-- import qualified Database.Beam.MySQL as BM
import qualified EulerHS.Language as L
import qualified Data.HashMap.Strict as HM
-- import           Sequelize (Model)

createWithKVConnector ::
  forall (table :: (Type -> Type) -> Type) r.
  ( HasCallStack,
    -- Model BM.MySQL table,
    FromJSON (table Identity),
    ToJSON (table Identity),
    -- Show (table Identity),
    KVConnector (table Identity)
  ) =>
  MeshConfig ->
  table Identity ->
  Maybe Text ->
  ReaderT r L.Flow (MeshResult (table Identity))
createWithKVConnector meshCfg value _ = do
  autoId <- getAutoIncId meshCfg (tableName value)
  case autoId of
    Right _id -> do
      -- TODO: Key - id is hardcoded to replace AutoIncrId. Make it Generic
      let val = unsafeJSONSet @Text "id" (T.pack . show $ _id) value
      let pKey = fromString . T.unpack $  getLookupKeyByPKey val
      _ <- L.runKVDB meshCfg.kvRedis $ L.set pKey (BSL.toStrict $ A.encode val)
      _ <- mapM (\secIdx -> do
        let sKey = fromString . T.unpack $ secIdx
        L.runKVDB meshCfg.kvRedis $ L.set sKey pKey) $ (getSecondaryLookupKeys val)
      return $ Right val
    Left err -> return $ Left err


---------------- UTILS -----------------------

getAutoIncId :: MeshConfig -> Text -> ReaderT r L.Flow (MeshResult Integer)
getAutoIncId meshCfg tName = do
  let key = tName <> "_auto_increment_id"
  mId <- L.runKVDB meshCfg.kvRedis $ L.incr $ encodeUtf8 key
  case mId of
    Right id_ -> return $ Right id_
    Left e    -> return $ Left $ MRedisError e

unsafeJSONSet :: forall a b. (ToJSON a, FromJSON b, ToJSON b) => Text -> a -> b -> b
unsafeJSONSet field value obj =
  case A.toJSON obj of
    A.Object o ->
      let jsonVal = A.toJSON value
          newObj =  A.Object (HM.insert field jsonVal o)
      in case resultToEither $ A.fromJSON newObj of
        Right r -> r
        Left e  -> error $ e
    _ -> error "Can't set  value of JSON which isn't a object."

resultToEither :: A.Result a -> Either Text a
resultToEither (A.Success res) = Right res
resultToEither (A.Error e)     = Left $ T.pack e