{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module EulerHS.KVConnector.Flow where

import           EulerHS.Prelude
import           EulerHS.KVConnector.Types (KVConnector, MeshConfig, MeshResult,MeshError(..), MeshMeta(..) ,tableName, getLookupKeyByPKey, getSecondaryLookupKeys, applyFPair, )
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import           Data.List (head)
-- import qualified Database.Beam.MySQL as BM
import qualified EulerHS.Language as L
import qualified Data.HashMap.Strict as HM
-- import           Sequelize (Model)
import           EulerHS.SqlDB.Types (BeamRunner, BeamRuntime)
import           Sequelize (fromColumnar', modelTableName, columnize, Model, Where, Clause(..), Term(..))
import qualified Database.Beam as B
import qualified Database.Beam.Schema.Tables as B
import           Data.Either.Extra (mapRight, mapLeft)



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
  autoId <- getAutoIncId meshCfg (tableName @(table Identity))
  L.logWarning @Text "createWithKVConnector" (show autoId)
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


---------------- find -----------------------
findWithKVConnector :: forall be table beM r.
  ( HasCallStack,
    BeamRuntime be beM,
    BeamRunner beM,
    Model be table,
    MeshMeta table,
    B.HasQBuilder be,
    KVConnector (table Identity),
    FromJSON (table Identity)
  ) =>
  MeshConfig ->
  Where be table ->
  ReaderT r L.Flow (MeshResult (table Identity))
findWithKVConnector meshCfg whereClause = do
  let fieldsAndValues = sort $ getFieldsAndValuesFromClause meshModelTableEntityDescriptor [] (And whereClause)
      modelName = tableName @(table Identity)
      keyAndValueCombinations = (\(a, b) -> zip a b) $ applyFPair (\x -> map (T.intercalate "_") (nonEmptySubsequences x)) $ unzip fieldsAndValues
  L.logDebugT "findWithKVConnector" (show keyAndValueCombinations)
  keyRes <- getPrimaryKeyFromFieldsAndValues modelName keyAndValueCombinations meshCfg   -- if length is 1 it's a primary key. TODO: This should be implemented with a proper logic
  L.logDebugT "findWithKVConnector" (show keyRes)

  case keyRes of
    Right (Just key) -> do
      res <- L.runKVDB meshCfg.kvRedis $ L.get (fromString $ T.unpack key)
      case res of
        Right (Just r) ->
          case A.eitherDecode $ BSL.fromChunks [r] of
            Right r' -> return $ mapRight head $ decodeField @(table Identity) r'
            -- Decoding Error
            Left e   -> return $ Left $ MDecodingError $ T.pack e
        -- Not found
        Right Nothing -> do
          let traceMsg = "redis_fetch_noexist: Could not find key: " <> key
          L.logWarningT "getCacheWithHash" traceMsg
          pure $ Left $ MKeyNotFound "not found"
        -- Redis error
        Left e -> return $ Left $ MRedisError e
    Right Nothing -> error "Fallback to MySQL and re cache"
    Left err -> pure $ Left err

decodeField :: forall a. (FromJSON a) => A.Value -> MeshResult [a]
decodeField o@(A.Object _) =
  case A.eitherDecode @a $ A.encode o of
    Right r -> return [r]
    Left e  -> Left $ MDecodingError $ T.pack e
decodeField o@(A.Array _) =
  mapLeft (MDecodingError . T.pack)
    $ A.eitherDecode @[a] $ A.encode o
decodeField o = Left $ MDecodingError
  ("Expected list or object but got '" <> T.pack (show o) <> "'.")

meshModelTableEntityDescriptor ::
  forall table be.
  (Model be table, MeshMeta table) =>
  B.DatabaseEntityDescriptor be (B.TableEntity table)
meshModelTableEntityDescriptor = let B.DatabaseEntity x = (meshModelTableEntity @table) in x

meshModelTableEntity ::
  forall table be db.
  (Model be table, MeshMeta table) =>
  B.DatabaseEntity be db (B.TableEntity table)
meshModelTableEntity =
  let B.EntityModification modification = B.modifyTableFields (meshModelFieldModification @table)
  in appEndo modification $ B.DatabaseEntity $ B.dbEntityAuto (modelTableName @table)

getFieldsAndValuesFromClause :: forall table be. (Model be table, MeshMeta table) =>
  B.DatabaseEntityDescriptor be (B.TableEntity table) -> [(Text, Text)] -> Clause be table -> [(Text, Text)]
getFieldsAndValuesFromClause dt res = \case
  And cs -> concat $ map (getFieldsAndValuesFromClause dt res) cs
  Or cs -> concat $ map (getFieldsAndValuesFromClause dt res) cs 
  Is column (Eq val) -> do
    let key = B._fieldName . fromColumnar' . column . columnize $ B.dbTableSettings dt
    (key, show val) : res
  _ -> res

getPrimaryKeyFromFieldsAndValues :: Text -> [(Text, Text)] -> MeshConfig -> ReaderT r L.Flow (MeshResult (Maybe Text))
getPrimaryKeyFromFieldsAndValues modelName fieldsAndValues meshCfg = do
  if length fieldsAndValues == 1
    then do
      let (key, val) = head fieldsAndValues
      pure $ Right $ Just $ contructKey key val
    else do
      getPKeyFromSkeys fieldsAndValues
  where
    getPKeyFromSkeys [] = pure $ Right Nothing
    getPKeyFromSkeys ((k, v) : xs) = do
        let sKey = contructKey k v
        res <- L.runKVDB meshCfg.kvRedis $ L.get (fromString $ T.unpack sKey)
        case res of
          Right (Just r) -> pure $ Right (Just $ decodeUtf8 r)
          -- Not found
          Right Nothing -> do
            let traceMsg = "redis_fetch_noexist: Could not find " <> sKey
            L.logWarningT "getPKeyFromSkeys" traceMsg
            getPKeyFromSkeys xs
          -- Redis error
          Left e -> return $ Left $ MRedisError e
    contructKey k v = modelName <> "_" <> k <> "_" <> v

-- >>> map (foldl (<>) "") (nonEmptySubsequences ["id", "id2", "id3"])
-- ["id","id2","id_id2","id3","id_id3","id2_id3","id_id2_id3"]
nonEmptySubsequences         :: [Text] -> [[Text]]
nonEmptySubsequences []      =  []
nonEmptySubsequences (x:xs)  =  [x]: foldr f [] (nonEmptySubsequences xs)
  where f ys r = ys : (x : ys) : r

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