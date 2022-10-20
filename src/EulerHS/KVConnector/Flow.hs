{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module EulerHS.KVConnector.Flow where

import           EulerHS.Prelude
import           EulerHS.KVConnector.Types (KVConnector, MeshConfig, MeshResult,MeshError(..), MeshMeta(..) ,tableName, getLookupKeyByPKey, getSecondaryLookupKeys)
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
  autoId <- getAutoIncId meshCfg (tableName value)
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
    FromJSON (table Identity)
  ) =>
  MeshConfig ->
  Where be table ->
  ReaderT r L.Flow (MeshResult (table Identity))
-- findWithKVConnector _ _ = pure $ Left $ MKeyNotFound "not found"
findWithKVConnector meshCfg whereClause = do
  let fieldsAndValues = getFieldsAndValuesFromClause meshModelTableEntityDescriptor [] (And whereClause)
      (_ , val) = head $ filter (\(a, _) -> a == primaryK) fieldsAndValues
  let key = "orderReference_" <> primaryK <> "_" <> val
  L.logWarning @Text "findWithKVConnector" key
  res <- L.runKVDB meshCfg.kvRedis $ L.get (fromString $ T.unpack key)
  case res of
    Right (Just r) ->
      case A.eitherDecode $ BSL.fromChunks [r] of
        Right r' -> return $ mapRight head $ decodeField @(table Identity) r'
        -- Decoding Error
        Left e   -> return $ Left $ MDecodingError $ T.pack e
    -- Not found
    Right Nothing -> do
      let traceMsg = "redis_fetch_noexist: Could not find hash: " <> key <> " while getting field"
      L.logWarning @Text "getCacheWithHash" traceMsg
      pure $ Left $ MKeyNotFound "not found"
    -- Redis error
    Left e -> return $ Left $ MRedisError e
  where
    primaryK = "id"

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

-- encodeClause ::
--   forall be table.
--   B.Beamable table =>
--   UpdateModel ->
--   B.DatabaseEntityDescriptor be (B.TableEntity table) ->
--   Clause be table ->
--   A.Object
-- encodeClause model dt w =
--   let foldWhere' = \case
--         And cs -> foldAnd cs
--         Or cs -> foldOr cs
--         Is column val -> foldIs column val
--       foldAnd = \case
--         [] -> HM.empty
--         [x] -> foldWhere' x
--         xs -> HM.singleton "$and" (A.toJSON $ map foldWhere' xs)
--       foldOr = \case
--         [] -> HM.empty
--         [x] -> foldWhere' x
--         xs -> HM.singleton "$or" (A.toJSON $ map foldWhere' xs)
--       foldIs :: A.ToJSON a => Column table value -> Term be a -> A.Object
--       foldIs column term =
--         let key =
--               B._fieldName . fromColumnar' . column . columnize $
--                 B.dbTableSettings dt
--          in HM.singleton key $ encodeTerm model key term
--    in foldWhere' w

-- encodeTerm :: A.ToJSON table => UpdateModel -> Text -> Term be table -> A.Value
-- encodeTerm model key = \case
--   In vals -> array "$in" (modifyToPsFormat <$> vals)
--   Eq val -> modifyToPsFormat val
--   Null -> A.Null
--   GreaterThan val -> single "$gt" (modifyToPsFormat val)
--   GreaterThanOrEq val -> single "$gte" (modifyToPsFormat val)
--   LessThan val -> single "$lt" (modifyToPsFormat val)
--   LessThanOrEq val -> single "$lte" (modifyToPsFormat val)
--   -- Like val -> single "$like" (modifyToPsFormat val)
--   -- Not (Like val) -> single "$notLike" (modifyToPsFormat val)
--   Not (In vals) -> array "$notIn" (modifyToPsFormat <$> vals)
--   Not (Eq val) -> single "$ne" (modifyToPsFormat val)
--   Not Null -> single "$ne" A.Null
--   Not term -> single "$not" (encodeTerm model key term)
--   _ -> error "Error while encoding - Term not supported"

--   where
--     modifyToPsFormat val = snd $ toPSJSON model (key, A.toJSON val)

-- array :: Text -> [A.Value] -> A.Value
-- array k vs = A.toJSON $ HM.singleton k vs

-- single :: Text -> A.Value -> A.Value
-- single k v = A.toJSON $ HM.singleton k v
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