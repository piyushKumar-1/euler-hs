{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module EulerHS.KVConnector.DBSync where

import           EulerHS.Prelude
import           EulerHS.KVConnector.Types (MeshMeta(..))
import qualified Data.Aeson as A
import           Data.Aeson ((.=))
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Database.Beam as B
import qualified Database.Beam.Schema.Tables as B
import           Sequelize (Model, Where, Clause(..), Term(..), Column, fromColumnar', columnize, modelTableName)
import           Text.Casing (pascal)


-- For storing DBCommands in stream

type Tag = Text

type DBName = Text

data DBCommandVersion = V1
  deriving (Generic, Show, ToJSON, FromJSON)

getCreateQuery :: (ToJSON (table Identity)) => Text -> DBCommandVersion -> Tag -> Double -> DBName -> table Identity -> A.Value
getCreateQuery model cmdVersion tag timestamp dbName dbObject = A.object
    [ "contents" .= A.toJSON
        [ A.toJSON cmdVersion
        , A.toJSON tag
        , A.toJSON timestamp
        , A.toJSON dbName
        , A.object
            [ "contents" .= dbObject,
              "tag" .= ((T.pack . pascal . T.unpack) model <> "Object")
            ]
        ]
    , "tag" .= ("Create" :: Text)
    ]
-- | This will take updateCommand from getDbUpdateCommandJson and returns Aeson value of Update DBCommand
getUpdateQuery :: DBCommandVersion -> Tag -> Double -> DBName -> A.Value -> A.Value
getUpdateQuery cmdVersion tag timestamp dbName updateCommand = A.object
    [ "contents" .= A.toJSON
        [ A.toJSON cmdVersion
        , A.toJSON tag
        , A.toJSON timestamp
        , A.toJSON dbName
        , updateCommand
        ]
    , "tag" .= ("Update" :: Text)
    ]

getDbUpdateCommandJson :: forall be table. (Model be table, MeshMeta table) => Text -> [(Text, A.Value)] -> Where be table -> A.Value
getDbUpdateCommandJson model upd whereClause = A.object
  [ "contents" .= A.toJSON
      [ updValToJSON . (toPSJSON @table) <$> upd
      , [whereClauseToJson whereClause]
      ]
  , "tag" .= ((T.pack . pascal . T.unpack) model <> "Options")
  ]

updValToJSON :: (Text, A.Value) -> A.Value
updValToJSON (k, v) = A.object [ "value0" .= k, "value1" .= v ]

whereClauseToJson :: (Model be table, MeshMeta table) => Where be table -> A.Value
whereClauseToJson whereClause = A.object
    [ ("value0" :: Text) .= ("where" :: Text)
    , "value1" .= modelEncodeWhere whereClause
    ]

modelEncodeWhere ::
  forall be table.
  (Model be table, MeshMeta table) =>
  Where be table ->
  A.Object
modelEncodeWhere = encodeWhere meshModelTableEntityDescriptor

encodeWhere ::
  forall be table.
  (B.Beamable table, MeshMeta table) =>
  B.DatabaseEntityDescriptor be (B.TableEntity table) ->
  Where be table ->
  A.Object
encodeWhere dt = encodeClause dt . And

encodeClause ::
  forall be table.
  (B.Beamable table, MeshMeta table) =>
  B.DatabaseEntityDescriptor be (B.TableEntity table) ->
  Clause be table ->
  A.Object
encodeClause dt w =
  let foldWhere' = \case
        And cs -> foldAnd cs
        Or cs -> foldOr cs
        Is column val -> foldIs column val
      foldAnd = \case
        [] -> HM.empty
        [x] -> foldWhere' x
        xs -> HM.singleton "$and" (A.toJSON $ map foldWhere' xs)
      foldOr = \case
        [] -> HM.empty
        [x] -> foldWhere' x
        xs -> HM.singleton "$or" (A.toJSON $ map foldWhere' xs)
      foldIs :: A.ToJSON a => Column table value -> Term be a -> A.Object
      foldIs column term =
        let key =
              B._fieldName . fromColumnar' . column . columnize $
                B.dbTableSettings dt
         in HM.singleton key $ (encodeTerm @table) key term
   in foldWhere' w

encodeTerm :: forall table be value. (A.ToJSON value, MeshMeta table) => Text -> Term be value -> A.Value
encodeTerm key = \case
  In vals -> array "$in" (modifyToPsFormat <$> vals)
  Eq val -> modifyToPsFormat val
  Null -> A.Null
  GreaterThan val -> single "$gt" (modifyToPsFormat val)
  GreaterThanOrEq val -> single "$gte" (modifyToPsFormat val)
  LessThan val -> single "$lt" (modifyToPsFormat val)
  LessThanOrEq val -> single "$lte" (modifyToPsFormat val)
  -- Like val -> single "$like" (modifyToPsFormat val)
  -- Not (Like val) -> single "$notLike" (modifyToPsFormat val)
  Not (In vals) -> array "$notIn" (modifyToPsFormat <$> vals)
  Not (Eq val) -> single "$ne" (modifyToPsFormat val)
  Not Null -> single "$ne" A.Null
  Not term -> single "$not" ((encodeTerm @table) key term)
  _ -> error "Error while encoding - Term not supported"

  where
    modifyToPsFormat val = snd $ (toPSJSON @table) (key, A.toJSON val)

toPSJSON :: forall table. MeshMeta table => (Text, A.Value) -> (Text, A.Value)
toPSJSON (k, v) = (k, Map.findWithDefault id k (valueMapper @table) v)

array :: Text -> [A.Value] -> A.Value
array k vs = A.toJSON $ HM.singleton k vs

single :: Text -> A.Value -> A.Value
single k v = A.toJSON $ HM.singleton k v

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