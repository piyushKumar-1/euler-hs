{-# LANGUAGE OverloadedStrings #-}
module EulerHS.Core.Caching.DB
  ( findOne
  )
where

import EulerHS.Core.Types.DB
import qualified EulerHS.Core.SqlDB.Language as DB
import EulerHS.Extra.Language (getOrInitSqlConn, rGetT, rSetB)
import qualified EulerHS.Framework.Language as L
import EulerHS.Prelude

import Data.Aeson (ToJSON, FromJSON, encode, fromJSON, toJSON)
import qualified Data.ByteString.Lazy as BSL
import qualified Database.Beam as B
import qualified Data.Text as T
import Named ((!), defaults)
import Sequelize

    {-
create :: (ToJSON a, FromJSON a, Model be table) => table a -> Maybe Text -> a -> Flow (table a)
create dbTable cacheKey value = do
  return undefined

updateOne :: (ToJSON a, FromJSON a) => Model a table -> Maybe Text -> a -> Where a table -> Flow (Maybe a)
updateOne dbTable cacheKey value whereClause = do
  return undefined
  -}

findOne ::
  ( BeamRuntime be beM,
    BeamRunner beM,
    Model be table,
    B.HasQBuilder be,
    ToJSON (table Identity),
    FromJSON (table Identity),
    Show (table Identity)
  ) =>
  DBConfig beM ->
  Maybe Text ->
  Where be table ->
  L.Flow (Maybe (table Identity))
findOne dbConf (Just cacheKey) whereClause = do
  -- TODO: What should the KVDB name be here?
  mRes <- rGetT (T.pack "redis") cacheKey
  case mRes of
    (Just res) -> return $ Just res
    Nothing -> do
      mDBRes <- findOneSql dbConf whereClause
      whenJust mDBRes (cacheWithKey cacheKey)
      return mDBRes
findOne dbConf Nothing whereClause = findOneSql dbConf whereClause

cacheWithKey :: (ToJSON row) => Text -> row -> L.Flow ()
cacheWithKey key row = do
  -- TODO: Which KVDB should be used here?
  -- TODO: Should we log errors here? 
  void $ rSetB (T.pack "redis") (encodeUtf8 key) (BSL.toStrict $ encode row)

findOneSql ::
  ( BeamRuntime be beM,
    BeamRunner beM,
    Model be table,
    B.HasQBuilder be,
    ToJSON (table Identity),
    FromJSON (table Identity)
  ) => 
  DBConfig beM ->
  Where be table ->
  L.Flow (Maybe (table Identity))
findOneSql dbConf whereClause = runDBInFlow dbConf findQuery
  where findQuery = DB.findRow (sqlSelect ! #where_ whereClause ! defaults)

runDBInFlow ::
  ( BeamRuntime be beM,
    BeamRunner beM,
    ToJSON (table Identity),
    FromJSON (table Identity)
  ) => 
  DBConfig beM ->
  DB.SqlDB beM (Maybe (table Identity)) ->
  L.Flow (Maybe (table Identity))
runDBInFlow dbConf db = do
  -- TODO: How do I get a DBConfig from the DBName?
  -- Do I need to lookup the DB name to know which backend to use?
  mConn <- getOrInitSqlConn dbConf
  case mConn of
    Right conn -> do
      dbRes <- L.runDB conn db
      case dbRes of
        Right res -> return res
        Left err -> return Nothing -- TODO
    Left _ -> return Nothing -- TODO: Maybe log the error?

    {-
findAll :: (ToJSON a, FromJSON a) => Model a table -> Maybe Text -> Where a table -> Flow [a]
findAll dbTable cacheKey whereClause = do
  return undefined
  -}
