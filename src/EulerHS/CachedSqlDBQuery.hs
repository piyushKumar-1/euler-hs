{-# LANGUAGE OverloadedStrings #-}
module EulerHS.CachedSqlDBQuery
  ( create
  , updateOne
  , findOne
  , findAll
  )
where

import EulerHS.Core.Types.DB
import qualified EulerHS.Core.SqlDB.Language as DB
import EulerHS.Extra.Language (rGetT, rSetB)
import qualified EulerHS.Framework.Language as L
import EulerHS.Prelude

import Data.Aeson (encode)
import qualified Data.ByteString.Lazy as BSL
import qualified Database.Beam as B
import qualified Data.Text as T
import Named ((!), defaults)
import Sequelize

-- TODO: Documentation
-- findAll and findOne can't use the same cache key
-- (findAll writes a `[a]` and findOne `Maybe a`

-- TODO: What KVDB should be used
cacheName :: String
cacheName = "eulerKVDB"

-- Core API

create ::
  forall be beM table.
  ( BeamRuntime be beM,
    BeamRunner beM,
    B.HasQBuilder be,
    Model be table,
    ToJSON (table Identity),
    FromJSON (table Identity)
  ) =>
  SqlConn beM ->
  table Identity ->
  Maybe Text ->
  L.Flow (DBResult (table Identity))
create dbConn value mCacheKey = do
  res <- L.runDB dbConn $
    DB.insertRowsReturningList $ sqlCreate value
  case res of
    Right [val] -> do
      whenJust mCacheKey (`cacheWithKey` val)
      return $ Right val
    Right _ -> error "Should return single value after insertion" -- TODO
    Left e -> return $ Left e

updateOne ::
  ( BeamRuntime be beM,
    BeamRunner beM,
    Model be table,
    ModelToSets be table,
    B.HasQBuilder be
  ) =>
  SqlConn beM ->
  Maybe Text ->
  table Identity ->
  Where be table ->
  L.Flow (DBResult ())
updateOne sqlConn (Just cacheKey) value whereClause = do
  val <- updateOneSql sqlConn value whereClause
  whenRight val (cacheWithKey cacheKey)
  return val
updateOne sqlConn Nothing value whereClause = updateOneSql sqlConn value whereClause

updateOneSql ::
  ( BeamRuntime be beM,
    BeamRunner beM,
    Model be table,
    ModelToSets be table,
    B.HasQBuilder be
  ) =>
  SqlConn beM ->
  table Identity ->
  Where be table ->
  L.Flow (DBResult ())
updateOneSql sqlConn value whereClause = do
  L.runDB sqlConn $ DB.updateRows $
    sqlUpdate ! #set (modelToSets value) ! #where_ whereClause

-- Note: Caches `Nothing` if it doesn't find anything
findOne ::
  ( BeamRuntime be beM,
    BeamRunner beM,
    Model be table,
    B.HasQBuilder be,
    ToJSON (table Identity),
    FromJSON (table Identity),
    Show (table Identity)
  ) =>
  SqlConn beM ->
  Maybe Text ->
  Where be table ->
  L.Flow (DBResult (Maybe (table Identity)))
findOne sqlConn (Just cacheKey) whereClause = do
  mRes <- rGetT (T.pack cacheName) cacheKey
  case join mRes of
    (Just res) -> return $ Right $ Just res
    Nothing -> do
      mDBRes <- findOneSql sqlConn whereClause
      whenRight mDBRes (cacheWithKey cacheKey)
      return mDBRes
findOne sqlConn Nothing whereClause = findOneSql sqlConn whereClause

findOneSql ::
  ( BeamRuntime be beM,
    BeamRunner beM,
    Model be table,
    B.HasQBuilder be,
    ToJSON (table Identity),
    FromJSON (table Identity)
  ) => 
  SqlConn beM ->
  Where be table ->
  L.Flow (DBResult (Maybe (table Identity)))
findOneSql sqlConn whereClause = L.runDB sqlConn findQuery
  where findQuery = DB.findRow (sqlSelect ! #where_ whereClause ! defaults)

findAll ::
  ( BeamRuntime be beM,
    BeamRunner beM,
    Model be table,
    B.HasQBuilder be,
    ToJSON (table Identity),
    FromJSON (table Identity),
    Show (table Identity)
  ) =>
  SqlConn beM ->
  Maybe Text ->
  Where be table ->
  L.Flow (DBResult [table Identity])
findAll sqlConn (Just cacheKey) whereClause = do
  mRes <- rGetT (T.pack cacheName) cacheKey
  case mRes of
    (Just res) -> return $ Right res
    Nothing -> do
      mDBRes <- findAllSql sqlConn whereClause
      whenRight mDBRes (cacheWithKey cacheKey)
      return mDBRes
findAll sqlConn Nothing whereClause = findAllSql sqlConn whereClause

findAllSql ::
  ( BeamRuntime be beM,
    BeamRunner beM,
    Model be table,
    B.HasQBuilder be,
    ToJSON (table Identity),
    FromJSON (table Identity)
  ) => 
  SqlConn beM ->
  Where be table ->
  L.Flow (DBResult [table Identity])
findAllSql sqlConn whereClause = L.runDB sqlConn findQuery
  where findQuery = DB.findRows (sqlSelect ! #where_ whereClause ! defaults)

------------ helper functions ------------

-- TODO: Maybe move this to sequelize?
sqlCreate ::
  forall be table.
  (B.HasQBuilder be, Model be table) =>
  table Identity ->
  B.SqlInsert be table
sqlCreate value = B.insert modelTableEntity (B.insertValues [value])

cacheWithKey :: (ToJSON table) => Text -> table -> L.Flow ()
cacheWithKey key row = do
  -- TODO: Should we log errors here? 
  void $ rSetB (T.pack cacheName) (encodeUtf8 key) (BSL.toStrict $ encode row)
