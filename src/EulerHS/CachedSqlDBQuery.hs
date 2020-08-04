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
import EulerHS.Core.Types.Serializable
import EulerHS.Extra.Language (rGetT, rSetB, getOrInitSqlConn)
import qualified EulerHS.Framework.Language as L
import EulerHS.Prelude

import Data.Aeson (encode)
import qualified Data.ByteString.Lazy as BSL
import qualified Database.Beam as B
import qualified Data.Text as T
import Named ((!), defaults)
import Sequelize

-- TODO: What KVDB should be used
cacheName :: String
cacheName = "eulerKVDB"

--------------- Core API ---------------

-- | Create a new database entry with the given value.
--   Cache the value if the DB insert succeeds.
create ::
  forall be beM table.
  ( BeamRuntime be beM,
    BeamRunner beM,
    B.HasQBuilder be,
    Model be table,
    ToJSON (table Identity),
    FromJSON (table Identity),
    Show (table Identity)
  ) =>
  DBConfig beM ->
  table Identity ->
  Maybe Text ->
  L.Flow (DBResult (table Identity))
create dbConf value mCacheKey = do
  res <- runQuery dbConf $
    DB.insertRowsReturningList $ sqlCreate value
  case res of
    Right [val] -> do
      whenJust mCacheKey (`cacheWithKey` val)
      return $ Right val
    Right xs -> do
      let message = "DB returned \"" <> show xs <> "\" after inserting \"" <> show value <> "\""
      L.logError @Text "create" message
      return $ Left $ DBError UnexpectedResult message
    Left e -> return $ Left e

-- | Update an element matching the query to the new value.
--   Cache the value at the given key if the DB update succeeds.
updateOne ::
  ( BeamRuntime be beM,
    BeamRunner beM,
    Model be table,
    ModelToSets be table,
    B.HasQBuilder be,
    ToJSON (table Identity)
  ) =>
  DBConfig beM ->
  Maybe Text ->
  table Identity ->
  Where be table ->
  L.Flow (DBResult ())
updateOne dbConf (Just cacheKey) value whereClause = do
  val <- updateOneSql dbConf value whereClause
  whenRight val (\_ -> cacheWithKey cacheKey value)
  return val
updateOne dbConf Nothing value whereClause = updateOneSql dbConf value whereClause

updateOneSql ::
  ( BeamRuntime be beM,
    BeamRunner beM,
    Model be table,
    ModelToSets be table,
    B.HasQBuilder be
  ) =>
  DBConfig beM ->
  table Identity ->
  Where be table ->
  L.Flow (DBResult ())
updateOneSql dbConf value whereClause = runQuery dbConf query
    where
      query = DB.updateRows
        $ sqlUpdate
        ! #set (modelToSets value)
        ! #where_ whereClause

-- | Find an element matching the query. Only uses the DB if the cache is empty.
--   Caches the result using the given key.
findOne ::
  ( BeamRuntime be beM,
    BeamRunner beM,
    Model be table,
    B.HasQBuilder be,
    ToJSON (table Identity),
    FromJSON (table Identity)
  ) =>
  DBConfig beM ->
  Maybe Text ->
  Where be table ->
  L.Flow (DBResult (Maybe (table Identity)))
findOne dbConf (Just cacheKey) whereClause = do
  mRes <- rGetT (T.pack cacheName) cacheKey
  case join mRes of
    (Just res) -> return $ Right $ Just res
    Nothing -> do
      mDBRes <- findOneSql dbConf whereClause
      whenRight mDBRes (cacheWithKey cacheKey)
      return mDBRes
findOne dbConf Nothing whereClause = findOneSql dbConf whereClause

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
  L.Flow (DBResult (Maybe (table Identity)))
findOneSql dbConf whereClause = runQuery dbConf findQuery
  where findQuery = DB.findRow (sqlSelect ! #where_ whereClause ! defaults)

-- | Find all elements matching the query. Only uses the DB if the cache is empty.
--   Caches the result using the given key.
--   NOTE: Can't use the same key as findOne, updateOne or create since it's result is a list.
findAll ::
  ( BeamRuntime be beM,
    BeamRunner beM,
    Model be table,
    B.HasQBuilder be,
    ToJSON (table Identity),
    FromJSON (table Identity)
  ) =>
  DBConfig beM ->
  Maybe Text ->
  Where be table ->
  L.Flow (DBResult [table Identity])
findAll dbConf (Just cacheKey) whereClause = do
  mRes <- rGetT (T.pack cacheName) cacheKey
  case mRes of
    (Just res) -> return $ Right res
    Nothing -> do
      mDBRes <- findAllSql dbConf whereClause
      whenRight mDBRes (cacheWithKey cacheKey)
      return mDBRes
findAll dbConf Nothing whereClause = findAllSql dbConf whereClause

findAllSql ::
  ( BeamRuntime be beM,
    BeamRunner beM,
    Model be table,
    B.HasQBuilder be,
    JSONEx (table Identity)
  ) => 
  DBConfig beM ->
  Where be table ->
  L.Flow (DBResult [table Identity])
findAllSql dbConf whereClause = do
  let findQuery = DB.findRows (sqlSelect ! #where_ whereClause ! defaults)
  sqlConn <- getOrInitSqlConn dbConf
  join <$> mapM (`L.runDB` findQuery) sqlConn

------------ helper functions ------------
runQuery ::
  ( BeamRuntime be beM, BeamRunner beM,
    JSONEx a
  ) =>
  DBConfig beM -> DB.SqlDB beM a -> L.Flow (DBResult a)
runQuery dbConf query = do
  conn <- getOrInitSqlConn dbConf
  case conn of
    Right c -> L.runDB c query
    Left  e -> return $ Left e

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
