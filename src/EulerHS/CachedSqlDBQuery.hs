{-# LANGUAGE OverloadedStrings #-}
module EulerHS.CachedSqlDBQuery
  ( create
  , createSQL
  , updateOne
  , updateOneSQL
  , findOne
  , findOneSQL
  , findOneKV
  , findAll
  , findAllSQL
  , findAllKV
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
import Database.Beam.Backend.SQL.BeamExtensions (MonadBeamUpdateReturning(..))
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
  res <- createSQL dbConf value 
  case res of
    Right val -> do
      whenJust mCacheKey (`cacheWithKey` val)
      return $ Right val
    Left e -> return $ Left e

createSQL ::
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
  L.Flow (DBResult (table Identity))
createSQL dbConf value = do
  res <- runQuery dbConf $ DB.insertRowsReturningList $ sqlCreate value
  case res of
    Right [val] -> return $ Right val
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
    B.HasQBuilder be,
    ToJSON (table Identity),
    FromJSON (table Identity),
    Show (table Identity)
  ) =>
  DBConfig beM ->
  Maybe Text ->
  [Set be table] ->
  Where be table ->
  L.Flow (DBResult (table Identity))
updateOne dbConf (Just cacheKey) newVals whereClause = do
  val <- updateOneSQL dbConf newVals whereClause
  whenRight val (\_ -> cacheWithKey cacheKey val)
  return val
updateOne dbConf Nothing value whereClause = updateOneSQL dbConf value whereClause

updateOneSQL ::
  forall be beM table.
  ( BeamRuntime be beM,
    BeamRunner beM,
    Model be table,
    B.HasQBuilder be,
    FromJSON (table Identity),
    ToJSON (table Identity),
    Show (table Identity)
  ) =>
  DBConfig beM ->
  [Set be table] ->
  Where be table ->
  L.Flow (DBResult (table Identity))
updateOneSQL dbConf newVals whereClause = do
  let updateQuery = DB.updateRowsReturningList $ sqlUpdate
        ! #set newVals
        ! #where_ whereClause
  res <- runQuery dbConf updateQuery
  case res of
    Right [x] -> return $ Right x
    Right xs -> do
      let message = "DB returned \"" <> show xs <> "\" after update"
      L.logError @Text "create" message
      return $ Left $ DBError UnexpectedResult message
    Left e -> return $ Left e

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
  mRes <- findOneKV cacheKey
  case join mRes of
    (Just res) -> return $ Right $ Just res
    Nothing -> do
      mDBRes <- findOneSQL dbConf whereClause
      whenRight mDBRes (cacheWithKey cacheKey)
      return mDBRes
findOne dbConf Nothing whereClause = findOneSQL dbConf whereClause

findOneSQL ::
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
findOneSQL dbConf whereClause = runQuery dbConf findQuery
  where findQuery = DB.findRow (sqlSelect ! #where_ whereClause ! defaults)

findOneKV ::
  ( FromJSON a ) =>
  Text -> L.Flow (Maybe (Maybe a))
findOneKV key = rGetT (T.pack cacheName) key

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
  mRes <- findAllKV cacheKey
  case mRes of
    (Just res) -> return $ Right res
    Nothing -> do
      mDBRes <- findAllSQL dbConf whereClause
      whenRight mDBRes (cacheWithKey cacheKey)
      return mDBRes
findAll dbConf Nothing whereClause = findAllSQL dbConf whereClause

findAllSQL ::
  ( BeamRuntime be beM,
    BeamRunner beM,
    Model be table,
    B.HasQBuilder be,
    JSONEx (table Identity)
  ) => 
  DBConfig beM ->
  Where be table ->
  L.Flow (DBResult [table Identity])
findAllSQL dbConf whereClause = do
  let findQuery = DB.findRows (sqlSelect ! #where_ whereClause ! defaults)
  sqlConn <- getOrInitSqlConn dbConf
  join <$> mapM (`L.runDB` findQuery) sqlConn

findAllKV :: ( FromJSON a ) => Text -> L.Flow (Maybe [a])
findAllKV key = rGetT (T.pack cacheName) key


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
