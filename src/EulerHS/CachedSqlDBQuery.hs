{-# LANGUAGE OverloadedStrings #-}
module EulerHS.CachedSqlDBQuery
  ( findOne
  )
where

import EulerHS.Core.Types.DB
import qualified EulerHS.Core.SqlDB.Language as DB
import EulerHS.Extra.Language (getOrInitSqlConn, rGetT, rSetB)
import qualified EulerHS.Framework.Language as L
import EulerHS.Prelude

import Data.Aeson (encode)
import qualified Data.ByteString.Lazy as BSL
import qualified Database.Beam as B
import qualified Data.Text as T
import Named ((!), defaults)
import Sequelize

-- Core API

-- `create`
-- Insert value if the table already exist, create a new table otherwise
-- Implement `create` in sequelize first

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
  L.Flow (Either DBError (Maybe (table Identity)))
findOne sqlConn (Just cacheKey) whereClause = do
  -- TODO: What should the KVDB name be here?
  mRes <- rGetT (T.pack "redis") cacheKey
  case mRes of
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
  L.Flow (Either DBError (Maybe (table Identity)))
findOneSql sqlConn whereClause = L.runDB sqlConn findQuery
  where findQuery = DB.findRow (sqlSelect ! #where_ whereClause ! defaults)

------------ Beam and Redis helper functions ------------

cacheWithKey :: (ToJSON table) => Text -> table -> L.Flow ()
cacheWithKey key row = do
  -- TODO: Which KVDB should be used here?
  -- TODO: Should we log errors here? 
  void $ rSetB (T.pack "redis") (encodeUtf8 key) (BSL.toStrict $ encode row)
