{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE ScopedTypeVariables #-}

module EulerHS.Extra.EulerDB (
  getEulerDbConf,
  getEulerDbConfR1,
  withEulerDB,
  withEulerDBR1,
  ) where

import           EulerHS.Language (MonadFlow, SqlDB, getOption,
                                   getSqlDBConnection, logErrorT, runDB,
                                   throwException)
import           EulerHS.Prelude hiding (getOption)
import           EulerHS.Types (BeamRunner, BeamRuntime, DBConfig, DBResult,
                                OptionEntity, SqlConn)

import           Database.Beam.MySQL (MySQLM)
import           Servant (err500)


data EulerDbCfg = EulerDbCfg
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance OptionEntity EulerDbCfg (DBConfig MySQLM)

data EulerDbCfgR1 = EulerDbCfgR1
  deriving stock (Eq, Show, Typeable, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance OptionEntity EulerDbCfgR1 (DBConfig MySQLM)


getEulerDbConf :: (MonadFlow m, Exception e) => e -> m (DBConfig MySQLM)
getEulerDbConf = getEulerDbByConfig EulerDbCfg

getEulerDbConfR1 :: (MonadFlow m, Exception e) => e -> m (DBConfig MySQLM)
getEulerDbConfR1 = getEulerDbByConfig EulerDbCfgR1

getEulerDbByConfig :: (MonadFlow m, Exception e, OptionEntity k (DBConfig MySQLM))
  => k -> e -> m (DBConfig MySQLM)
getEulerDbByConfig dbConf internalError = do
  dbcfg <- getOption dbConf
  case dbcfg of
    Just cfg -> pure cfg
    Nothing -> do
      logErrorT "MissingDB identifier" "Can't find EulerDB identifier in options"
      throwException internalError

-- NOTE: Does NOT run inside a transaction
withEulerDB :: (MonadFlow m, Exception e) => e -> SqlDB MySQLM a -> m a
withEulerDB internalError act = withEulerDBGeneral EulerDbCfg internalError act

-- | Runs the query against the MySQL read replica DB
-- Falls back to using default MySQL DB if replica DB connection is not initialized
-- NOTE: Does NOT run inside a transaction
withEulerDBR1 :: (MonadFlow m, Exception e) => e -> SqlDB MySQLM a -> m a
withEulerDBR1 internalError act = withEulerDBGeneral EulerDbCfg internalError act

withEulerDBGeneral :: (MonadFlow m, Exception e, OptionEntity k (DBConfig MySQLM))
  => k -> e -> SqlDB MySQLM a -> m a
withEulerDBGeneral key internalError act = do
  dbcfg <- getOption key
  case dbcfg of
    Just cfg -> withDB cfg act
    Nothing -> do
      logErrorT "MissingDB identifier" "Can't find EulerDB identifier in options"
      throwException internalError

-- Helpers

withDB ::
  ( BeamRunner beM
  , BeamRuntime be beM
  , MonadFlow m
  )
  => DBConfig beM
  -> SqlDB beM a
  -> m a
withDB = withDB' runDB

withDB' :: (MonadFlow m) =>
  (SqlConn beM -> SqlDB beM a -> m (DBResult a))
  -> DBConfig beM
  -> SqlDB beM a
  -> m a
withDB' runDB' dbConf act = do
  mConn <- getSqlDBConnection dbConf
  conn <- case mConn of
    Right c -> pure c
    Left err -> do
      logErrorT "SqlDB connect" . show $ err
      throwException err500
  res <- runDB' conn act
  case res of
    Left dbError -> do
     logErrorT "SqlDB interraction" . show $ dbError
     throwException err500
    Right r -> pure r
