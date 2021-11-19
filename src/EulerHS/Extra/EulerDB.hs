{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE ScopedTypeVariables #-}

module EulerHS.Extra.EulerDB (
  getEulerDbConf,
  getEulerDbConfR1,
  withEulerDB,
  withEulerDBR1,
  ) where

import           EulerHS.Language (MonadFlow, SqlDB, getOption, logErrorT,
                                   throwException, withDB)
import           EulerHS.Prelude hiding (getOption)
import           EulerHS.Types (DBConfig, OptionEntity)

import           Database.Beam.MySQL (MySQLM)


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
