{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE ScopedTypeVariables #-}

module EulerHS.Extra.DB (
  getEulerDbConf,
  withEulerDB,
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

getEulerDbConf :: (MonadFlow m, Exception e) => e -> m (DBConfig MySQLM)
getEulerDbConf internalError = do
  dbcfg <- getOption EulerDbCfg
  case dbcfg of
    Just cfg -> pure cfg
    Nothing -> do
      logErrorT "MissingDB identifier" "Can't find EulerDB identifier in options"
      throwException internalError

withEulerDB :: (MonadFlow m, Exception e) => e -> SqlDB MySQLM a -> m a
withEulerDB internalError act = do
  dbcfg <- getOption EulerDbCfg
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
