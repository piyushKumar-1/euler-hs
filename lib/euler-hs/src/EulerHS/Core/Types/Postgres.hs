{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module EulerHS.Core.Types.Postgres where

import EulerHS.Prelude

import qualified Database.Beam.Postgres as BP
import qualified Data.Pool as DP
import Data.Time.Clock (NominalDiffTime)


data PostgresConfig = PostgresConfig
  { connectHost :: String
  , connectPort :: Word16
  , connectUser :: String
  , connectPassword :: String
  , connectDatabase :: String
  } deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

-- | Transform PostgresConfig to the Postgres ConnectInfo.
toBeamPostgresConnectInfo :: PostgresConfig -> BP.ConnectInfo
toBeamPostgresConnectInfo (PostgresConfig {..}) = BP.ConnectInfo {..}

-- | Connect with the given config to the database.
createPostgresConn :: PostgresConfig -> IO BP.Connection
createPostgresConn = BP.connect . toBeamPostgresConnectInfo

-- | Close the given connection.
closePostgresConn :: BP.Connection -> IO ()
closePostgresConn = BP.close

