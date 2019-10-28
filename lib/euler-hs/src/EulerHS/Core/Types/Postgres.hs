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

toBeamPostgresConnectInfo :: PostgresConfig -> BP.ConnectInfo
toBeamPostgresConnectInfo (PostgresConfig {..}) = BP.ConnectInfo {..}

createPostgresConn :: PostgresConfig -> IO BP.Connection
createPostgresConn = BP.connect . toBeamPostgresConnectInfo

closePostgresConn :: BP.Connection -> IO ()
closePostgresConn = BP.close

