{-# LANGUAGE DeriveGeneric #-}

module Console.Config
  ( App(..)
  , Redis(..)
  , loadConfig
  ) where

import Dashboard.Query.Config (QueryConfiguration)
import Dhall (Interpret, auto, input)
import System.Directory (XdgDirectory(..), getXdgDirectory)
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import qualified Data.Text as T
import Universum

consoleEnv :: String -> String
consoleEnv s = "CONSOLE_" ++ s

configPath :: FilePath
configPath = "console" </> "console.dhall"

data Redis =
  Redis
    { redisHost :: Text
    , redisPort :: Natural
    , redisDb :: Natural
    }
  deriving (Generic, Show)

instance Interpret Redis

data App =
  App
    { httpPort :: Natural
    , jwtSecret :: Text
    , redis :: Redis
    , enableCors :: Bool
    , bqProject :: Text
    , queryConfig :: QueryConfiguration
    }
  deriving (Generic, Show)

instance Interpret App

loadConfig :: IO App
loadConfig = do
  envPath <- lookupEnv $ consoleEnv "CONFIG"
  xdgPath <- getXdgDirectory XdgConfig configPath
  input auto . T.pack . fromMaybe xdgPath $ envPath
