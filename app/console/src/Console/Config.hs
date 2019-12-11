{-# LANGUAGE DeriveGeneric #-}

module Console.Config
  ( Config(..)
  , loadConfig
  ) where

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

data Config =
  Config
    { httpPort :: Natural
    , enableCors :: Bool
    }
  deriving (Generic, Show)

instance Interpret Config

loadConfig :: IO Config
loadConfig = do
  envPath <- lookupEnv $ consoleEnv "CONFIG"
  xdgPath <- getXdgDirectory XdgConfig configPath
  input auto . T.pack . fromMaybe xdgPath $ envPath
