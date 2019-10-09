module Console.Env
  ( allowCors
  ) where

import System.Environment (lookupEnv)
import Universum

envPrefix :: String
envPrefix = "CONSOLE_"

allowCors :: IO Bool
allowCors = do
  env <- lookupEnv $ envPrefix <> "ALLOW_CORS"
  return $ case env of
                Just "1" -> True
                _        -> False
