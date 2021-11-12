module EulerHS.Extra.Config where

import           EulerHS.Prelude

import qualified Data.Map.Strict as Map
import           System.Environment (getEnvironment)
import           System.IO.Unsafe (unsafePerformIO)

-- | Get enviromant variables unsafely
{-# NOINLINE environmentVars #-}
environmentVars :: Map String String
environmentVars = Map.fromList $ unsafePerformIO getEnvironment

-- | Find a particular variable by name.
lookupEnv :: String -> Maybe String
lookupEnv k = Map.lookup k environmentVars
