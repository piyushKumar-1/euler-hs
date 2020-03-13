module EulerBackend(main) where

import EulerHS.Prelude

import App (runEulerBackendApp)
import Euler.Playback.Service
import           System.Environment

-- TODO: command line args, env configs here


main :: IO ()
main = do --runEulerBackendApp
  appMode <- lookupEnv "APP_MODE"
  case appMode of
    Just "PLAYER" -> runSinglePlayerMode
    Just "BULK_PLAYER" -> runBulkPlayerMode
    _ -> runEulerBackendApp
