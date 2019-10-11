module App where

import EulerHS.Prelude

import           Servant.Server (serve)
import           Network.Wai.Handler.Warp (run)

import qualified Euler.Server as Euler
import qualified EulerHS.Runtime as R
import qualified EulerHS.Types as T


runEulerBackendApp :: IO ()
runEulerBackendApp = do
  let port = 8080
  let loggerCfg = T.defaultLoggerConfig
        { T._logToFile = True
        , T._logFilePath = "/tmp/euler-backend.log"
        , T._isAsync = True
        }

  R.withFlowRuntime (Just loggerCfg) $ \flowRt -> do
    putStrLn @String "Runtime created. Starting server..."
    let env = Euler.Env flowRt
    run port $ Euler.eulerBackendApp env