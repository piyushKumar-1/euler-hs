module App where

import EulerHS.Prelude

import           Servant.Server (serve)
import           Network.Wai.Handler.Warp (run)

import qualified Euler.Server as Euler
import qualified Euler.Playback.Service as PB
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
    recorderParams <- PB.initRecorderParams
    let env = Euler.Env flowRt recorderParams
    run port $ Euler.eulerBackendApp env