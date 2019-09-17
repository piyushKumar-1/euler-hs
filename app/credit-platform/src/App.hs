module App where

import EulerHS.Prelude

import           Servant.Server (serve)
import           Network.Wai.Handler.Warp (run)

import qualified CreditPlatform.Server as CP
import qualified EulerHS.Runtime as R
import qualified EulerHS.Types as T


runCreditPlatformApp :: IO ()
runCreditPlatformApp = do
  let port = 8080
  let loggerCfg = T.defaultLoggerConfig
        { T._logToFile = True
        , T._logFilePath = "/tmp/euler-hs.log"
        , T._isAsync = True
        }

  R.withFlowRuntime (Just loggerCfg) $ \flowRt -> do
    putStrLn @String "Runtime created. Starting server..."
    let env = CP.Env flowRt
    run port $ CP.creditPlatformApp env
