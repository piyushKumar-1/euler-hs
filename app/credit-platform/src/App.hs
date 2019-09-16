module App where

import EulerHS.Prelude

import           Network.HTTP.Client (newManager, defaultManagerSettings)
import           Servant.Server (serve)
import           Network.Wai.Handler.Warp (run)

import CreditPlatform.API as CP

runCreditPlatformApp :: IO ()
runCreditPlatformApp = do
  let port = 8080

  mgr <- newManager defaultManagerSettings
  mgrMVar <- newMVar mgr

  run port CP.creditPlatformApp
