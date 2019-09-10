module EulerHS.App where

import EulerHS.Prelude

import CreditPlatform.API as CP


runCreditPlatformApp :: IO ()
runCreditPlatformApp = run 8080 (serve Proxy CP.creditPlatformServer)
