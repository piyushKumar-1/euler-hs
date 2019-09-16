module Test.Types.Runtime where

import           EulerHS.Prelude
import           Network.HTTP.Client      (Manager)
import           Network.Wai.Handler.Warp (Port)

data TestRT = TestRT { manager :: MVar Manager, port :: Port }
