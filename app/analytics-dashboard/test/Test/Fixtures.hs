module Test.Fixtures where

import Universum

import qualified Control.Concurrent as C
import qualified Network.Wai.Handler.Warp as Warp

import Console.API (app)

testPort = 8084

withConsoleServer :: IO () -> IO ()
withConsoleServer action =
  bracket (liftIO $ C.forkIO $ Warp.run testPort app)
    C.killThread
    (const action)
