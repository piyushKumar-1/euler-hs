module Main where

import EulerHS.Prelude

import Console.API (app)
import Network.Wai.Handler.Warp (run)

main :: IO ()
main = run 8080 app
