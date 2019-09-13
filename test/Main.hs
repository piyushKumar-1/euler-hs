module Main where

import           EulerHS.Prelude

import           Test.Tasty

import           Test.Framework.Language
import           Test.Types.Runtime

import           Network.HTTP.Client     (defaultManagerSettings, newManager)

tests :: TestRT -> TestTree
tests rt = testGroup "Tests" [testLanguage rt]

main = do
  manager' <- newManager defaultManagerSettings
  manager  <- newMVar manager'
  defaultMain $ tests (TestRT manager 8081)
