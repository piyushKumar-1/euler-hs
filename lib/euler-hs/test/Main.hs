module Main where

import           EulerHS.Prelude

import           Test.Tasty

import           Test.Framework.Language
import           Test.Types.Runtime

import           Network.HTTP.Client     (defaultManagerSettings, newManager)
import           EulerHS.Types

import           EulerHS.Runtime

tests :: FlowRuntime -> TestTree
tests rt = testGroup "Tests" [testLanguage rt]

main = do
  manager <- newMVar =<< newManager defaultManagerSettings
  options <- newMVar mempty
  lrt <- createVoidLoggerRuntime
  let rt = FlowRuntime lrt manager options
  defaultMain $ tests rt
