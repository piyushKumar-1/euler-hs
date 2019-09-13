module Main where

import           EulerHS.Prelude

import           Test.Tasty

import           Test.Framework.Language
import           Test.Types.Runtime

import           Network.HTTP.Client     (defaultManagerSettings, newManager)
import EulerHS.Framework.Language.Types

tests :: Runtime -> TestTree
tests rt = testGroup "Tests" [testLanguage rt]

main = do
  manager <- newManager defaultManagerSettings
  options <- newMVar mempty
  let rt = Runtime options manager
  defaultMain $ tests rt
