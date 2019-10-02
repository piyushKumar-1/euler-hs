module Main where

import           EulerHS.Prelude

import           Test.Tasty

import           Test.Framework.Language
import           Test.Types.Runtime

import           EulerHS.Runtime

import           Database.Redis (checkedConnect, defaultConnectInfo)
import           Data.Map (singleton)

tests :: FlowRuntime -> TestTree
tests rt = testGroup "Tests" [testLanguage rt]

main = do
  manager <- newEmptyMVar
  options <- newMVar mempty
  lrt <- createVoidLoggerRuntime
  conn <- checkedConnect defaultConnectInfo
  connPool <- newMVar (singleton "redis" $ Redis conn)
  let rt = FlowRuntime lrt manager options connPool
  defaultMain $ tests rt
