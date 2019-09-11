module Main where

import           Test.Tasty

import           Test.Framework.Language

tests :: TestTree
tests = testGroup "Tests" [testLanguage]

main = defaultMain tests
