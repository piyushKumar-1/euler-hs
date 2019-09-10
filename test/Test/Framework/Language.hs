module Test.Framework.Language (testLanguage) where

import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck as QC

import           Data.List
import           Data.Ord

unitTests :: TestTree
unitTests = testGroup "Unit tests" [ testCase "Dumb test" ([1, 2, 3] `compare` [1,2] @?= GT) ]

testLanguage :: TestTree
testLanguage = testGroup "EulerHS.Framework.Language tests" [unitTests]
