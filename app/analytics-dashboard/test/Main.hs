module Main where

import Universum

import Test.Tasty
import Test.Tasty.Hspec

import qualified Test.Query as Query
import Test.Fixtures (withConsoleServer)

main = defaultMain =<< specs

specs :: IO TestTree
specs = testSpec "specs" $ around_ withConsoleServer Query.specs
