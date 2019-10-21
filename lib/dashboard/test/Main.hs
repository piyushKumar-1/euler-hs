module Main where

import Universum

import Test.Tasty
import Test.Tasty.Hspec

import qualified Test.SQL as SQL

main :: IO ()
main = defaultMain =<< specs

specs :: IO TestTree
specs = testSpec "specs" SQL.specs
