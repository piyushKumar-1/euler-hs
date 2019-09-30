module Main where

import           EulerHS.Prelude

import           Test.Hspec

import qualified EulerHS.Tests.Framework.Languages as Framework

main = hspec Framework.flow
