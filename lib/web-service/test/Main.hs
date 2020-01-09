module Main where

import           EulerHS.Prelude

import qualified Tests.AnyBaseTest as Any
import qualified Tests.UuidTest as Uuid

import           Test.Hspec
import           Hedgehog.Main



main :: IO ()
main = do
  defaultMain
    [ Any.anyBaseTests
    , Uuid.uuidTests
    ]
  hspec Uuid.spec
  hspec Any.spec
