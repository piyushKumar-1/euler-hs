module Main where

import           EulerHS.Prelude

import           Test.Hspec  
import qualified Euler.Tests.Transformation.TransactionSpec as TxnTransform
import qualified Euler.Tests.FlexApis.SampleApiSpec         as FlexApis.SampleApiSpec
  
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "Txn transformation"                         TxnTransform.spec
    describe "Flex casing and content-types: sample API"  FlexApis.SampleApiSpec.spec