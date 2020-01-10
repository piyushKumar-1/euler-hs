module Main where

import           EulerHS.Prelude

import           Test.Hspec
import qualified Euler.Tests.Transformation.TransactionSpec as TxnTransform
import qualified Euler.Tests.FlexApis.SampleApiSpec         as FlexApis.SampleApiSpec
import qualified Euler.Tests.Encryption.EncryptionSpec      as Encryption

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "Txn transformation"                         TxnTransform.spec
    describe "Flex casing and content-types: sample API"  FlexApis.SampleApiSpec.spec
    describe "Encryption"                                 Encryption.spec