module Main where

import           EulerHS.Prelude

<<<<<<< HEAD
import           Test.Hspec
import qualified Euler.Tests.API.CardSpec as Card
import qualified Euler.Tests.API.OrderSpec as Order
import qualified Euler.Tests.API.OrderUpdateSpec as OrderUpdate
import qualified Euler.Tests.Encryption.EncryptionSpec as Encryption
import qualified Euler.Tests.FlexApis.SampleApiSpec as FlexApis.SampleApiSpec
||||||| merged common ancestors
import           Test.Hspec
import qualified Euler.Tests.API.OrderSpec                  as Order
=======
import qualified Euler.Tests.API.CardSpec as Card
import qualified Euler.Tests.API.OrderSpec as Order
import qualified Euler.Tests.Encryption.EncryptionSpec as Encryption
import qualified Euler.Tests.FlexApis.SampleApiSpec as FlexApis.SampleApiSpec
>>>>>>> Add test for validator functions
import qualified Euler.Tests.Transformation.TransactionSpec as TxnTransform
import qualified Euler.Tests.Money.MoneySpec                as MoneySpec
import qualified Euler.Tests.API.AuthRSA                    as AuthRSA
import           Test.Hspec



main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "Order spec"                                 Order.spec
<<<<<<< HEAD
    describe "OrderUpdate"                                OrderUpdate.spec
    describe "Card validators spec"                       Card.spec
||||||| merged common ancestors
=======
    describe "Card validators spec"                       Card.spec
>>>>>>> Add test for validator functions
    describe "Txn transformation"                         TxnTransform.spec
    describe "Flex casing and content-types: sample API"  FlexApis.SampleApiSpec.spec
    describe "Encryption"                                 Encryption.spec
    describe "Money Tests"                                MoneySpec.spec
    describe "AuthRSA"                                    AuthRSA.spec
