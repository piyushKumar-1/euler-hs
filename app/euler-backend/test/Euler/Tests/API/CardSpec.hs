{-# LANGUAGE AllowAmbiguousTypes #-}

module Euler.Tests.API.CardSpec where

import           EulerHS.Prelude

import           Euler.API.Validators.Card
import           Euler.Common.Validators
import qualified Euler.Product.Domain.Card as DC
import           EulerHS.Extra.Validation

import           Data.Validation
import           Test.Hspec


withValue :: (r -> ReaderT r m a) -> r -> m a
withValue validator value = runReaderT (validator value) value

more128 = "qweeeeeeeeeeeeeeeeeeeeeeedsfffffffffsdfffffffffffffffffffffffffffffffffsdfdsssssd ssssssssssssf fdddddddddddddddddddddddujhhhhhhhhhhhh"

spec :: Spec
spec =
  describe "Card validatators" $ do

      it "textNotEmpty success" $ do
        let val = withValue textNotEmpty "text"
        val `shouldBe` Right "text"

      it "textNotEmpty fail" $ do
        let val = withValue textNotEmpty ""
        val `shouldBe` Left [" Can't be empty"]

      it "textSizeFrom1To128 success" $ do
        let val = withValue textSizeFrom1To128 "123"
        val `shouldBe` Right "123"

      it "textSizeFrom1To128 fail" $ do
        let val = withValue textSizeFrom1To128 ""
        val `shouldBe` Left [" Length should be from 1 to 128."]

      it "textSizeFrom1To128 fail2" $ do
        let val = withValue textSizeFrom1To128 more128
        val `shouldBe` Left [more128 <> " Length should be from 1 to 128."]

      it "textMaxLength128 success" $ do
        let val = withValue textSizeFrom1To128 "123"
        val `shouldBe` Right "123"

      it "textMaxLength128 fail" $ do
        let val = withValue textSizeFrom1To128 ""
        val `shouldBe` Left [" Length should be from 1 to 128."]

      it "textMaxLength128 fail2" $ do
        let val = withValue textSizeFrom1To128 more128
        val `shouldBe` Left [more128 <> " Length should be from 1 to 128."]

      it "maskCardNumberValidator success" $ do
        let val = withValue maskCardNumberValidator "1234-XXXXXXXX-1234"
        val `shouldBe` Right "1234-XXXXXXXX-1234"

      it "maskCardNumberValidator success2" $ do
        let val = withValue maskCardNumberValidator "123456-XXXXXX-1234"
        val `shouldBe` Right "123456-XXXXXX-1234"

      it "maskCardNumberValidator fail" $ do
        let val = withValue maskCardNumberValidator ""
        val `shouldBe` Left [" 1234-XXXXXXXX-1234 or 123456-XXXXXX-1234"]

      it "maskCardNumberValidator fail2" $ do
        let val = withValue maskCardNumberValidator "123-XX-12312312312"
        val `shouldBe` Left ["123-XX-12312312312 1234-XXXXXXXX-1234 or 123456-XXXXXX-1234"]

      it "maskCardNumberValidator fail3" $ do
        let val = withValue maskCardNumberValidator "123-XX-"
        val `shouldBe` Left ["123-XX- 1234-XXXXXXXX-1234 or 123456-XXXXXX-1234"]

      it "cardIsinValidators success" $ do
        let val = withValue cardIsinValidators "123456"
        val `shouldBe` Right "123456"

      it "cardIsinValidators fail" $ do
        let val = withValue cardIsinValidators "qwerty"
        val `shouldBe` Left ["qwerty Only digits."]

      it "cardIsinValidators fail2" $ do
        let val = withValue cardIsinValidators "123"
        val `shouldBe` Left ["123 Length is 6"]

      it "cardLastFourDigitsV success" $ do
        let val = withValue cardLastFourDigitsV "1234"
        val `shouldBe` Right "1234"

      it "cardLastFourDigitsV fail" $ do
        let val = withValue cardLastFourDigitsV "qwerty"
        val `shouldBe` Left ["qwerty Length is 4","qwerty Only digits."]

      it "cardLastFourDigitsV fail2" $ do
        let val = withValue cardLastFourDigitsV "123"
        val `shouldBe` Left ["123 Length is 4"]

      it "cardExpYearValidator success" $ do
        let val = withValue cardExpYearValidator "2020"
        val `shouldBe` Right "2020"

      it "cardExpYearValidator fail" $ do
        let val = withValue cardExpYearValidator "qwerty"
        val `shouldBe` Left ["qwerty 1970 < Year <= 2050"]

      it "cardExpYearValidator fail2" $ do
        let val = withValue cardExpYearValidator "2060"
        val `shouldBe` Left ["2060 1970 < Year <= 2050"]

      it "cardExpMounthValidator success" $ do
        let val = withValue cardExpMounthValidator "12"
        val `shouldBe` Right "12"

      it "cardExpMounthValidator fail" $ do
        let val = withValue cardExpMounthValidator "0"
        val `shouldBe` Left ["0 1 <= n <= 12"]

      it "cardExpMounthValidator fail2" $ do
        let val = withValue cardExpMounthValidator "13"
        val `shouldBe` Left ["13 1 <= n <= 12"]

      it "isCardType success" $ do
        let val = withValue (isCardType <=< decode) "CREDIT"
        val `shouldBe` Right DC.CREDIT

      it "isCardType fail" $ do
        let val = withValue (isCardType <=< decode) "JUSPAY"
        val `shouldBe` Left ["Can't decode JUSPAY from field JUSPAY, should be one of [CREDIT,DEBIT,PREPAID,NB,WALLET,PAYLATER,UPI,ATM_CARD,REWARD]"]


