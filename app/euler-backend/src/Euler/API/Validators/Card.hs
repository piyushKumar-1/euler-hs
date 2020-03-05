{-# LANGUAGE AllowAmbiguousTypes #-}
module Euler.API.Validators.Card where

import EulerHS.Prelude

import           EulerHS.Extra.Validation as V
import           Euler.Common.Validators
import qualified Euler.Product.Domain.Card as DC
import qualified Euler.API.Card as AC
import           Data.Char (isDigit)

import qualified Data.Text as T
import Data.Attoparsec.Text


instance Transform AC.AddCardInputRequest DC.StoredCard where
  transform apiCst = DC.StoredCard
    <$> withField @"merchantAccountId"     apiCst pure
    <*> withField @"customerId"            apiCst textMaxLength128

    <*> withField @"maskedCardNumber"      (AC.cardDetail apiCst) maskCardNumberValidator
    <*> withField @"nameOnCard"            (AC.cardDetail apiCst) (insideJust textMaxLength128)
    <*> withField @"cardExpMonth"          (AC.cardDetail apiCst) cardExpMounthValidator
    <*> withField @"cardExpYear"           (AC.cardDetail apiCst) cardExpYearValidator
    <*> withField @"cardIsin"              (AC.cardDetail apiCst) cardIsinValidators
    <*> withField @"cardLastFourDigits"    (AC.cardDetail apiCst) cardLastFourDigitsV
    <*> withField @"cardReference"         (AC.cardDetail apiCst) (extractJust >=> textNotEmpty)
    <*> withField @"cardFingerprint"       (AC.cardDetail apiCst) (insideJust textNotEmpty)
    <*> withField @"cardGlobalFingerprint" (AC.cardDetail apiCst) (insideJust textNotEmpty)
    <*> pure DC.JUSPAY
    <*> withField @"nickname"              (AC.cardDetail apiCst) (insideJust textMaxLength128)
    <*> withField @"cardType"              (AC.cardDetail apiCst) (insideJust (decode >=> isCardType))
    <*> withField @"cardIssuer"            (AC.cardDetail apiCst) (insideJust textNotEmpty)
    <*> withField @"cardBrand"             (AC.cardDetail apiCst) (insideJust textNotEmpty)
    <*> withField @"cardToken"             (AC.cardDetail apiCst) (insideJust textNotEmpty)


--textNotEmpty :: Validator Text
--textNotEmpty = mkValidator "Can't be empty." (not . T.null)
--
--textSizeFrom1To128 :: Validator Text
--textSizeFrom1To128 = mkValidator "Length should be from 1 to 128." (lengthFromTo 1 128)
--
--textMaxLength128 :: Validator Text
--textMaxLength128 = mkValidator "Max length is 128." (textMaxLength 128)
--
--onlyDigits :: Validator Text
--onlyDigits = mkValidator "Only digits." onlyDigits'
--
--maskCardNumberValidator :: Validator Text
--maskCardNumberValidator = mkValidator "1234-XXXXXXXX-1234 or 123456-XXXXXX-1234" maskCardNumber'
--
--cardIsinValidators :: Validator Text
--cardIsinValidators = parValidate
--  [ textLength6
--  , onlyDigits
--  ]
--
--cardExpYearValidator :: Validator Text
--cardExpYearValidator = mkValidator "1970 < Year <= 2050" cardExpYearB
--
--cardExpMounthValidator :: Validator Text
--cardExpMounthValidator = mkValidator "1 <= n <= 12" cardExpMonthB
--
--cardLastFourDigitsV :: Validator Text
--cardLastFourDigitsV = parValidate
--  [ textLength4
--  , onlyDigits
--  ]
--
--textLength6 :: Validator Text
--textLength6 = mkValidator "Length is 6" (textLength 6)
--
--textLength4 :: Validator Text
--textLength4 = mkValidator "Length is 4" (textLength 4)
--
--isCardType :: Validator DC.CardType
--isCardType = mkValidator "Text is CardType" (`elem`
--  [ DC.CREDIT
--  , DC.DEBIT
--  , DC.PREPAID
--  , DC.NB
--  , DC.WALLET
--  , DC.PAYLATER
--  , DC.UPI
--  , DC.ATM_CARD
--  , DC.REWARD
--  ])
--
--maskCardNumber' :: Text -> Bool
--maskCardNumber' number = case parseOnly parser number of
--  Left _ -> False
--  Right (digitsFirst, charX, digitLast) ->
--    and [(digitsFirst == 6 && charX == 6) || (digitsFirst == 4 && charX == 8), digitLast == 4]
--  where
--    parser = do
--      d1 <- manyTill digit (char '-')
--      x <- manyTill (char 'X') (char '-')
--      d2 <- many1 digit
--      pure $ (length d1, length x, length d2)
--
--textLength :: Int -> Text -> Bool
--textLength n txt = length txt == n
--
--cardExpYearB :: Text -> Bool
--cardExpYearB year = case parseOnly (many1 digit) year of
--  Left _ -> False
--  Right d ->
--    let n = read d :: Int
--    in 1970 < n && n <= 2050
--
--cardExpMonthB :: Text -> Bool
--cardExpMonthB year = case parseOnly (many1 digit) year of
--  Left _ -> False
--  Right d ->
--    let n = read d :: Int
--    in 1 <= n && n <= 12
--
--onlyDigits' :: Text -> Bool
--onlyDigits' = all isDigit
--
--textMaxLength :: Int -> Text -> Bool
--textMaxLength n t = length t <= n
--
--lengthFromTo :: Int -> Int -> Text -> Bool
--lengthFromTo from to t = tLength <= to && tLength >= from
--  where
--    tLength = length t
