{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Euler.Common.Validators where

import EulerHS.Prelude
import EulerHS.Extra.Validation as V
import qualified Data.Map  as Map
import qualified Data.Set  as Set
import qualified Data.Text as T
import           Data.Char (isDigit)
import           Data.Attoparsec.Text

-- EHS: rework imports, rework dependencies.
import Euler.Common.Types.Gateway   (Gateway, GatewayId, gatewayRMap, gateways)
import Euler.Common.Types.Order (UDF(..))
import Euler.Common.Types.Transaction (AuthType(..))
import Euler.Product.Domain.UPIPayment (UPITxnType)
import Euler.Product.Domain.Card (CardType)
import Euler.Product.Domain.UPIPayment as UPI
import qualified Euler.Product.Domain.Card as DC

toInt :: Transformer Text Int
toInt = V.decode

-- | Non-negative amount validator, use it when checking amounts other than order amount.
notNegativeAmount :: Validator Double
notNegativeAmount =
  parValidate
    [ max2DecimalDigits
    , notNegativeF
    ]

-- | Order amount validator
amountValidators :: Validator Double
amountValidators =
  parValidate
    [ max2DecimalDigits
    , gteOne
    ]

notBlank :: Validator Text
notBlank = mkValidator "Can't be blank" (not . T.null . T.strip)

notNegative :: Validator Int
notNegative = mkValidator "Should not be negative." (>= 0)

notNegativeF :: Validator Double
notNegativeF = mkValidator "Should not be negative." (>= 0.0)

textNotEmpty :: Validator Text
textNotEmpty = mkValidator "Can't be empty" (not . T.null)

--Will accept double values with upto two decimal places.
max2DecimalDigits :: Validator Double
max2DecimalDigits = mkValidator
  "Will accept double values with upto two decimal places."
  ((<=3) . length . dropWhile (/='.') . show)

gteOne :: Validator Double
gteOne = mkValidator
  "Should be greater than or equal 1"
  (>=1)

textSizeFrom1To128 :: Validator Text
textSizeFrom1To128 = mkValidator "Length should be from 1 to 128." (lengthFromTo 1 128)

textMaxLength128 :: Validator Text
textMaxLength128 = mkValidator "Max length is 128." (textMaxLength 128)

onlyDigits :: Validator Text
onlyDigits = mkValidator "Only digits." onlyDigits'

onlyDigits' :: Text -> Bool
onlyDigits' = all isDigit

textLength6 :: Validator Text
textLength6 = mkValidator "Length is 6" (textLength 6)

textLength4 :: Validator Text
textLength4 = mkValidator "Length is 4" (textLength 4)

textLength :: Int -> Text -> Bool
textLength n txt = length txt == n


textMaxLength :: Int -> Text -> Bool
textMaxLength n t = length t <= n

lengthFromTo :: Int -> Int -> Text -> Bool
lengthFromTo from to t = tLength <= to && tLength >= from
  where
    tLength = length t

textMaxLength16 :: Validator Text
textMaxLength16 = mkValidator "Max length is 16." (textMaxLength 16)

textMaxLength64 :: Validator Text
textMaxLength64 = mkValidator "Max length is 64." (textMaxLength 64)


testValidator1 :: Validator a
testValidator1  = mkValidator "always should pass" (const True)

testValidator2 :: Validator a
testValidator2  = pure

--------------------------------------------------------------------------------

-- EHS: domain-related validators temporary here.


isRegularCardAuthType :: Validator AuthType
isRegularCardAuthType = mkValidator "inappropriate field type" (`elem` [THREE_DS, OTP, VISA_CHECKOUT])

isAtmCardAuthType :: Validator AuthType
isAtmCardAuthType = mkValidator "inappropriate field type" (== ATMPIN)

isUPICollectTxnType :: Validator UPITxnType
isUPICollectTxnType = mkValidator "inappropriate txn_type" (== UPI.UPI_COLLECT)

isUPIPayTxnType :: Validator UPITxnType
isUPIPayTxnType = mkValidator "inappropriate txn_type" (`elem` [UPI.UPI_PAY, UPI.BHARAT_PAY])

cardNumberValidators :: Validator Text
cardNumberValidators =
  parValidate
    [ textNotEmpty
    , testValidator1
    , testValidator2
    ]

cardExpMonthB :: Text -> Bool
cardExpMonthB year = case parseOnly (many1 digit) year of
  Left _ -> False
  Right d ->
    let n = read d :: Int
    in 1 <= n && n <= 12


cardExpYearB :: Text -> Bool
cardExpYearB year = case parseOnly (many1 digit) year of
  Left _ -> False
  Right d ->
    let n = read d :: Int
    in 1970 < n && n <= 2050


maskCardNumberValidator :: Validator Text
maskCardNumberValidator = mkValidator "1234-XXXXXXXX-1234 or 123456-XXXXXX-1234" maskCardNumber'

cardIsinValidators :: Validator Text
cardIsinValidators = parValidate
  [ textLength6
  , onlyDigits
  ]

cardExpYearValidator :: Validator Text
cardExpYearValidator = mkValidator "1970 < Year <= 2050" cardExpYearB

cardExpMounthValidator :: Validator Text
cardExpMounthValidator = mkValidator "1 <= n <= 12" cardExpMonthB

cardLastFourDigitsV :: Validator Text
cardLastFourDigitsV = parValidate
  [ textLength4
  , onlyDigits
  ]



transformGatewayId :: Transformer Int GatewayId
transformGatewayId v = do
  V.guarded ("Should be in " <> show gatewayRMap) $ v `Map.member` gatewayRMap
  pure v

customerIdValidators :: Validator Text
customerIdValidators =
  parValidate
    [ notBlank
    ]


maskCardNumber' :: Text -> Bool
maskCardNumber' number' = case parseOnly parser number' of
  Left _ -> False
  Right (digitsFirst, charX, digitLast) ->
    and [(digitsFirst == 6 && charX == 6) || (digitsFirst == 4 && charX == 8), digitLast == 4]
  where
    parser = do
      d1 <- manyTill digit (char '-')
      x <- manyTill (char 'X') (char '-')
      d2 <- many1 digit
      pure $ (length d1, length x, length d2)


isCardType :: Validator CardType
isCardType = mkValidator "Text is CardType" (`elem`
  [ DC.CREDIT
  , DC.DEBIT
  , DC.PREPAID
  , DC.NB
  , DC.WALLET
  , DC.PAYLATER
  , DC.UPI
  , DC.ATM_CARD
  , DC.REWARD
  ])

isGateway :: Validator Gateway
isGateway = mkValidator "Text is Gateway" (`elem` gateways)

objectReferenceIdValidators :: Validator Text
objectReferenceIdValidators =
  parValidate
    [ textSizeFrom1To128
    ]

mobileNumberValidators :: Validator Text
mobileNumberValidators =
  parValidate
    [ textNotEmpty
    , textMaxLength16
    , onlyDigits
    ]

mobileCountryCodeValidators :: Validator Text
mobileCountryCodeValidators =
  parValidate
    [ onlyDigits
    ]

emailAddressValidators :: Validator Text
emailAddressValidators =
  parValidate
    [ textMaxLength128
    ]

firstNameValidators :: Validator Text
firstNameValidators =
  parValidate
    [ textMaxLength64
    ]

lastNameValidators :: Validator Text
lastNameValidators =
  parValidate
    [ textMaxLength64
    ]

cleanUpUDF :: UDF -> UDF
cleanUpUDF UDF {..} = UDF
  { udf1 = cleanUp udf1
  , udf2 = cleanUp udf2
  , udf3 = cleanUp udf3
  , udf4 = cleanUp udf4
  , udf5 = cleanUp udf5
  , ..
  }
  where
    cleanUp :: Maybe Text -> Maybe Text
    cleanUp mStr =  cleanUpSpecialChars <$>  mStr

    cleanUpSpecialChars :: Text -> Text
    cleanUpSpecialChars = T.filter (`Set.notMember` specialChars)

    specialChars :: Set.Set Char
    specialChars = Set.fromList "~!#%^=+\\|:;,\"'()-.&/"