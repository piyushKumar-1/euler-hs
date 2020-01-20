{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Euler.API.Validators.Customer where

import EulerHS.Prelude
import EulerHS.Extra.Validation as V

import qualified Euler.Product.Domain.Customer as DC
import qualified Euler.API.Customer as AC


--         mobileNumber nullable: false, blank: false, digits: true, maxSize: 16
--         objectReferenceId nullable: false, size: 1..128, unique: "merchantAccount"
--         emailAddress nullable: true, blank: true, maxSize: 128
--         firstName nullable: true, blank: true, maxSize: 64
--         lastName nullable: true, blank: true, maxSize: 64
--     }


instance Transform AC.CustomerReq DC.CreateCustomer where
  transform apiCst = DC.CreateCustomer
    <$> withField @"object_reference_id" apiCst (extractJust >=> objectReferenceIdValidators)
    <*> withField @"mobile_number"       apiCst (extractJust >=> mobileNumberValidators)
    <*> withField @"email_address"       apiCst (insideJust emailAddressValidators)
    <*> withField @"first_name"          apiCst (insideJust firstNameValidators)
    <*> withField @"last_name"           apiCst (insideJust lastNameValidators)
    <*> withField @"mobile_country_code" apiCst (insideJust mobileCountryCodeValidators)

instance Transform AC.CustomerReqSignaturePayload DC.CreateCustomer where
  transform apiSigCst =  DC.CreateCustomer
    <$> withField @"customer_id" apiCst objectReferenceIdValidators
    <*> withField @"mobile_number"       apiSigCst (extractJust >=> mobileNumberValidators)
    <*> withField @"email_address"       apiSigCst (insideJust emailAddressValidators)
    <*> withField @"first_name"          apiSigCst (insideJust firstNameValidators)
    <*> withField @"last_name"           apiSigCst (insideJust lastNameValidators)
    <*> withField @"mobile_country_code" apiSigCst (insideJust mobileCountryCodeValidators)

objectReferenceIdValidators :: Validator Text
objectReferenceIdValidators =
  parValidate
    [ textSizeFrom1To128
    ]

mobileNumberValidators :: Validators Text
mobileNumberValidators =
  parValidate
    [ textNotEmpty
    , textMaxLength16
    , onlyDigits
    ]

mobileCountryCodeValidators :: Validators Text
mobileCountryCodeValidators =
  parValidate
    [ onlyDigits
    ]

emailAddressValidators :: Validators Text
emailAddressValidators =
  parValidate
    [ textMaxLength128
    ]

firstNameValidators :: Validators Text
firstNameValidators =
  parValidate
    [ textMaxLength64
    ]

lastNameValidators :: Validators Text
lastNameValidators =
  parValidate
    [ textMaxLength64
    ]
textNotEmpty :: Validator Text
textNotEmpty = mkValidator "Can't be empty." (not . T.null)

textSizeFrom1To128 :: Validator Text
textSizeFrom1To128 = mkValidator "Length should be from 1 to 128." (lengthFromTo 1 128)

textMaxLength16 :: Validator Text
textMaxLength16 = mkValidator "Max length is 16." (textMaxLength 16)

textMaxLength64 :: Validator Text
textMaxLength64 = mkValidator "Max length is 64." (textMaxLength 64)

textMaxLength128 :: Validator Text
textMaxLength128 = mkValidator "Max length is 128." (textMaxLength 128)

onlyDigits :: Validator Text
onlyDigits = mkValidator "Only digits." onlyDigits'


onlyDigits' :: Text -> Bool
onlyDigits' = all isDigit

textMaxLength :: Int -> Text -> Bool
textMaxLength n t = length t <= n

lengthFromTo :: Int -> Int -> Text -> Bool
lengthFromTo from to t = tLength <= to && tLength >= from
  where
    tLength = length t