{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Euler.API.Validators.Customer where

import EulerHS.Prelude

import           EulerHS.Extra.Validation as V

import           Euler.Common.Validators

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
    <*> withField @"mobile_country_code" apiCst (extractJust >=> mobileCountryCodeValidators)

instance Transform AC.CustomerReqSignaturePayload DC.CreateCustomer where
  transform apiSigCst =  DC.CreateCustomer
    <$> withField @"customer_id"         apiSigCst objectReferenceIdValidators
    <*> withField @"mobile_number"       apiSigCst (extractJust >=> mobileNumberValidators)
    <*> withField @"email_address"       apiSigCst (insideJust emailAddressValidators)
    <*> withField @"first_name"          apiSigCst (insideJust firstNameValidators)
    <*> withField @"last_name"           apiSigCst (insideJust lastNameValidators)
    <*> withField @"mobile_country_code" apiSigCst (extractJust >=> mobileCountryCodeValidators)
