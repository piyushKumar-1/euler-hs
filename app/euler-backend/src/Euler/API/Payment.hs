{-# LANGUAGE DeriveAnyClass #-}

-- FIXME: identify the target types and get rid of this preliminary stuff

module Euler.API.Payment where

import           EulerHS.Prelude

import qualified Data.ByteString.Lazy.Char8         as BC
import qualified Data.Text                          as Text
import           WebService.ContentType               
                (WrappableJSON, wrapper)
import           WebService.FlexCasing                
                (FlexCasingResponse, casing, flexCasingOptions)

data PaymentStatus = PaymentStatus 
  { gatewayId        :: Int
  , status           :: Text.Text
  , statusId         :: Int
  , bankErrorMessage :: Text.Text
  , orderId          :: Text.Text
  , merchantId       :: Text.Text
  , bankErrorCode    :: Int
  , returnUrl        :: Text.Text
} deriving (Generic)

defaultPaymentStatus = PaymentStatus 
  17 
  "CHARGED" 
  21 
  ""
  "5ZLTuReaA5f36OKf_1737991" 
  "furlenco" 
  0 
  "https://gringotts.furlenco.com/payments/response?order_id..." 


-- TODO how could it be abstracted better? we have several types for payloads
data PaymentStatusResponse = PaymentStatusResponse
  { payload         :: PaymentStatus
  , caseStyle       :: Text.Text          
  , callback        :: Maybe Text.Text 
  }

instance FlexCasingResponse PaymentStatusResponse where    
  casing = caseStyle

-- for wrapped JS repsonse
instance WrappableJSON PaymentStatusResponse where
  wrapper p = case (callback $ p) of
    Just s ->  BC.pack $ Text.unpack $ s
    Nothing -> undefined        

-- for JSON response
instance ToJSON PaymentStatusResponse where    
  toJSON v = genericToJSON (flexCasingOptions v) (payload v)

-- dummy deserializer
instance FromJSON PaymentStatusResponse where
  parseJSON = undefined

data JsonError = JsonError
  { status       :: Text
  , errorCode    :: Text
  , errorMessage :: Text
  } deriving (Generic, ToJSON)

defaultJsonError = JsonError "invalid_request_error" "nullable" "[merchantId] cannot be null"    