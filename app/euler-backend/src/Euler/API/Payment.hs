{-# LANGUAGE DeriveAnyClass #-}

-- FIXME wrover: identify the target type and get rid of this
module Euler.API.Payment where

import           EulerHS.Prelude

import           Data.Aeson.Casing (aesonPrefix, camelCase, snakeCase)
import qualified Data.ByteString.Lazy.Char8         as BC
import qualified Data.Text as Text

-- FIXME wrover: how to import all typeclass methods?
import           WebService.ContentType
--import           WebService.ContentType (WrappableJSON)

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

defaultPaymentStatus :: PaymentStatus
defaultPaymentStatus = PaymentStatus 
    17 
    "CHARGED" 
    21 
    ""
    "5ZLTuReaA5f36OKf_1737991" 
    "furlenco" 
    0 
    "https://gringotts.furlenco.com/payments/response?order_id..." 

-- TODO how could it be abstracted better? 
data PaymentStatusResponse = PaymentStatusResponse
    { payload         :: PaymentStatus
    , caseStyle       :: Text.Text          
    , callback        :: Maybe Text.Text 
    }

instance WrappableJSON PaymentStatusResponse where
    wrapper p = case (callback $ p) of
        Just s ->  BC.pack $ Text.unpack $ s
        Nothing -> undefined        

-- TODO move this instance and convertFun to WebService?
instance ToJSON PaymentStatusResponse where
    -- FIXME aesonPrefix removes prefix, it's not good really
    toJSON v = (genericToJSON $ aesonPrefix $ convertFun v) $ payload v

convertFun :: PaymentStatusResponse -> (String -> String)
convertFun p = case (caseStyle p) of
    "camel" -> camelCase
    "snake" -> snakeCase
    otherwise -> undefined

instance FromJSON PaymentStatusResponse where
    parseJSON = undefined
