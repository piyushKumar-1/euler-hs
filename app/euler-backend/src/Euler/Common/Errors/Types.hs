{-# LANGUAGE DeriveAnyClass #-}

module Euler.Common.Errors.Types where

import EulerHS.Prelude

import qualified Data.ByteString.Lazy     as BSL

-- from src/Engineering/Types/API.purs

data ErrorResponse = ErrorResponse
   { code     :: Int
   , response :: ErrorPayload
   }
  deriving (Eq, Show, Generic)

instance Exception ErrorResponse


data ErrorPayload = ErrorPayload
  { error        :: Bool
  , error_message :: String
  , userMessage  :: String
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)


data ECErrorResponse = ECErrorResponse
  { code     :: Int
  , response :: BSL.ByteString
  }
  deriving (Eq, Show, Generic)

instance Exception ECErrorResponse


data ECErrorPayload = ECErrorPayload
  { status       :: String
  , error_code    :: Maybe String
  , error_message :: Maybe String
  , status_id     :: Maybe Int
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)


data ECOrderStatusErrorPayload = ECOrderStatusErrorPayload
  { status       :: String
  , error_code    :: Maybe String
  , error_message :: Maybe String
  , status_id     :: Maybe Int
  , order_id      :: Maybe String
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)


data ECTxnStatusErrorPayload = ECTxnStatusErrorPayload
  { status       :: String
  , error_code    :: Maybe String
  , error_message :: Maybe String
  , status_id     :: Maybe Int
  , txn_uuid      :: Maybe String
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)


data TxnValidationErrorResp = TxnValidationErrorResp
  { error_code    :: String
  , error_message :: String
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

