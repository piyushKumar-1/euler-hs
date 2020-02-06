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
  { error         :: Bool
  , error_message :: Text
  , userMessage   :: Text
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)


data ECErrorResponse = ECErrorResponse
  { code     :: Int
  , response :: BSL.ByteString
  }
  deriving (Eq, Show, Generic)

instance Exception ECErrorResponse


data ECErrorPayload = ECErrorPayload
  { status        :: Text
  , error_code    :: Maybe Text
  , error_message :: Maybe Text
  , status_id     :: Maybe Int
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)


data ECOrderStatusErrorPayload = ECOrderStatusErrorPayload
  { status        :: Text
  , error_code    :: Maybe Text
  , error_message :: Maybe Text
  , status_id     :: Maybe Int
  , order_id      :: Maybe Text
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)


data ECTxnStatusErrorPayload = ECTxnStatusErrorPayload
  { status        :: Text
  , error_code    :: Maybe Text
  , error_message :: Maybe Text
  , status_id     :: Maybe Int
  , txn_uuid      :: Maybe Text
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)


data TxnValidationErrorResp = TxnValidationErrorResp
  { error_code    :: Text
  , error_message :: Text
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

