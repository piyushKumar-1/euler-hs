
{-# LANGUAGE DeriveAnyClass #-}

module EulerHS.Framework.Types.API
  ( RestEndpoint(..)
  , APIResult
  , ErrorPayload(..)
  , ErrorResponse
  , Method(..)
  , Header(..)
  , HeaderField
  , HeaderValue
  , Headers(..)
  , Request(..)
  , Response(..)
  , URL
  -- , defaultMakeRequest
  -- , defaultMakeRequest_
  -- , defaultDecodeResponse
  -- , makeRequest
  -- , decodeResponse
  -- , responsePayload
  ) where

import EulerHS.Prelude

-- TODO: port

type APIResult s = Either ErrorResponse s

class RestEndpoint a b | a -> b, b -> a where
  makeRequest :: a -> Headers -> Request
  -- decodeResponse :: String -> F b

type HeaderField = String
type HeaderValue = String

data Header = Header HeaderField HeaderValue
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

newtype Headers = Headers [Header]
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

type URL = String

data Method = POST | GET | PUT | DELETE
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

data Request = Request
  { method :: Method
  , url :: URL
  , payload :: String
  , headers :: Headers
  } deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

data Response a = Response
  { code :: Int
  , status :: String
  , response :: a
  } deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

data ErrorPayload = ErrorPayload
  { error :: Bool
  , errorMessage :: String
  , userMessage :: String
  } deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

type ErrorResponse = Response ErrorPayload
