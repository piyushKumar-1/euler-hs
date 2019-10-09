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
newtype Headers = Headers [Header]

type URL = String

data Method = POST | GET | PUT | DELETE

data Request = Request
  { method :: Method
  , url :: URL
  , payload :: String
  , headers :: Headers
  }

data Response a = Response
  { code :: Int
  , status :: String
  , response :: a
  }

data ErrorPayload = ErrorPayload
  { error :: Bool
  , errorMessage :: String
  , userMessage :: String
  }

type ErrorResponse = Response ErrorPayload
