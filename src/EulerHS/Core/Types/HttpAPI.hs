{-# LANGUAGE DeriveAnyClass #-}

module EulerHS.Core.Types.HttpAPI
    (
    -- * Core Logger
    -- ** Types
      HTTPRequest(..)
    , HTTPResponse(..)
    , HTTPMethod(..)
    , httpGet
    , httpPut
    , httpPost
    , httpDelete
    , httpHead
    ) where

import           EulerHS.Prelude

import qualified Data.ByteString.Lazy as Lazy
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding

import qualified EulerHS.Core.Types.BinaryString as T

data HTTPRequest
  = HTTPRequest
    { getRequestMethod  :: HTTPMethod
    , getRequestHeaders :: Map.Map HeaderName HeaderValue
    , getRequestBody    :: Maybe T.LBinaryString
    , getRequestURL     :: Text.Text
    }
    deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

data HTTPResponse
  = HTTPResponse
    { getResponseBody    :: T.LBinaryString
    , getResponseCode    :: Int
    , getResponseHeaders :: Map.Map HeaderName HeaderValue
    , getResponseStatus  :: Text.Text
    }
    deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

data HTTPMethod
  = Get
  | Put
  | Post
  | Delete
  | Head
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

type HeaderName = Text.Text
type HeaderValue = Text.Text

--------------------------------------------------------------------------
-- Convenience functions
--------------------------------------------------------------------------

-- | HTTP GET request.
--
-- > httpGet "https://google.com"
httpGet :: Text.Text -> HTTPRequest
httpGet = HTTPRequest Get Map.empty Nothing

httpPut :: Text.Text -> HTTPRequest
httpPut = HTTPRequest Put Map.empty Nothing

httpPost :: Text.Text -> HTTPRequest
httpPost = HTTPRequest Post Map.empty Nothing

httpDelete :: Text.Text -> HTTPRequest
httpDelete = HTTPRequest Delete Map.empty Nothing

httpHead :: Text.Text -> HTTPRequest
httpHead = HTTPRequest Head Map.empty Nothing

-- | Add a header to an HTTPRequest
--
--  > httpGet "https://google.com"
--  >   & withHeader "Content-Type" "application/json"
--
withHeader :: Text.Text -> Text.Text -> HTTPRequest -> HTTPRequest
withHeader headerName headerValue (request@HTTPRequest {getRequestHeaders}) =
  let headers = Map.insert headerName headerValue getRequestHeaders
  in  request { getRequestHeaders = headers }
