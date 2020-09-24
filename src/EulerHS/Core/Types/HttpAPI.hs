{-# LANGUAGE DeriveAnyClass #-}

module EulerHS.Core.Types.HttpAPI
    (
    -- * Core Logger
    -- ** Types
      HTTPRequest(..)
    , HTTPResponse(..)
    , HTTPMethod(..)
    , HTTPCert(..)
    , HTTPRequestResponse(HTTPRequestResponse)
    , HTTPIOException(HTTPIOException)
    , httpGet
    , httpPut
    , httpPost
    , httpDelete
    , httpHead
    ) where

import           EulerHS.Prelude

import qualified Data.Aeson as Aeson
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Lazy as LazyText
import qualified Data.Text.Lazy.Encoding as LazyText

import qualified EulerHS.Core.Types.BinaryString as T

data HTTPRequest
  = HTTPRequest
    { getRequestMethod  :: HTTPMethod
    , getRequestHeaders :: Map.Map HeaderName HeaderValue
    , getRequestBody    :: Maybe T.LBinaryString
    , getRequestURL     :: Text.Text
    , getRequestTimeout :: Maybe Int
    , getRequestRedirects :: Maybe Int
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

data HTTPCert
  = HTTPCert
    { getCert      :: B.ByteString
    , getCertChain :: [B.ByteString]
    , getCertHost  :: String
    , getCertKey   :: B.ByteString
    }

data HTTPMethod
  = Get
  | Put
  | Post
  | Delete
  | Head
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

type HeaderName = Text.Text
type HeaderValue = Text.Text

data HTTPRequestResponse
  = HTTPRequestResponse
    { request  :: HTTPRequest
    , response :: HTTPResponse
    }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

-- | Used when some IO (or other) exception ocurred during a request
data HTTPIOException
  = HTTPIOException
    { errorMessage :: Text
    , request      :: HTTPRequest
    }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

-- showRequest :: HTTPRequest -> Text
-- showRequest request
--   = jsonSetField "getRequestBody" requestBody $ Aeson.toJSON request
--   where
--     requestBody :: Aeson.Value
--     requestBody = maybe mempty convertBody requestBodyBS

--     convertBody :: LB.ByteString -> HashMap.HashMap Text Aeson.Value
--     convertBody body = case LazyText.decodeUtf8' body of
--       Left e -> mempty
--       Right body -> HashMap.singleton [ "getRequestBody" Aeson..= body ]

--     requestBodyBS :: Maybe LB.ByteString
--     requestBodyBS = T.getLBinaryString <$> getRequestBody request


--------------------------------------------------------------------------
-- Convenience functions
--------------------------------------------------------------------------

-- | HTTP GET request.
--
-- > httpGet "https://google.com"
httpGet :: Text.Text -> HTTPRequest
httpGet = defaultRequest Get

httpPut :: Text.Text -> HTTPRequest
httpPut = defaultRequest Put

httpPost :: Text.Text -> HTTPRequest
httpPost = defaultRequest Post

httpDelete :: Text.Text -> HTTPRequest
httpDelete = defaultRequest Delete

httpHead :: Text.Text -> HTTPRequest
httpHead = defaultRequest Head

defaultRequest :: HTTPMethod -> Text.Text -> HTTPRequest
defaultRequest method url = HTTPRequest method Map.empty Nothing url Nothing Nothing

-- | Add a header to an HTTPRequest
--
--  > httpGet "https://google.com"
--  >   & withHeader "Content-Type" "application/json"
--
withHeader :: Text.Text -> Text.Text -> HTTPRequest -> HTTPRequest
withHeader headerName headerValue (request@HTTPRequest {getRequestHeaders}) =
  let headers = Map.insert headerName headerValue getRequestHeaders
  in  request { getRequestHeaders = headers }
