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

import           EulerHS.Prelude hiding ((.=))

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

-- instance ToJSON HTTPRequest where
--   toJSON request =
--     Aeson.object
--       [ "getRequestMethod"    Aeson..= getRequestMethod request
--       , "getRequestHeaders"   Aeson..= getRequestHeaders request
--       , "getRequestBody"      Aeson..= getRequestBody request
--       , "getRequestURL"       Aeson..= getRequestURL request
--       , "getRequestTimeout"   Aeson..= getRequestTimeout request
--       , "getRequestRedirects" Aeson..= getRequestRedirects request
--       , "utf8Body"            Aeson..= (getMaybeUtf8 <$> getRequestBody request)
--       ]

data HTTPResponse
  = HTTPResponse
    { getResponseBody    :: T.LBinaryString
    , getResponseCode    :: Int
    , getResponseHeaders :: Map.Map HeaderName HeaderValue
    , getResponseStatus  :: Text.Text
    }
    deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

-- instance ToJSON HTTPResponse where
--   toJSON response =
--     Aeson.object
--       [ "getResponseBody"    Aeson..= getResponseBody response
--       , "getResponseCode"    Aeson..= getResponseCode response
--       , "getResponseHeaders" Aeson..= getResponseHeaders response
--       , "getResponseStatus"  Aeson..= getResponseStatus response
--       , "utf8Body"           Aeson..= getMaybeUtf8 (getResponseBody response)
--       ]

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


getMaybeUtf8 :: T.LBinaryString -> Maybe LazyText.Text
getMaybeUtf8 body = case LazyText.decodeUtf8' (T.getLBinaryString body) of
  -- return request body as base64-encoded text (not valid UTF-8)
  Left e -> Nothing
  -- return request body as UTF-8 decoded text
  Right utf8Body -> Just utf8Body



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
