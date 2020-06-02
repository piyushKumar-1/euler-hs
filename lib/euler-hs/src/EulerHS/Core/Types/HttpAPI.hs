{-# LANGUAGE DeriveAnyClass #-}

module EulerHS.Core.Types.HttpAPI
    (
    -- * Core Logger
    -- ** Types
      HTTPRequest(..)
    , HTTPResponse(..)
    , HTTPMethod(..)
    ) where

import EulerHS.Prelude

import qualified Data.Text            as Text
import qualified Data.Text.Encoding   as Encoding
import qualified Data.ByteString.Lazy as Lazy

data HTTPRequest
  = HTTPRequest
    { getRequestMethod  :: HTTPMethod
    , getRequestHeaders :: [(Text.Text, Text.Text)]
    , getRequestURL     :: Text.Text
    }
    deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

data HTTPResponse
  = HTTPResponse
    { getResponseStatus :: Int
    , getResponseBody   :: BinaryString
    }
    deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

data HTTPMethod
  = Get
  | Put
  | Post
  | Delete
  | Head
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

newtype BinaryString 
  = BinaryString
    { getBinaryString :: Lazy.ByteString }
    deriving (Show, Eq, Ord)

instance ToJSON BinaryString where
  toJSON = toJSON . Encoding.decodeUtf8 . Lazy.toStrict . getBinaryString

instance FromJSON BinaryString where
  -- parseJSON = BinaryString . Encoding.encodeUtf8 . parseJSON
  parseJSON = undefined

--------------------------------------------------------------------------
-- Convenience functions
--------------------------------------------------------------------------

-- | HTTP GET request.
--
-- > httpGet "https://google.com"
httpGet :: Text.Text -> HTTPRequest
httpGet = HTTPRequest Get []

httpPut :: Text.Text -> HTTPRequest
httpPut = HTTPRequest Put []

httpPost :: Text.Text -> HTTPRequest
httpPost = HTTPRequest Post []

httpDelete :: Text.Text -> HTTPRequest
httpDelete = HTTPRequest Delete []

httpHead :: Text.Text -> HTTPRequest
httpHead = HTTPRequest Head []

-- | Add a header to an HTTPRequest
--
--  > httpGet "https://google.com"
--  >   & withHeader "Content-Type" "application/json"
--
withHeader :: Text.Text -> Text.Text -> HTTPRequest -> HTTPRequest
withHeader headerName headerValue (HTTPRequest method headers url)
  = HTTPRequest method ((headerName, headerValue):headers) url
