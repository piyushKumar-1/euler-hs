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
    , withHeader
    , withOptionalHeader
    , withBody
    , withTimeout
    , withRedirects
    , extractBody
    ) where

import           EulerHS.Prelude                 hiding ((.=), ord)
import qualified EulerHS.Core.Types.BinaryString as T

import qualified Data.Aeson                      as Aeson
import qualified Data.ByteString                 as B
import qualified Data.ByteString.Char8           as B8
import qualified Data.ByteString.Lazy            as LB
import           Data.ByteString.Lazy.Builder    (Builder)
import qualified Data.ByteString.Lazy.Builder    as Builder
import           Data.Char                       hiding (ord)
import qualified Data.Char                       as Char
import qualified Data.Map                        as Map
import           Data.String.Conversions         (convertString)
import           Data.Text                       (Text)
import qualified Data.Text                       as Text
import qualified Data.Text.Encoding              as Text
import           Data.Text.Encoding              (decodeUtf8With)
import           Data.Text.Encoding.Error        (lenientDecode)
import qualified Data.Text.Lazy                  as LazyText
import qualified Data.Text.Lazy.Encoding         as LazyText

data HTTPRequest
  = HTTPRequest
    { getRequestMethod    :: HTTPMethod
    , getRequestHeaders   :: Map.Map HeaderName HeaderValue
    , getRequestBody      :: Maybe T.LBinaryString
    , getRequestURL       :: Text
    , getRequestTimeout   :: Maybe Int                        -- ^ timeout, in microseconds
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
    , getResponseStatus  :: Text
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

type HeaderName = Text
type HeaderValue = Text

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
httpGet :: Text -> HTTPRequest
httpGet = defaultRequest Get

httpPut :: Text -> HTTPRequest
httpPut = defaultRequest Put

httpPost :: Text -> HTTPRequest
httpPost = defaultRequest Post

httpDelete :: Text -> HTTPRequest
httpDelete = defaultRequest Delete

httpHead :: Text -> HTTPRequest
httpHead = defaultRequest Head

defaultRequest :: HTTPMethod -> Text -> HTTPRequest
defaultRequest method url = HTTPRequest method Map.empty Nothing url Nothing Nothing

-- | Add a header to an HTTPRequest
--
--  > httpGet "https://google.com"
--  >   & withHeader "Content-Type" "application/json"
--
withHeader :: Text -> Text -> HTTPRequest -> HTTPRequest
withHeader headerName headerValue (request@HTTPRequest {getRequestHeaders}) =
  let headers = Map.insert headerName headerValue getRequestHeaders
  in  request { getRequestHeaders = headers }

withOptionalHeader :: Text -> Maybe Text -> HTTPRequest -> HTTPRequest
withOptionalHeader headerName (Just headerValue) = withHeader headerName headerValue
withOptionalHeader _ Nothing = id

-- | Sets timeout, in microseconds
withTimeout :: Int -> HTTPRequest -> HTTPRequest
withTimeout timeout (request@HTTPRequest {getRequestTimeout}) =
  request {getRequestTimeout = Just timeout}

withRedirects :: Int -> HTTPRequest -> HTTPRequest
withRedirects redirects (request@HTTPRequest {getRequestRedirects}) =
  request {getRequestRedirects = Just redirects}

withBody :: [(Text, Text)] -> HTTPRequest -> HTTPRequest
withBody pairs (request@HTTPRequest {getRequestBody}) = request {getRequestBody = Just body}
  where
    body = T.LBinaryString $ formUrlEncode pairs

extractBody :: HTTPResponse -> Text
extractBody HTTPResponse{getResponseBody} = decodeUtf8With lenientDecode $ convertString getResponseBody

formUrlEncode :: [(Text, Text)] -> LB.ByteString
formUrlEncode = Builder.toLazyByteString . mconcat . intersperse amp . map encodePair
  where
    equals = Builder.word8 (ord '=')
    amp = Builder.word8 (ord '&')
    percent = Builder.word8 (ord '%')
    plus = Builder.word8 (ord '+')

    encodePair :: (Text, Text) -> Builder
    encodePair (key, value) = encode key <> equals <> encode value

    encode :: Text -> Builder
    encode = escape . Text.encodeUtf8

    escape :: ByteString -> Builder
    escape = mconcat . map f . B.unpack
      where
        f :: Word8 -> Builder
        f c
          | p c = Builder.word8 c
          | c == ord ' ' = plus
          | otherwise = percentEncode c

        p :: Word8 -> Bool
        p c =
             ord 'a' <= c && c <= ord 'z'
          || c == ord '_'
          || c == ord '*'
          || c == ord '-'
          || c == ord '.'
          || ord '0' <= c && c <= ord '9'
          || ord 'A' <= c && c <= ord 'Z'

    ord :: Char -> Word8
    ord = fromIntegral . Char.ord

    percentEncode :: Word8 -> Builder
    percentEncode n = percent <> hex hi <> hex lo
      where
        (hi, lo) = n `divMod` 16

    hex :: Word8 -> Builder
    hex n = Builder.word8 (offset + n)
      where
        offset
          | n < 10    = 48
          | otherwise = 55