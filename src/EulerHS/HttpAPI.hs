{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module EulerHS.HttpAPI
    (
      -- * HTTP manager builder stuff
      HTTPClientSettings(..)
    -- remove
    , ProxySettings (..)
    , buildSettings
    , withInsecureProxy
    , withClientTls
    , withCustomCA


    , HTTPRequest(..)
    , HTTPResponse(..)
    , HTTPMethod(..)
    , HTTPCert(..)
    , HTTPRequestResponse(HTTPRequestResponse)
    , HTTPIOException(HTTPIOException)
    , defaultTimeout
    , extractBody
    , httpGet
    , httpPut
    , httpPost
    , httpDelete
    , httpHead
    , defaultRequest
    , withHeader
    , withOptionalHeader
    , withBody
    , withTimeout
    , withRedirects
    , maskHTTPRequest
    , maskHTTPResponse
    ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import           Data.ByteString.Lazy.Builder (Builder)
import qualified Data.ByteString.Lazy.Builder as Builder
import qualified Data.Char as Char
import           Data.Default
import qualified Data.Map as Map
import           Data.String.Conversions (convertString)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import           Data.X509.CertificateStore (CertificateStore, readCertificateStore)
import           Data.X509.Validation (validateDefault)
import qualified EulerHS.BinaryString as T
import qualified EulerHS.Logger.Types as Log
import           EulerHS.Masking (defaultMaskText, getContentTypeForHTTP,
                                  maskHTTPHeaders, parseRequestResponseBody,
                                  shouldMaskKey)
import           EulerHS.Prelude hiding (ord)
import qualified Network.HTTP.Client as HTTP
import qualified Network.Connection as Conn
import qualified Network.HTTP.Client.TLS as TLS
import qualified Network.TLS as TLS
import qualified Network.TLS.Extra.Cipher as TLS
import           System.IO.Unsafe (unsafePerformIO)
import           System.X509 (getSystemCertificateStore)


-- "Creating a new Manager is a relatively expensive operation, you are advised to share a single Manager between requests instead."

-- |
data HTTPClientSettings = HTTPClientSettings
  { httpClientSettingsProxy             :: Last ProxySettings
  , httpClientSettingsClientCertificate :: Last HTTPCert
  , httpClientSettingsCustomStore       :: CertificateStore
  }

data ProxySettings
  = InsecureProxy
  { proxySettingsHost :: Text
  , proxySettingsPort :: Int
  }

instance Semigroup HTTPClientSettings where
  (HTTPClientSettings a1 b1 c1) <> (HTTPClientSettings a2 b2 c2) =
    HTTPClientSettings (a1 <> a2) (b1 <> b2) (c1 <> c2)

instance Monoid HTTPClientSettings where
  mempty = HTTPClientSettings mempty mempty mempty

type ManagerSettingsBuilder = HTTPClientSettings -> HTTP.ManagerSettings

-- | The simplest builder
buildSettings :: ManagerSettingsBuilder
buildSettings HTTPClientSettings{..} =
    HTTP.managerSetProxy proxyOverride $ tlsManagerSettings
  where
    proxyOverride = case getLast httpClientSettingsProxy of
      Just (InsecureProxy host port) -> HTTP.useProxy $ HTTP.Proxy (Text.encodeUtf8 host) port
      Nothing -> HTTP.noProxy

    tlsManagerSettings = case getLast httpClientSettingsClientCertificate of
      Just HTTPCert{..} ->
        case TLS.credentialLoadX509ChainFromMemory getCert getCertChain getCertKey of
          Right creds ->
            let hooks = def { TLS.onCertificateRequest =
                                \_ -> return $ Just creds
                            , TLS.onServerCertificate =
                                \ upstreamStore cache serviceId certChain -> do
                                  store <- fmap (maybe upstreamStore (upstreamStore <>)) $ runMaybeT $
                                      hoistMaybe getTrustedCAs >>= MaybeT . readCertificateStore
                                  validateDefault (sysStore <> store) cache serviceId certChain
                            }
                clientParams = (TLS.defaultParamsClient getCertHost "")
                              { TLS.clientHooks = hooks
                              , TLS.clientSupported = def { TLS.supportedCiphers = TLS.ciphersuite_default }
                              }
                tlsSettings = Conn.TLSSettings clientParams in
            TLS.mkManagerSettings tlsSettings Nothing
          -- TODO?
          Left err -> error $ "cannot load client certificate data: " <> Text.pack err
      Nothing -> TLS.mkManagerSettings def Nothing

    sysStore = memorizedSysStore

{-# NOINLINE memorizedSysStore #-}
memorizedSysStore :: CertificateStore
memorizedSysStore = unsafePerformIO getSystemCertificateStore

-- | Add unconditional proxying (for both http/https, regardless request )
withInsecureProxy :: Text -> Int -> ManagerSettingsBuilder -> HTTP.ManagerSettings
withInsecureProxy host port builder = builder $ mempty {httpClientSettingsProxy = Last $ Just $ InsecureProxy host port}

type ClientCert = B.ByteString
type CertChain = [B.ByteString]
type ClientKey = B.ByteString

-- | Define the name of the server, along with an extra service identification blob.
-- this is important that the hostname part is properly filled for security reason,
-- as it allow to properly associate the remote side with the given certificate
-- during a handshake.
type ServerName = Text

-- | Adds the client certificate to use client TLS authentication
withClientTls :: ClientCert -> CertChain -> ClientKey -> ServerName -> ManagerSettingsBuilder -> HTTP.ManagerSettings
withClientTls cert chain key serverName builder =
    builder $ mempty {httpClientSettingsClientCertificate = Last $ Just $ httpCert}
  where
    httpCert = HTTPCert cert chain (Text.unpack serverName) key Nothing

-- | Adds an additional store with trusted CA certificates
withCustomCA :: CertificateStore -> ManagerSettingsBuilder -> HTTP.ManagerSettings
withCustomCA store builder = builder mempty {httpClientSettingsCustomStore = store}

data HTTPRequest
  = HTTPRequest
    { getRequestMethod    :: HTTPMethod
    , getRequestHeaders   :: Map.Map HeaderName HeaderValue
    , getRequestBody      :: Maybe T.LBinaryString
    , getRequestURL       :: Text
    , getRequestTimeout   :: Maybe Int                        -- ^ timeout, in microseconds
    , getRequestRedirects :: Maybe Int
    }
    deriving (Eq, Ord, Generic, ToJSON)

data HTTPResponse
  = HTTPResponse
    { getResponseBody    :: T.LBinaryString
    , getResponseCode    :: Int
    , getResponseHeaders :: Map.Map HeaderName HeaderValue
    , getResponseStatus  :: Text
    }
    deriving (Show, Eq, Ord, Generic, ToJSON)

data HTTPCert
  = HTTPCert
    { getCert       :: B.ByteString
    , getCertChain  :: [B.ByteString]
    , getCertHost   :: String
    , getCertKey    :: B.ByteString
    , getTrustedCAs :: Maybe FilePath  -- ^ optional store (either a file with certs in PEM format
                                       -- ^ or a directory containing such files)
    }

data HTTPMethod
  = Get
  | Put
  | Post
  | Delete
  | Head
  | Trace
  | Connect
  | Options
  | Patch
  deriving (Eq, Ord, Generic, ToJSON)

type HeaderName = Text
type HeaderValue = Text

data HTTPRequestResponse
  = HTTPRequestResponse
    { request  :: HTTPRequest
    , response :: HTTPResponse
    }
  deriving (Eq, Ord, Generic, ToJSON)

-- | Used when some IO (or other) exception ocurred during a request
data HTTPIOException
  = HTTPIOException
    { errorMessage :: Text
    , request      :: HTTPRequest
    }
  deriving (Eq, Ord, Generic, ToJSON)


-- Not Used anywhere
-- getMaybeUtf8 :: T.LBinaryString -> Maybe LazyText.Text
-- getMaybeUtf8 body = case LazyText.decodeUtf8' (T.getLBinaryString body) of
--   -- return request body as base64-encoded text (not valid UTF-8)
--   Left e -> Nothing
--   -- return request body as UTF-8 decoded text
--   Right utf8Body -> Just utf8Body



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
defaultRequest method url
  = HTTPRequest
    { getRequestMethod = method
    , getRequestHeaders = Map.empty
    , getRequestBody = Nothing
    , getRequestURL = url
    , getRequestTimeout = Just defaultTimeout
    , getRequestRedirects = Just 10
    }

defaultTimeout :: Int
defaultTimeout = 9000000

-- | Add a header to an HTTPRequest
--
--  > httpGet "https://google.com"
--  >   & withHeader "Content-Type" "application/json"
--
withHeader :: HeaderName -> HeaderValue -> HTTPRequest -> HTTPRequest
withHeader headerName headerValue request@HTTPRequest {getRequestHeaders} =
  let headers = Map.insert headerName headerValue getRequestHeaders
  in  request { getRequestHeaders = headers }

withOptionalHeader :: HeaderName -> Maybe HeaderValue -> HTTPRequest -> HTTPRequest
withOptionalHeader headerName (Just headerValue) = withHeader headerName headerValue
withOptionalHeader _ Nothing = id

-- | Sets timeout, in microseconds
withTimeout :: Int -> HTTPRequest -> HTTPRequest
withTimeout timeout request =
  request {getRequestTimeout = Just timeout}

withRedirects :: Int -> HTTPRequest -> HTTPRequest
withRedirects redirects request =
  request {getRequestRedirects = Just redirects}

-- TODO: Rename to `withFormData` or some such?
withBody :: [(Text, Text)] -> HTTPRequest -> HTTPRequest
withBody pairs request = request {getRequestBody = Just body}
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

maskHTTPRequest :: Maybe Log.LogMaskingConfig -> HTTPRequest -> HTTPRequest
maskHTTPRequest mbMaskConfig request =
  request
    { getRequestHeaders = maskHTTPHeaders (shouldMaskKey mbMaskConfig) getMaskText requestHeaders
    , getRequestBody = maskedRequestBody
    }
  where
    requestHeaders = getRequestHeaders request

    requestBody = getRequestBody request

    getMaskText = maybe defaultMaskText (fromMaybe defaultMaskText . Log._maskText) mbMaskConfig

    maskedRequestBody =
      T.LBinaryString
        . encodeUtf8
        . parseRequestResponseBody (shouldMaskKey mbMaskConfig) getMaskText (getContentTypeForHTTP requestHeaders)
        . LB.toStrict
        . T.getLBinaryString <$> requestBody

maskHTTPResponse :: Maybe Log.LogMaskingConfig -> HTTPResponse -> HTTPResponse
maskHTTPResponse mbMaskConfig response =
  response
    { getResponseHeaders = maskHTTPHeaders (shouldMaskKey mbMaskConfig) getMaskText responseHeaders
    , getResponseBody = maskedResponseBody
    }
  where
    responseHeaders = getResponseHeaders response

    responseBody = getResponseBody response

    getMaskText = maybe defaultMaskText (fromMaybe defaultMaskText . Log._maskText) mbMaskConfig

    maskedResponseBody =
      T.LBinaryString
        . encodeUtf8
        . parseRequestResponseBody (shouldMaskKey mbMaskConfig) getMaskText (getContentTypeForHTTP responseHeaders)
        . LB.toStrict
        $ T.getLBinaryString responseBody
