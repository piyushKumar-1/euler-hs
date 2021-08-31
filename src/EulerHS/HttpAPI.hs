{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DerivingStrategies #-}

module EulerHS.HttpAPI
    (
      -- * HTTP manager builder stuff
      HTTPClientSettings
    , buildSettings
    , withProxy
    , withMbProxy
    , withClientTls
    , withMbClientTls
    , withCustomCA
      -- * Common types and convenience methods
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
import           Data.X509 (encodeSignedObject, getCertificate, Certificate (certSerial), HashALG(..))
import           Data.X509.CertificateStore (CertificateStore, listCertificates)
import           Data.X509.Validation (validate, defaultHooks, defaultChecks, checkLeafV3)
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
import           Generics.Deriving.Monoid (memptydefault, mappenddefault)

newtype CertificateStore'
  = CertificateStore'
    { getCertificateStore :: CertificateStore
    }
  deriving newtype (Semigroup, Monoid)

instance Eq CertificateStore' where
  (==) a b = a' == b' where
    a' = sortedSignedObjects a
    b' = sortedSignedObjects b

sortedSignedObjects :: CertificateStore' -> [ByteString]
sortedSignedObjects = sort . fmap encodeSignedObject . listCertificates . getCertificateStore

instance Ord CertificateStore' where
  compare a b = a' `compare` b' where
    a' = sortedSignedObjects a
    b' = sortedSignedObjects b

instance Hashable CertificateStore' where
  hashWithSalt salt = hashWithSalt salt .
    fmap (certSerial . getCertificate) . listCertificates . getCertificateStore


-- |
data HTTPClientSettings = HTTPClientSettings
  { httpClientSettingsProxy             :: Last ProxySettings
  , httpClientSettingsClientCertificate :: Last HTTPCert
  , httpClientSettingsCustomStore       :: CertificateStore'
  }
  deriving stock (Eq, Ord, Generic)
  -- use DeriveVia?
  -- see https://hackage.haskell.org/package/generic-deriving-1.14/docs/Generics-Deriving-Default.html

instance Hashable HTTPClientSettings where
  hashWithSalt salt settings = hashWithSalt salt $
    ( getLast $ httpClientSettingsProxy             settings
    , getLast $ httpClientSettingsClientCertificate settings
    , httpClientSettingsCustomStore settings
    )


-- instance Hashable a => Hashable (Last a) where
--   hashWithSalt salt = hashWithSalt salt . getLast

data ProxySettings
  = InsecureProxy
  { proxySettingsHost :: Text
  , proxySettingsPort :: Int
  }
  deriving stock (Eq, Ord, Generic)

instance Hashable ProxySettings


instance Semigroup (HTTPClientSettings) where
  (<>)  = mappenddefault

instance Monoid (HTTPClientSettings) where
  mempty  = memptydefault

-- | The simplest settings builder
buildSettings :: HTTPClientSettings -> HTTP.ManagerSettings
buildSettings HTTPClientSettings{..} =
    applyProxySettings $ baseSettings
  where
    applyProxySettings = HTTP.managerSetProxy proxyOverride

    proxyOverride = case getLast httpClientSettingsProxy of
      Just (InsecureProxy host port) -> HTTP.useProxy $ HTTP.Proxy (Text.encodeUtf8 host) port
      Nothing -> HTTP.noProxy

    baseSettings = case getLast httpClientSettingsClientCertificate of
      Just HTTPCert{..} ->
        case TLS.credentialLoadX509ChainFromMemory getCert getCertChain getCertKey of
          Right creds ->
            let hooks = def { TLS.onCertificateRequest =
                                \_ -> return $ Just creds
                            }
                clientParams = mkClientParams hooks
            in mkSettings clientParams
          -- TODO?
          Left err -> error $ "cannot load client certificate data: " <> Text.pack err
      Nothing ->
        let clientParams = mkClientParams def
        in mkSettings clientParams

    mkClientParams hooks =
      let defs = TLS.defaultParamsClient empty ""
          _ = hooks {
                  TLS.onServerCertificate = validate HashSHA256 defaultHooks (defaultChecks { checkLeafV3 = False})
              }
      in
        defs
          { TLS.clientShared = (TLS.clientShared defs) { TLS.sharedCAStore = sysStore <> getCertificateStore httpClientSettingsCustomStore }
          , TLS.clientSupported = (TLS.clientSupported defs) { TLS.supportedCiphers = TLS.ciphersuite_default }
          , TLS.clientHooks = hooks
          }

    mkSettings clientParams = let
        tlsSettings = Conn.TLSSettings clientParams
      in
        TLS.mkManagerSettings tlsSettings Nothing

    sysStore = memorizedSysStore

{-# NOINLINE memorizedSysStore #-}
memorizedSysStore :: CertificateStore
memorizedSysStore = unsafePerformIO getSystemCertificateStore

type SimpleProxySettings = (Text, Int)

-- | Add unconditional proxying (for both http/https, regardless 
-- HTTP.Client's request proxy settings).
withProxy :: SimpleProxySettings -> HTTPClientSettings
withProxy (host, port) =
    mempty {httpClientSettingsProxy = Last $ proxySettings}
  where
    proxySettings = Just $ InsecureProxy host port

-- | The same as 'withProxy' but to use with optionally existsting settings.
withMbProxy :: Maybe SimpleProxySettings -> HTTPClientSettings
withMbProxy (Just s) = withProxy s
withMbProxy Nothing = mempty

-- | Adds a client certificate to do client's TLS authentication
withClientTls :: HTTPCert -> HTTPClientSettings
withClientTls httpCert =
    mempty {httpClientSettingsClientCertificate = Last $ Just $ httpCert}

withMbClientTls :: Maybe HTTPCert -> HTTPClientSettings
withMbClientTls (Just cert) = withClientTls cert
withMbClientTls Nothing = mempty

-- | Adds an additional store with trusted CA certificates. There is no Maybe version
-- since 'CertificateStore` is a monoid.
withCustomCA :: CertificateStore -> HTTPClientSettings
withCustomCA store = mempty {httpClientSettingsCustomStore = CertificateStore' store}

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
    , getCertHost   :: String        -- ^ Defines the name of the server, along with an extra
                                     -- ^ service identification blob (not supported in Euler ATM).
                                     -- ^ This is important that the hostname part is properly
                                     -- ^ filled for security reason, as it allows to properly
                                     -- ^ as it allow to properly associate the remote side
                                     -- ^ with the given certificate during a handshake.
    , getCertKey    :: B.ByteString
    }
    deriving stock (Eq, Ord, Generic)

instance Hashable HTTPCert

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
