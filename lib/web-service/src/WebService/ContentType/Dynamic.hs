{-# LANGUAGE OverloadedStrings #-}

module WebService.ContentType.Dynamic
(chooseContentType) where

import           EulerHS.Prelude

import qualified Data.ByteString.UTF8     as BS
import           Data.List 
                 (filter, lookup)
import           Network.Wai 
                 (Middleware, requestHeaders, queryString)
import           Servant.API 
                 (JSON, contentType)
import qualified Servant.API.ContentTypes as ContentTypes
import           WebService.ContentType.Javascript

-- accept header (could it be taken from libraries?)
-- accept = "Accept" :: ByteString
-- driving parameter name 
-- callbackParam = "callback" :: ByteString


-- middleware to choose content type
chooseContentType :: Middleware
chooseContentType app req =
    app req'
  where
    req' = case join $ lookup "callback" $ queryString req of
        Nothing       -> req { 
                requestHeaders = changeVal "Accept" (contentType' (Proxy :: Proxy JSON)) $ headers
                }
        Just _        -> req { 
                requestHeaders = changeVal "Accept" (contentType' (Proxy :: Proxy JavascriptWrappedJSON)) $ headers
                }

    changeVal :: Eq a
            => a
            -> ByteString
            -> [(a, ByteString)]
            -> [(a, ByteString)]
    changeVal key val old = (key, val)
                        : Data.List.filter (\(k, _) -> k /= key) old
    -- FIXME is show appropriate here? string representation of content/type
    contentType' :: (ContentTypes.Accept a) => Proxy a -> ByteString
    contentType' = BS.fromString . show . contentType
    headers = requestHeaders req
