{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------
-- | Module : WebService.ContentType.Dynamic
--
-- Middleware to enable dynamic content-type choosing based on presence of
-- the driving parameter.
-----------------------------------------------------------------------------
module WebService.ContentType.Dynamic
  (
    mkDynContentTypeMiddleware
  ) where

import           EulerHS.Prelude

import qualified Data.ByteString.UTF8     as BS
import           Data.List 
                (filter, lookup)
import           Network.HTTP.Types.Header                 
                (hAccept)
import           Network.Wai 
                (Middleware, requestHeaders, queryString)
import           Servant.API 
                (JSON, contentType)
import qualified Servant.API.ContentTypes as ContentTypes
import           Text.Casing 
                (camel)
import           WebService.ContentType.Javascript

-----------------------------------------------------------------------------

-- | Middleware builder
mkDynContentTypeMiddleware :: String -> Middleware
mkDynContentTypeMiddleware p app req =
    app req'
  where
    p' = camel p
    req' = case join $ lookup (BS.fromString p') $ queryString req of
      Nothing -> req 
        { requestHeaders = changeVal hAccept (contentType' (Proxy :: Proxy JSON)) $ headers 
        }
      Just _  -> req 
        { requestHeaders = 
          changeVal hAccept (contentType' (Proxy :: Proxy JavascriptWrappedJSON)) $ headers                
        }

    changeVal :: Eq a
            => a
            -> ByteString
            -> [(a, ByteString)]
            -> [(a, ByteString)]
    changeVal key val old = (key, val) : filter (\(k, _) -> k /= key) old
    
    contentType' :: (ContentTypes.Accept a) => Proxy a -> ByteString
    contentType' = BS.fromString . show . contentType
    headers = requestHeaders req
