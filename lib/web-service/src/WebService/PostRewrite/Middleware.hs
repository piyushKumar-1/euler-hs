-----------------------------------------------------------------------------
-- | Module : WebService.MethodRewrite.Middleware
--
-- Middleware to convert presumably short x-www-form-urlencoded POST request
-- to GET. This makes use of WebService.FlexCasing and WebService.ContentType
-- features possible for POST requests alike.
-----------------------------------------------------------------------------
module WebService.PostRewrite.Middleware
  (
    mkPostToGetMiddleware
  ) where

import           EulerHS.Prelude

import           Data.ByteString.Lazy (toChunks)
import           Data.List (lookup)
import           Network.HTTP.Types 
                (parseQuery)
import           Network.HTTP.Types.Header                 
                (hContentType)     
import           Network.HTTP.Types.Method (methodGet)                           
import           Network.Wai 
                (Middleware, Request, queryString, requestMethod, requestHeaders, lazyRequestBody)

-----------------------------------------------------------------------------
mkPostToGetMiddleware :: Middleware
mkPostToGetMiddleware app req send =
    case (requestMethod req, lookup hContentType (requestHeaders req)) of
      ("POST", Just "application/x-www-form-urlencoded") -> convert req >>= flip app send
      _                                                  -> app req send

-----------------------------------------------------------------------------
convert :: Request -> IO Request
convert req = do
  body <- (mconcat . toChunks) `fmap` lazyRequestBody req
  return $ req { requestMethod = methodGet, queryString = parseQuery body}