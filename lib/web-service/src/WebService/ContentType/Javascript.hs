{-# LANGUAGE OverloadedStrings #-}

module WebService.ContentType.Javascript where

import           EulerHS.Prelude

import           Data.Aeson (encode)
import qualified Data.ByteString.Lazy.Char8 as BC
import           Network.HTTP.Media ((//))

-- FIXME why explicit import breaks code?
import           Servant.API
--import           Servant.API (Accept, MimeRender)

-- typeclass for responses that could be wrapped with a JS wrapping code
class WrappableJSON a where
    wrapper :: a -> BC.ByteString

-- content type for application/javascript wrapper for JSON structure
data JavascriptWrappedJSON deriving Typeable

-- | @application/javascript@
instance Accept JavascriptWrappedJSON where
    contentType _ = "application" // "javascript"

instance (ToJSON o, WrappableJSON o)=> MimeRender JavascriptWrappedJSON o where
    mimeRender _ o = (wrapper o) <> "(\n" <> "    " <> encode o <> "\n)"

