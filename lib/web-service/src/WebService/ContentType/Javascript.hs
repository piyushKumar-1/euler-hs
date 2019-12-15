{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------
-- | Module : WebService.ContentType.Javascript
--
-- Content type and typeclass for JS-wrapped responses.  
-----------------------------------------------------------------------------
module WebService.ContentType.Javascript where

import           EulerHS.Prelude

import           Data.Aeson 
                (encode)
import qualified Data.ByteString.Lazy.Char8 as BC
import           Network.HTTP.Media ((//), (/:))
import           Servant.API 
                (Accept, MimeRender, contentType, mimeRender)

-- typeclass for responses that could be wrapped by JS code
class WrappableJSON a where
  wrapper :: a -> BC.ByteString

-- content type for application/javascript wrapper for JSON structure
data JavascriptWrappedJSON deriving Typeable

-- | @application/javascript@
instance Accept JavascriptWrappedJSON where
  contentType _ = "application" // "javascript" /: ("charset", "utf-8")
    
instance (ToJSON o, WrappableJSON o)=> MimeRender JavascriptWrappedJSON o where
  mimeRender _ o = (wrapper o) <> "(\n" <> "    " <> encode o <> "\n)"
