module Euler.Common.Errors.ErrorsMapping
where

import EulerHS.Prelude

import qualified Data.Aeson.Encode.Pretty as A   (encodePretty)
import qualified Servant.Server as SE

import qualified Euler.Common.Errors.Types as ET


class (Exception a, Exception b) => MappableErrors a b where
  mapError :: a -> b

instance MappableErrors ET.ErrorResponse SE.ServerError where
  mapError ET.ErrorResponse {..} = SE.ServerError
    { errHTTPCode = code
    , errReasonPhrase = ""
    , errBody         = A.encodePretty response
    , errHeaders      =[]
    }

instance MappableErrors ET.ECErrorResponse SE.ServerError where
  mapError ET.ECErrorResponse {..} = SE.ServerError
    { errHTTPCode = code
    , errReasonPhrase = ""
    , errBody         = response
    , errHeaders      =[]
    }