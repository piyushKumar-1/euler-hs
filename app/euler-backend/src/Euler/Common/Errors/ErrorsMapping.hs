module Euler.Common.Errors.ErrorsMapping
where

import EulerHS.Prelude

import qualified Control.Exception.Safe   as CES (catches, Handler(..), throwIO)
import qualified Data.Aeson.Encode.Pretty as A   (encodePretty)
import qualified Servant as SE

import qualified Euler.Common.Errors.Types as ET


handlers :: [CES.Handler IO a]
handlers = [ CES.Handler (\ (ex :: ET.ErrorResponse)   -> CES.throwIO $ handleErrorResponse   ex)
           , CES.Handler (\ (ex :: ET.ECErrorResponse) -> CES.throwIO $ handleECErrorResponse ex)
           , CES.Handler (\ (ex :: SE.ServerError)     -> CES.throwIO ex)
           , CES.Handler (\ (ex :: SomeException)      -> CES.throwIO SE.err500)
           ]


handleErrorResponse :: ET.ErrorResponse -> SE.ServerError
handleErrorResponse ET.ErrorResponse {..} = SE.ServerError
    { errHTTPCode = code
    , errReasonPhrase = ""
    , errBody         = A.encodePretty response
    , errHeaders      =[]
    }

handleECErrorResponse :: ET.ECErrorResponse -> SE.ServerError
handleECErrorResponse ET.ECErrorResponse {..} = SE.ServerError
    { errHTTPCode = code
    , errReasonPhrase = ""
    , errBody         = response
    , errHeaders      =[]
    }