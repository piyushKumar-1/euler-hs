{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

-----------------------------------------------------------------------------
-- | Module : WebService.ContentType.JsonError
--
-- Provides a convenient way to throw JSON responses on errors.
-----------------------------------------------------------------------------
module WebService.ContentType.JsonError where


import Servant
import Data.Aeson           (ToJSON, encode)
import Network.HTTP.Types   (hContentType)
import Control.Monad.Except (MonadError)

throwJsonError :: (MonadError ServerError m, ToJSON a) => ServerError -> a -> m b
throwJsonError err json = throwError err
    { errBody = encode json
    , errHeaders = [ jsonHeader ]
    }
  where
    jsonHeader = ( hContentType
                  , "application/json;charset=utf-8" )
