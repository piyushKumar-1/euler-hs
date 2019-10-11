{-# LANGUAGE DeriveAnyClass #-}

module Euler.Product.Domain.OrderMetadataV2 where

import EulerHS.Prelude
import Data.Time
import Euler.Common.DefaultDate

data OrderMetadataV2 = OrderMetadataV2
  { id               :: Maybe Int
  , browser          :: Maybe Text
  , browserVersion   :: Maybe Text
  , dateCreated      :: LocalTime
  , device           :: Maybe Text
  , lastUpdated      :: LocalTime
  , metadata         :: Maybe Text
  , mobile           :: Maybe Bool
  , operatingSystem  :: Maybe Text
  , orderReferenceId :: Int
  , ipAddress        :: Maybe Text
  , referer          :: Maybe Text
  , userAgent        :: Maybe Text
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

defaultOrderMetadataV2 = OrderMetadataV2
  { id               = Nothing -- :: Maybe Int
  , browser          = Nothing -- :: Maybe Text
  , browserVersion   = Nothing -- :: Maybe Text
  , dateCreated      = defaultDate -- :: LocalTime
  , device           = Nothing -- :: Maybe Text
  , lastUpdated      = defaultDate -- :: LocalTime
  , metadata         = Nothing -- :: Maybe Text
  , mobile           = Nothing -- :: Maybe Bool
  , operatingSystem  = Nothing -- :: Maybe Text
  , orderReferenceId = 1 -- :: Int
  , ipAddress        = Nothing -- :: Maybe Text
  , referer          = Nothing -- :: Maybe Text
  , userAgent        = Nothing -- :: Maybe Text
  }