{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}

module CreditPlatform.Domain.Types where

import EulerHS.Prelude

-- Demo types

newtype GSTIN = GSTIN Text
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

newtype AuthToken = AuthToken Text
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

data SomeData = SomeData
  { _field1 :: Text
  , _field2 :: Int
  }
  deriving (Show, Eq, Ord, Generic)


instance FromJSON SomeData where
    parseJSON = genericParseJSON stripLensPrefixOptions

instance ToJSON SomeData where
    toJSON = genericToJSON stripLensPrefixOptions
