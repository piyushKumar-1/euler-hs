{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE RecordWildCards #-}

module Euler.Product.Domain.Templates.Address where

import           EulerHS.Prelude

data AddressTemplate = AddressTemplate
  { line1             :: Maybe Text
  , line2             :: Maybe Text
  , line3             :: Maybe Text
  , city              :: Maybe Text
  , state             :: Maybe Text
  , country           :: Maybe Text
  , postalCode        :: Maybe Text
  , phone             :: Maybe Text
  , countryCodeIso    :: Maybe Text -- Default value: IND
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)


data AddressHolderTemplate = AddressHolderTemplate
  { firstName  :: Maybe Text
  , lastName   :: Maybe Text
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)
