{-# LANGUAGE DeriveAnyClass #-}
module EulerHS.TestData.Types where

import           EulerHS.Prelude
import           EulerHS.Types

import           Test.QuickCheck.Arbitrary



data UrlKey



instance OptionEntity UrlKey String

data TestStringKey

instance OptionEntity TestStringKey String

data TestStringKey2

instance OptionEntity TestStringKey2 String

data User = User { firstName :: String, lastName :: String , userGUID :: String}
  deriving (Generic, Show, Eq, ToJSON, FromJSON )

instance Arbitrary User where
  arbitrary = User <$> arbitrary <*> arbitrary <*> arbitrary

data Book = Book { author :: String, name :: String }
  deriving (Generic, Show, Eq, ToJSON, FromJSON )

instance Arbitrary Book where
  arbitrary = Book <$> arbitrary <*> arbitrary
