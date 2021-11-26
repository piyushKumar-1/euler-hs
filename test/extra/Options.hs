{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}

module Options where

import           EulerHS.Extra.Aeson
import           EulerHS.Prelude

import           Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import           Test.Hspec


spec :: Spec
spec = do
  describe "With aesonOmitNothingFields" $ do
    it "Null age" $ do
      encode personNull `shouldBe` expected_personNull
    it "Just 33 age" $ do
      encode person `shouldBe` expected_person

  describe "Default json instances" $ do
    it "Null age" $ do
      encode personUsualNull `shouldBe` expected_personUsualNull
    it "Just 33 age" $ do
      encode personUsual `shouldBe` expected_personUsual

data Person = Person
  { name :: Text
  , age  :: Maybe Int
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON Person where
  toJSON     = genericToJSON aesonOmitNothingFields
  toEncoding = genericToEncoding aesonOmitNothingFields

instance FromJSON Person where
  parseJSON = genericParseJSON aesonOmitNothingFields

personNull :: Person
personNull = Person "Omar" Nothing

expected_personNull :: BSL.ByteString
expected_personNull = "{\"name\":\"Omar\"}"

person :: Person
person = Person "Omar" (Just 33)

expected_person :: BSL.ByteString
expected_person = "{\"name\":\"Omar\",\"age\":33}"

data PersonUsual = PersonUsual
  { name :: Text
  , age  :: Maybe Int
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

personUsualNull :: PersonUsual
personUsualNull = PersonUsual "Omar" Nothing

expected_personUsualNull :: BSL.ByteString
expected_personUsualNull = "{\"age\":null,\"name\":\"Omar\"}"

personUsual :: PersonUsual
personUsual = PersonUsual "Omar" (Just 33)

expected_personUsual :: BSL.ByteString
expected_personUsual = "{\"age\":33,\"name\":\"Omar\"}"
