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
  describe "Without aesonOmitNothingFields" $ do
    it "Null age" $ do
      encode personNull `shouldBe` enc_personNull
    it "Just 33 age" $ do
      encode person `shouldBe` enc_person

  describe "With aesonOmitNothingFields" $ do
    it "Null age" $ do
      encode personOmitNull `shouldBe` enc_personOmitNull
    it "Just 33 age" $ do
      encode personOmit `shouldBe` enc_personOmit

  describe "Without unaryRecordOptions" $ do
    it "Complete json" $ do
      decode enc_planet  `shouldBe` Just lunar
    it "Incomplete json" $ do
      eitherDecode @Cosmos enc_planetIncomplete `shouldBe` Left decode_error

  describe "With unaryRecordOptions" $ do
    it "Complete unary json" $ do
      decode enc_unarySpace `shouldBe` Just unarySpace
    it "Incomplete unary json" $ do
      decode enc_unarySpaceIncomplete `shouldBe` Just unarySpace



-------------------------------------------------------------------------------
-- aesonOmitNothingFields option
-------------------------------------------------------------------------------

-- Default option
data Person = Person
  { name :: Text
  , age  :: Maybe Int
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

personNull :: Person
personNull = Person "Omar" Nothing

enc_personNull :: BSL.ByteString
enc_personNull = "{\"age\":null,\"name\":\"Omar\"}"

person :: Person
person = Person "Omar" (Just 33)

enc_person :: BSL.ByteString
enc_person = "{\"age\":33,\"name\":\"Omar\"}"

-- omit field option
data PersonOmit = PersonOmit
  { name :: Text
  , age  :: Maybe Int
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON PersonOmit where
  toJSON     = genericToJSON aesonOmitNothingFields
  toEncoding = genericToEncoding aesonOmitNothingFields

instance FromJSON PersonOmit where
  parseJSON = genericParseJSON aesonOmitNothingFields

personOmitNull :: PersonOmit
personOmitNull = PersonOmit "Omar" Nothing

enc_personOmitNull :: BSL.ByteString
enc_personOmitNull = "{\"name\":\"Omar\"}"

personOmit :: PersonOmit
personOmit = PersonOmit "Omar" (Just 33)

enc_personOmit :: BSL.ByteString
enc_personOmit = "{\"name\":\"Omar\",\"age\":33}"


-------------------------------------------------------------------------------
-- unaryRecordOptions option
-------------------------------------------------------------------------------

-- default decoding
data Space = Space
  { name :: Text
  , distance :: Maybe Int
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data Planet = Planet
  { name :: Text
  , weight :: Maybe Int
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data Cosmos
  = Solar Space
  | Lunar Planet
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

lunar :: Cosmos
lunar = Lunar $ Planet "Moon" (Just 5923)

enc_planet :: BSL.ByteString
enc_planet = "{\"tag\":\"Lunar\",\"contents\":{\"weight\":5923,\"name\":\"Moon\"}}"

enc_planetIncomplete :: BSL.ByteString
enc_planetIncomplete = "{\"weight\":5923,\"name\":\"Moon\"}"

decode_error :: String
decode_error = "Error in $: parsing Options.Cosmos failed, expected Object with key \"tag\" containing one of [\"Solar\",\"Lunar\"], key \"tag\" not found"

-- decode with unaryRecordOptions option
data CosmosUnary
  = SolarU Space
  | LunarU Planet
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)

instance FromJSON CosmosUnary where
  parseJSON val
    =   (SolarU <$> parseJSON val)
    <|> (LunarU <$> parseJSON val)
    <|> genericParseJSON unaryRecordOptions val

unarySpace :: CosmosUnary
unarySpace = SolarU $ Space "Sirius" (Just 8910)

enc_unarySpace :: BSL.ByteString
enc_unarySpace = "{\"tag\":\"SolarU\",\"contents\":{\"distance\":8910,\"name\":\"Sirius\"}}"

enc_unarySpaceIncomplete :: BSL.ByteString
enc_unarySpaceIncomplete = "{\"distance\":8910,\"name\":\"Sirius\"}"

