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
  describe "aesonOmitNothingFields" $ do
    it "Default option. Null age" $ do
      encode personNull `shouldBe` enc_personNull
    it "Default option. Just 33 age" $ do
      encode person `shouldBe` enc_person
    it "With omit option. Null age" $ do
      encode personOmitNull `shouldBe` enc_personOmitNull
    it "With omit option. Just 33 age" $ do
      encode personOmit `shouldBe` enc_personOmit

  describe "unaryRecordOptions" $ do
    it "Default option. Complete json" $ do
      decode enc_planet  `shouldBe` Just lunar
    it "Default option. Incomplete json" $ do
      eitherDecode @Cosmos enc_planetIncomplete `shouldBe` Left decode_error
    it "With unary option. Complete unary json" $ do
      decode enc_unarySpace `shouldBe` Just unarySpace
    it "With unary option. Incomplete unary json" $ do
      decode enc_unarySpaceIncomplete `shouldBe` Just unarySpace

  describe "untaggedOptions" $ do
    it "Default option" $ do
      encode fruit `shouldBe` enc_plant
    it "With untagged option" $ do
      encode berry `shouldBe` enc_plantUntag

  describe "stripLensPrefixOptions" $ do
    it "Default option" $ do
      encode cat `shouldBe` enc_cat
    it "With strip option" $ do
      encode dog `shouldBe` enc_dog



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
  deriving anyclass (FromJSON)

instance ToJSON PersonOmit where
  toJSON     = genericToJSON aesonOmitNothingFields
  toEncoding = genericToEncoding aesonOmitNothingFields

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

-- for default decoding
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

-------------------------------------------------------------------------------
-- untaggedOptions
-------------------------------------------------------------------------------

data Apple = Apple
  { weight :: Int
  , colour :: Maybe Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data Strawberry = Strawberry
  { weight :: Int
  , colour :: Maybe Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- for default encoding
data Plant
  = Fruit Apple
  | Berry Strawberry
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

fruit :: Plant
fruit = Fruit $ Apple 12 (Just "green")

enc_plant :: BSL.ByteString
enc_plant = "{\"tag\":\"Fruit\",\"contents\":{\"weight\":12,\"colour\":\"green\"}}"

data PlantUnTag
  = FruitU Apple
  | BerryU Strawberry
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON)

instance ToJSON PlantUnTag where
  toJSON     = genericToJSON untaggedOptions
  toEncoding = genericToEncoding untaggedOptions

berry :: PlantUnTag
berry = BerryU $ Strawberry 2 (Just "red")

enc_plantUntag :: BSL.ByteString
enc_plantUntag = "{\"weight\":2,\"colour\":\"red\"}"

-------------------------------------------------------------------------------
-- stripLensPrefixOptions
-------------------------------------------------------------------------------

data Cat = Cat
  { cName :: Text
  , cColour :: Maybe Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

cat :: Cat
cat = Cat "Kita" (Just "grey")

enc_cat :: BSL.ByteString
enc_cat = "{\"cName\":\"Kita\",\"cColour\":\"grey\"}"

data Dog = Dog
  { cName :: Text
  , cColour :: Maybe Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON)

instance ToJSON Dog where
  toJSON     = genericToJSON stripLensPrefixOptions
  toEncoding = genericToEncoding stripLensPrefixOptions

dog :: Dog
dog = Dog "Buddy" (Just "white")

enc_dog :: BSL.ByteString
enc_dog = "{\"Name\":\"Buddy\",\"Colour\":\"white\"}"
