module Tests.UuidTest
  ( uuidTests
  , spec
  )
  where

import           EulerHS.Prelude
import           WebService.AnyBase (hex)
import           WebService.UUID (enlargeUUID, shortenUUID)

import qualified Data.Text as T
import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import           Test.Hspec


-- Property tests

-- | Generate flickr58
genUuid :: (MonadGen m, GenBase m ~ Identity) => m String
genUuid = do
  c <- Gen.element $ drop 1 hex
  let genUuidPart l = Gen.string (Range.singleton l) $ Gen.element hex
  hex8 <- (c :) <$> genUuidPart 7
  hex4 <- genUuidPart 4
  hex12 <- genUuidPart 12

  pure $ intercalate "-" [hex8, hex4, hex4, hex4, hex12]

prop_shortenEnlargeUuid :: Property
prop_shortenEnlargeUuid = property $ do
  x <- forAll genUuid
  tripping (T.pack x) shortenUUID (enlargeUUID =<<)

uuidTests :: IO Bool
uuidTests =
  checkSequential $ Group "UUID property tripping tests" [
      ("shortenUuid enlargeUuid", prop_shortenEnlargeUuid)
    ]

-- Spec tests

large :: Text
large = "43128207-6cd9-4e3a-b6fc-edac33c1fab0"

short :: Text
short = "9hnUomW3tL85TvsrmzFkoy"

spec :: Spec
spec = describe "UUID spec tests" $ do
  it "shorten UUID" $ do
    let result = shortenUUID large
    result `shouldBe` Just short
  it "enlarge UUID" $ do
    let result = enlargeUUID short
    result `shouldBe` Just large
