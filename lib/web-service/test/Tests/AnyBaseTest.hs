module Tests.AnyBaseTest
    ( anyBaseTests
    , spec
    )
    where

import           EulerHS.Prelude
import           WebService.AnyBase (bin, dec, flickr2Hex, flickr58, fromTo, hex, hex2Flickr, oct)

import qualified Data.Text as T
import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import           Test.Hspec


-- Generators

genBase :: (MonadGen m, GenBase m ~ Identity) => String -> m String
genBase base = do
  c <- Gen.element $ drop 1 base
  str <- Gen.string (Range.constant 0 100) $ Gen.element base
  pure $ c : str

-- | Generate bin
genBin :: (MonadGen m, GenBase m ~ Identity) => m String
genBin = genBase bin

-- | Generate dec
genDec :: (MonadGen m, GenBase m ~ Identity) => m String
genDec = genBase dec

-- | Generate hex
genHex :: (MonadGen m, GenBase m ~ Identity) => m String
genHex = genBase hex

-- | Generate oct
genOct :: (MonadGen m, GenBase m ~ Identity) => m String
genOct = genBase oct

-- | Generate flickr58
genFlickr58 :: (MonadGen m, GenBase m ~ Identity) => m String
genFlickr58 = genBase flickr58


-- Proparty tests

prop_fromTo :: String -> String -> Gen String -> Property
prop_fromTo from to gen = property $ do
    x <- forAll gen
    let encoder = fromTo from to
    let decoder = fromTo to from
    tripping x encoder (decoder =<<)

prop_hex2Flickr :: Property
prop_hex2Flickr = property $ do
    x <- forAll genHex
    tripping (T.pack x) hex2Flickr (flickr2Hex =<<)

prop_flickr2Hex :: Property
prop_flickr2Hex = property $ do
    x <- forAll genFlickr58
    tripping (T.pack x) flickr2Hex (hex2Flickr =<<)

anyBaseTests :: IO Bool
anyBaseTests =
  checkSequential $ Group "AnyBase property tripping tests" [
      ("bin dec", prop_fromTo bin dec genBin)
    , ("bin hex", prop_fromTo bin hex genBin)
    , ("bin oct", prop_fromTo bin oct genBin)
    , ("bin flickr58", prop_fromTo bin flickr58 genBin)
    , ("dec bin", prop_fromTo dec bin genDec)
    , ("dec hex", prop_fromTo dec hex genDec)
    , ("dec oct", prop_fromTo dec oct genDec)
    , ("dec flickr58", prop_fromTo dec flickr58 genDec)
    , ("hex bin", prop_fromTo hex bin genHex)
    , ("hex dec", prop_fromTo hex dec genHex)
    , ("hex oct", prop_fromTo hex oct genHex)
    , ("hex flickr58", prop_fromTo hex flickr58 genHex)
    , ("oct bin", prop_fromTo oct bin genOct)
    , ("oct dec", prop_fromTo oct dec genOct)
    , ("oct hex", prop_fromTo oct hex genOct)
    , ("oct flickr58", prop_fromTo oct flickr58 genOct)
    , ("flickr58 bin", prop_fromTo flickr58 bin genFlickr58)
    , ("flickr58 dec", prop_fromTo flickr58 dec genFlickr58)
    , ("flickr58 hex", prop_fromTo flickr58 hex genFlickr58)
    , ("flickr58 oct", prop_fromTo flickr58 oct genFlickr58)
    , ("hex2Flickr flickr2Hex", prop_hex2Flickr)
    , ("flickr2Hex hex2Flickr", prop_flickr2Hex)
    ]

-- Spect tests

flickr58Sample :: String
flickr58Sample = "4KAWxivvjgHnP5bt3umYRUf"

decSample :: String
decSample = "2342348900015834576417489237564945234234"

hexSample :: String
hexSample = "6e22ffd19c2489edabc47320ad13a793a"

octSample :: String
octSample = "33421377643160444236665361071440532116474472"

binSample :: String
binSample = "11011100010001011111111110100011001110000100100100010011110110110101011110001000111001100100000101011010001001110100111100100111010"

spec :: Spec
spec = describe "AnyBase spec tests" $ do
  it "fromTo bin dec" $ do
    let result = fromTo bin dec binSample
    result `shouldBe` Just decSample
  it "fromTo bin hex" $ do
    let result = fromTo bin hex binSample
    result `shouldBe` Just hexSample
  it "fromTo bin oct" $ do
    let result = fromTo bin oct binSample
    result `shouldBe` Just octSample
  it "fromTo bin flickr58" $ do
    let result = fromTo bin flickr58 binSample
    result `shouldBe` Just flickr58Sample

  it "fromTo dec bin" $ do
    let result = fromTo dec bin decSample
    result `shouldBe` Just binSample
  it "fromTo dec hex" $ do
    let result = fromTo dec hex decSample
    result `shouldBe` Just hexSample
  it "fromTo dec oct" $ do
    let result = fromTo dec oct decSample
    result `shouldBe` Just octSample
  it "fromTo dec flickr58" $ do
    let result = fromTo dec flickr58 decSample
    result `shouldBe` Just flickr58Sample

  it "fromTo hex bin" $ do
    let result = fromTo hex bin hexSample
    result `shouldBe` Just binSample
  it "fromTo hex dec" $ do
    let result = fromTo hex dec hexSample
    result `shouldBe` Just decSample
  it "fromTo hex oct" $ do
    let result = fromTo hex oct hexSample
    result `shouldBe` Just octSample
  it "fromTo hex flickr58" $ do
    let result = fromTo hex flickr58 hexSample
    result `shouldBe` Just flickr58Sample

  it "fromTo oct bin" $ do
    let result = fromTo oct bin octSample
    result `shouldBe` Just binSample
  it "fromTo oct dec" $ do
    let result = fromTo oct dec octSample
    result `shouldBe` Just decSample
  it "fromTo oct hex" $ do
    let result = fromTo oct hex octSample
    result `shouldBe` Just hexSample
  it "fromTo oct flickr58" $ do
    let result = fromTo oct flickr58 octSample
    result `shouldBe` Just flickr58Sample

  it "fromTo flickr58 bin" $ do
    let result = fromTo flickr58 bin flickr58Sample
    result `shouldBe` Just binSample
  it "fromTo flickr58 dec" $ do
    let result = fromTo flickr58 dec flickr58Sample
    result `shouldBe` Just decSample
  it "fromTo flickr58 hex" $ do
    let result = fromTo flickr58 hex flickr58Sample
    result `shouldBe` Just hexSample
  it "fromTo flickr58 oct" $ do
    let result = fromTo flickr58 oct flickr58Sample
    result `shouldBe` Just octSample
