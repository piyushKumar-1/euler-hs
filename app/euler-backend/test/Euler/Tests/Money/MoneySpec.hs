module Euler.Tests.Money.MoneySpec where

import           EulerHS.Prelude
import           Test.Hspec

import qualified Euler.Product.Domain.Money as M


m1 :: Double
m1 = 0.01

m2 :: Double
m2 = 0.10

m3 :: Double
m3 = 1.00

m4 :: Double
m4 = 10.00

bmP :: Double
bmP = 1.009

bmN :: Double
bmN = (- 1.009)


spec :: Spec
spec =
  describe "Money tests" $ do


    it "Double convert to/from Money" $  do
      let newRM1  = M.fromMoney $ M.mkMoney m1
      let newRM2  = M.fromMoney $ M.mkMoney m2
      let newRM3  = M.fromMoney $ M.mkMoney m3
      let newRM4  = M.fromMoney $ M.mkMoney m4
      newRM1 `shouldBe` m1
      newRM2 `shouldBe` m2
      newRM3 `shouldBe` m3
      newRM4 `shouldBe` m4

    it "Money add" $ do
      let mR01 = foldl1 mappend $ replicate 10 (M.mkMoney m1)
      let mR1 = foldl1 mappend $ replicate 100 (M.mkMoney m1)
      let mR10 = foldl1 mappend $ replicate 1000 (M.mkMoney m1)
      mR01 `shouldBe` M.mkMoney m2
      M.fromMoney mR01 `shouldBe` m2
      mR1 `shouldBe` M.mkMoney m3
      M.fromMoney mR1 `shouldBe` m3
      mR10 `shouldBe` M.mkMoney m4
      M.fromMoney mR10 `shouldBe` m4

    it "Mixed operations" $ do
      let m = (M.mkMoney m1)
      let v1 = M.mPow 500 m `M.mSubtract` (M.mPow 300 m `M.mSubtract` M.mPow 200 m)
      let v2 = M.mNegate $ M.mPow 500 m `M.mSubtract` (M.mPow 1100 m `M.mSubtract` M.mPow 200 m)
      let v3 = M.mNegate m
      M.fromMoney v1 `shouldBe` 4
      M.fromMoney v2 `shouldBe` 4
      (M.mAbs m == M.mAbs v3) && (m == M.mAbs v3) `shouldBe` True

    it "Truncate test" $ do
      let fbmP = M.mkMoney bmP
      let fbmN = M.mkMoney bmN
      fbmP `shouldBe` M.mkMoney m3
      M.fromMoney fbmP `shouldBe` m3
      fbmN `shouldBe` (M.mNegate . M.mkMoney) m3
      M.fromMoney fbmN `shouldBe`  negate m3