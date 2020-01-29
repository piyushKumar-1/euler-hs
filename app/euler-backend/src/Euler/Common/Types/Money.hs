{-# LANGUAGE DeriveAnyClass #-}

module Euler.Common.Types.Money
  ( Money
  , mkMoney
  , fromMoney
  , mAdd
  , mSubtract
  , mPow
  , mNegate
  , mAbs
  ) where

import EulerHS.Prelude


import           GHC.Generics                         (Generic)


newtype Money = Money {unMoney :: Integer}
  deriving (Ord, Eq, Show, Generic, ToJSON, FromJSON)

instance Semigroup Money where
  (Money m1) <> (Money m2) = Money (m1 + m2)
  stimes n (Money m) = Money (fromIntegral n * m)

instance Monoid Money where
  mempty = Money 0

mkMoney :: Double -> Money
mkMoney = Money . truncate . (*100)

fromMoney :: Money -> Double
fromMoney (Money m) = (fromInteger m) / 100

mAdd :: Money -> Money -> Money
mAdd = (<>)

mSubtract :: Money -> Money -> Money
mSubtract (Money m1) (Money m2) = Money (m1 - m2)

mPow :: Integral n => n -> Money -> Money
mPow = stimes

mNegate :: Money -> Money
mNegate = Money . negate . unMoney

mAbs :: Money -> Money
mAbs = Money . abs . unMoney
