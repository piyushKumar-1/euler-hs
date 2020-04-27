{-# LANGUAGE DeriveAnyClass #-}

module Euler.Product.Domain.MerchantSecondFactorResponse where

import           EulerHS.Prelude


data MerchantSecondFactorResponse = MerchantSecondFactorResponse
  {  cavv         :: Maybe Text
  ,  eci          :: Text
  ,  xid          :: Text
  ,  paresStatus :: Text
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)
