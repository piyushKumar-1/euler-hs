{-# LANGUAGE DeriveAnyClass #-}

module Euler.Product.Domain.ResellerAccount
  ( ResellerAccount(..)
  )
  where

import EulerHS.Prelude

data ResellerAccount = ResellerAccount
  { resellerId          :: Int
  , userId              :: Int
  , resellerName        :: Text
  , resellerApiEndpoint :: Maybe Text
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)
