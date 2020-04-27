{-# LANGUAGE DeriveAnyClass #-}

module Euler.Product.Domain.SecondFactorResponse where

import EulerHS.Prelude

import Data.Time (LocalTime)

newtype SecondFactorResponsePId = SecondFactorResponsePId
  { sfrPId :: Int
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

data SecondFactorResponse = SecondFactorResponse
  { id :: SecondFactorResponsePId
  , version :: Int
  , cavv :: Maybe Text
  , currency ::  Maybe Text
  , eci :: Text
  , mpiErrorCode :: Maybe Text
  , purchaseAmount :: Maybe Double
  , responseId :: Maybe Text
  , shoppingContext :: Text
  , status :: Text
  , xid :: Text
  , dateCreated :: LocalTime
  , secondFactorId :: Maybe Int
  , gatewayAuthResData :: Maybe Text
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)



