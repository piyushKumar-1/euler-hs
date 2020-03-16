{-# LANGUAGE DeriveAnyClass #-}

module Euler.Product.Domain.SecondFactor where

import EulerHS.Prelude

import Data.Time (LocalTime)

newtype SecondFactorPId = SecondFactorPId
  { sfPId :: Int
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

data SecondFactor = SecondFactor
  { id :: SecondFactorPId
  , version :: Int
  , otp :: Maybe Text
  , status :: Text
  , txnId :: Text
  , sfType :: Text -- EPS: type is reserved word
  , url :: Maybe Text
  , secondFactorResponse :: Maybe Text
  , dateCreated :: LocalTime
  , epgTxnId :: Maybe Text
  , lastUpdated :: LocalTime
  , txnDetailId :: Maybe Int
  , gatewayAuthReqParams :: Maybe Text
  , authenticationAccountId :: Maybe Text
  , canAcceptResponse :: Maybe Bool
  , challengesAttempted :: Maybe Int
  , responseAttempted :: Maybe Int
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)



