{-# LANGUAGE DeriveAnyClass #-}

module Euler.Product.Domain.PaymentGatewayResponse where

import           EulerHS.Prelude

import           Data.Time (LocalTime)



newtype PaymentGatewayResponsePId = PaymentGatewayResponsePId
  { paymentGatewayResponsePId :: Int
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)


data PaymentGatewayResponse = PaymentGatewayResponse
  { id               :: PaymentGatewayResponsePId
  , version          :: Int
  , bankCode         :: Maybe Text
  , dateCreated      :: Maybe LocalTime
  , responseXml      :: Maybe Text
  , txnId            :: Maybe Text
  , iciciRespCode    :: Maybe Text
  , iciciRespMessage :: Maybe Text
  , axisRespCode     :: Maybe Text
  , axisRespMessage  :: Maybe Text
  , hdfcRespCode     :: Maybe Text
  , hdfcRespMessage  :: Maybe Text
  , respCode         :: Maybe Text
  , respMessage      :: Maybe Text
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)
