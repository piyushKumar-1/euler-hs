{-# LANGUAGE DeriveAnyClass #-}

module Euler.Product.Domain.MerchantPaymentGatewayResponse where


import EulerHS.Prelude


data MerchantPaymentGatewayResponse = MerchantPaymentGatewayResponse
  { respCode           :: Maybe Text
  , rrn                :: Maybe Text
  , created            :: Maybe Text
  , epgTxnId           :: Maybe Text
  , respMessage        :: Maybe Text
  , authIdCode         :: Maybe Text
  , txnId              :: Maybe Text
  , offer              :: Maybe Text
  , offerType          :: Maybe Text
  , offerAvailed       :: Maybe Text
  , discountAmount     :: Maybe Text
  , offerFailureReason :: Maybe Text
  , gatewayResponse    :: Maybe Text
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)


defaultMerchantPaymentGatewayResponse :: MerchantPaymentGatewayResponse
defaultMerchantPaymentGatewayResponse = MerchantPaymentGatewayResponse
  { respCode = Nothing
  , rrn = Nothing
  , created = Nothing
  , epgTxnId = Nothing
  , respMessage = Nothing
  , authIdCode = Nothing
  , txnId = Nothing
  , offer = Nothing
  , offerType = Nothing
  , offerAvailed = Nothing
  , discountAmount = Nothing
  , offerFailureReason = Nothing
  , gatewayResponse = Nothing
  }
