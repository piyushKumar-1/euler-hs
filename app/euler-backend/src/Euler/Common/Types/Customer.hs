{-# LANGUAGE DeriveAnyClass #-}

module Euler.Common.Types.Customer where

import EulerHS.Prelude
import Data.Time

import Euler.Common.Types.DefaultDate

type CustomerId = Text

data Customer = Customer
  { id                :: Maybe CustomerId
  , version           :: Int
  , dateCreated       :: LocalTime
  , emailAddress      :: Maybe Text
  , firstName         :: Maybe Text
  , lastName          :: Maybe Text
  , lastUpdated       :: LocalTime
  , merchantAccountId :: Int
  , mobileCountryCode :: Text
  , mobileNumber      :: Text
  , objectReferenceId :: Maybe Text
  }

defaultCustomer :: Customer
defaultCustomer = Customer
  { id                 = Nothing -- :: Maybe Text
  , version            = 1 -- :: Int
  , dateCreated        = defaultDate -- :: LocalTime
  , emailAddress       = Nothing -- :: Maybe Text
  , firstName          = Nothing -- :: Maybe Text
  , lastName           = Nothing -- :: Maybe Text
  , lastUpdated        = defaultDate -- :: LocalTime
  , merchantAccountId  = 1 -- :: Int
  , mobileCountryCode  = "+91" -- :: Text
  , mobileNumber       = "9000000000" -- :: Text
  , objectReferenceId  = Nothing -- :: Maybe Text
  }
