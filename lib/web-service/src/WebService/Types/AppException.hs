{-# LANGUAGE DeriveAnyClass #-}

module WebService.Types.AppException where

import EulerHS.Prelude


data AppException
  = SqlDBConnectionFailedException Text
  | KVDBConnectionFailedException Text
  deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON, Exception)
