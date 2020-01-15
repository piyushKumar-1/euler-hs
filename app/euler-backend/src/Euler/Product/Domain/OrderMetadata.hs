{-# LANGUAGE DeriveAnyClass #-}

module Euler.Product.Domain.GatewayMetadata where

import EulerHS.Prelude

import qualified Data.Map as Map
import Euler.Common.Types.Gateway (Gateway)


data GatewayMetaEntry = GatewayMetaEntry
  { gateway :: Gateway
  , name :: Text
  , value :: Text
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

type GatewayMetadata = Map.Map Gateway [GatewayMetaEntry]
