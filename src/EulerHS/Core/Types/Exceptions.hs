{-# LANGUAGE DerivingVia #-}

module EulerHS.Core.Types.Exceptions
  ( -- * Exceptions
    HttpManagerNotFound(..)
  , AwaitingError (..)
  ) where

import           EulerHS.Prelude

newtype HttpManagerNotFound = HttpManagerNotFound Text
 deriving stock (Show)
 deriving (Eq) via Text

instance Exception HttpManagerNotFound

data AwaitingError = AwaitingTimeout | ForkedFlowError Text
  deriving (Show, Eq, Ord, Generic)
