{-# LANGUAGE DeriveAnyClass #-}

module EulerHS.Core.Types.Exceptions
  ( -- * Exceptions
    HttpManagerNotFound(..)
  ) where

import           EulerHS.Prelude


data HttpManagerNotFound = HttpManagerNotFound String
 deriving (Show, Eq, Exception)


