{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell        #-}

module Euler.Product.Domain.Lens where

import EulerHS.Prelude
import           Control.Lens       (makeFieldsNoPrefix)

import Euler.Product.Domain.Transaction (Transaction)

makeFieldsNoPrefix ''Transaction
