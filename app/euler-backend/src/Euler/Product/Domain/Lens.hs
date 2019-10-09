{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell        #-}

module Euler.Product.Domain.Lens where


import           Control.Lens       (makeFieldsNoPrefix)

import Euler.Product.Domain.Transaction (Transaction)

makeFieldsNoPrefix ''Transaction
