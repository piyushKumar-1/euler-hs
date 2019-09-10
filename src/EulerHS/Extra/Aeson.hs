module EulerHS.Extra.Aeson where

import Prelude

import Data.Aeson (Options, defaultOptions, fieldLabelModifier)

stripLensPrefixOptions :: Options
stripLensPrefixOptions = defaultOptions { fieldLabelModifier = drop 1 }
