module EulerHS.Extra.Aeson where

import EulerHS.Prelude


stripLensPrefixOptions :: Options
stripLensPrefixOptions = defaultOptions { fieldLabelModifier = drop 1 }
