module EulerHS.Extra.Aeson where

import           Prelude

import           Data.Aeson (Options, defaultOptions, fieldLabelModifier)

stripLensPrefixOptions :: Options
stripLensPrefixOptions = defaultOptions { fieldLabelModifier = drop 1 }

stripAllLensPrefixOptions :: Options
stripAllLensPrefixOptions = defaultOptions { fieldLabelModifier = dropPrefix}
  where
    dropPrefix :: String -> String
    dropPrefix field = if length field > 0
                         then dropWhile (== head field) field
                         else field
