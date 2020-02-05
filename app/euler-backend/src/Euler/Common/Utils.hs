module Euler.Common.Utils where


import           EulerHS.Prelude

import qualified Data.Text as Text

isBlank :: Text -> Bool
isBlank = Text.null . Text.strip

-- In groovy empty string is `falsy` value
isBlankMaybe :: Maybe Text -> Bool
isBlankMaybe Nothing = False
isBlankMaybe (Just val) = isBlank val

isTrueMaybe :: Maybe Bool -> Bool
isTrueMaybe (Just True) = True
isTrueMaybe _ = False

-- unNullEmptyStringAsNothing
blankToNothing :: Maybe Text -> Maybe Text
blankToNothing Nothing = Nothing
blankToNothing (Just val) = case Text.strip val of
  "" -> Nothing
  _ -> Just val
