

module EulerHS.Extra.Text where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import           EulerHS.Prelude hiding (unpack)
import           Data.Char (toLower, toUpper)
import qualified Data.Text as T
import qualified EulerHS.Extra.Secret as Secret
-- ByteString

toLazy :: BS.ByteString -> BSL.ByteString
toLazy s = BSL.fromChunks [s]

-- Text

-- former readMay
-- | readMaybe for Text
readMaybeT :: Read a => Text -> Maybe a
readMaybeT = readMaybe . T.unpack

-- | Check whether text is empty after striping trailing spaces
-- By empty it means text without trailing spaces.
isBlank :: Text -> Bool
isBlank = T.null . T.strip

-- | Check whether text is empty after striping trailing spaces
notBlank :: Text -> Maybe Text
notBlank t
  | isBlank t = Nothing
  | otherwise = Just t

-- | Check whether text is not empty after striping trailing spaces
isNotBlank ::Text -> Bool
isNotBlank =  not . T.null . T.strip

-- | Check whether text in Maybe is empty
isBlankMaybe :: Maybe Text -> Bool
isBlankMaybe Nothing    = False
isBlankMaybe (Just val) = isBlank val

-- In groovy empty string is `false` value
-- euler-ps's isTrueString == isBlankMaybe
-- | Check whether text in Maybe is not empty
isFullMaybe :: Maybe Text -> Bool
isFullMaybe Nothing    = False
isFullMaybe (Just val) = isNotBlank val

-- | Check whether text in Maybe Secret is not empty
isFullMaybeSecret :: Maybe (Secret.Secret Text) -> Bool
isFullMaybeSecret Nothing    = False
isFullMaybeSecret (Just val) = (isNotBlank <$> val) == Secret.makeSecret True

-- | Simplifyer. Check Bool inside Maybe is False or not.
-- Simplifier means reducing complex type to simple one.
-- Just False -> True
-- Just True -> False
-- Nothing -> False
isFalseMaybe :: Maybe Bool -> Bool
isFalseMaybe (Just False) = True
isFalseMaybe _            = False

-- | Simplifyer. Check Bool inside Maybe is True or not.
-- Just True -> True
-- Just False -> False
-- Nothing -> False
isTrueMaybe :: Maybe Bool -> Bool
isTrueMaybe (Just True) = True
isTrueMaybe _           = False

-- | Simplifyer. Check Bool inside Maybe is False or not.
-- Reverse to isTrueMaybe
-- Just True -> False
-- Just False -> True
-- Nothing -> True
isNotTrueMaybe :: Maybe Bool -> Bool
isNotTrueMaybe = not . isTrueMaybe

-- | Make a word Title case
toTitle :: String -> String
toTitle ""     = ""
toTitle (x:xs) = toUpper x : map toLower xs

-- Helper which will interpret empty text "" as Nothing
emptyTextAsNothing :: Maybe Text -> Maybe Text
emptyTextAsNothing Nothing = Nothing
emptyTextAsNothing (Just "") = Nothing
emptyTextAsNothing x = x
