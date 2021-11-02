{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module EulerHS.Extra.NonEmptyText
  ( NonEmptyText,
    fromText,
    nonEmpty,
    new,
    newtypeNetPrism,
    toText,
  )
where

import           EulerHS.Prelude hiding (preview, nonEmpty, toText)
import           Data.Aeson (withText)
import           Data.Aeson.Types (prependFailure)
import           Data.Coerce (coerce)
--import           Data.Functor (($>))
import qualified Data.Store as Binary
import qualified Data.Text as Text
import           Optics.Core (preview, review)
import           Optics.Prism (Prism', prism')
import           Test.QuickCheck (Arbitrary (arbitrary, shrink), listOf)

-- | An opaque wrapper for 'Text' which cannot be empty.
newtype NonEmptyText = NET {toText :: Text}
  deriving newtype (Eq, Ord, Show, Semigroup, ToJSON, Binary.Store)

instance FromJSON NonEmptyText where
  parseJSON = prependFailure "Parsing of the NonEmptyText value failed: " . withText "NonEmptyText" go
    where
      go t = case Text.uncons t of
        Nothing -> fail "Tried to convert an empty Text"
        Just _  -> pure . NET $ t

instance Arbitrary NonEmptyText where
  arbitrary = do
    h <- arbitrary @Char
    t <- listOf @Char arbitrary
    pure . NET . Text.pack $ h : t
  shrink =
    fmap NET . shrinkText . toText
    where
      shrinkText text = Text.pack <$> filter (not . null) (shrink (Text.unpack text))

-- | Create a `NonEmptyText` from `Text`
fromText :: Text -> Maybe NonEmptyText
fromText "" = Nothing
fromText t  = Just (NET t)

-- | Move between 'Text' and 'NonEmptyText' conveniently.
nonEmpty :: Prism' Text NonEmptyText
nonEmpty = prism' toText (\t -> Text.uncons t $> NET t)

-- | Create a 'NonEmptyText' from a \'head\' and \'tail\'.
new :: Char -> Text -> NonEmptyText
new c = NET . Text.cons c

-- | Create prism for newtypes over NonEmptyText
newtypeNetPrism :: forall a . (Coercible a NonEmptyText) => Prism' Text a
newtypeNetPrism = prism' out into
  where
    out = review nonEmpty . coerce @a @NonEmptyText
    into t = coerce @NonEmptyText @a <$> preview nonEmpty t
