-- This warning is disabled while this bug remains in the wild:
-- https://github.com/ndmitchell/record-dot-preprocessor/issues/30
{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DerivingVia         #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}

module EulerHS.Extra.Parsing
(
  ParsingErrorType (..),
  ParsingError (..),
  Parsed (Failed, Result),
  handleParsed, fromParsed,
  Step,
  parse, parseField,
  project, liftEither, liftPure, nonNegative, nonEmptyText,
  mandated, integral, toUTC, defaulting,
  around, aroundSecret, reconcile, secret
) where

import           Control.Applicative (liftA2)
import           Control.Arrow (Kleisli (Kleisli), runKleisli)
import           Control.Category (Category ((.)))
import           Data.Either (fromRight)
import           Data.Int (Int16, Int32, Int64, Int8)
import           Data.Kind (Type)
import           Data.Maybe (fromMaybe)
import           Data.Proxy (Proxy (Proxy))
import           Data.Sequence (Seq (Empty, (:<|)))
import qualified Data.Sequence as Seq
import           Data.Sequence.NonEmpty (NESeq ((:<||)), singleton)
import           Data.Text (Text, pack, unpack)
import           Data.Time (LocalTime, UTCTime, localTimeToUTC, utc)
import qualified Data.Vector as V
import           Data.Vector.NonEmpty (NonEmptyVector, uncons, unfoldr1)
import           EulerHS.Extra.Secret (Secret, makeSecret, unsafeExtractSecret)
import           EulerHS.Extra.NonEmptyText (NonEmptyText, nonEmpty)
import           GHC.Records (HasField, getField)
import           GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import           Optics.Core (preview)
import           Prelude hiding ((.))
import           Text.Read (readMaybe)
import           Type.Reflection (Typeable, tyConName, typeRep, typeRepTyCon)
import           Validation (Validation (Failure, Success), validationToEither)


-- Describes what kind of error we ran into.
data ParsingErrorType =
  MandatoryValueMissing {
    typeName :: {-# UNPACK #-} !Text
    } |
  UnexpectedNegative {
    typeName :: {-# UNPACK #-} !Text
    } |
  UnexpectedEmptyText |
  NotAnIntegral {
    typeName :: {-# UNPACK #-} !Text,
    message  :: {-# UNPACK #-} !Text
    } |
  UnexpectedTextValue {
    intendedType :: {-# UNPACK #-} !Text,
    value        :: {-# UNPACK #-} !Text
    } |
  Other {
    message :: {-# UNPACK #-} !Text
    }
  deriving stock (Eq, Show)

-- Describes the error in full. In particular, when parsing a record field, this
-- tracks the field and record we tried parsing from.
data ParsingError =
  RecordParsingError {
    recordType :: {-# UNPACK #-} !Text,
    fieldName  :: {-# UNPACK #-} !Text,
    errorType  :: !ParsingErrorType
    } |
  OtherParsingError !ParsingErrorType
  deriving stock (Eq, Show)

-- Represents either (possibly many) errors, or a successful result.
--
-- 'fmap' applies a pure function to the result, if any.
--
-- 'pure' produces the given result. '<*>' combines two erroring outcomes (by
-- concatenation), two successes by function application, and a mixture of
-- failure and success by forwarding the failure.
--
-- 'foldMap' replaces _all_ errors with 'mempty', and applies its function to a
-- success. The 'Foldable' instance is provided only as a prerequisite to
-- 'Traversable'; due to this 'blind' treatment of errors (any error, in any
-- number, is treated the same), prefer 'Traversable' functionality if at all
-- possible.
newtype Parsed (a :: Type) = Parsed (Validation (NESeq ParsingError) a)
  deriving (Functor, Applicative, Foldable)
    via (Validation (NESeq ParsingError))

-- 'traverse' either forwards all errors as-are, not executing the effect at
-- all, or executes the effect (and its transformation) on the result.
instance Traversable Parsed where
  {-# INLINEABLE traverse #-}
  traverse f (Parsed comp) = Parsed <$> traverse f comp

-- We can use these patterns as if 'Parsed' had the following definition:
--
-- data Parsed (a :: Type) =
--    Failed (NonEmptyVector ParsingError) |
--    Result a
--
-- We use this to allow 'Parsed' to be a newtype while still having a pleasant
-- pattern matching syntax, as well as to provide a stable API against changes
-- of internals.
pattern Failed :: NonEmptyVector ParsingError -> Parsed a
pattern Failed errs <- Parsed (Failure (intoNEVector -> errs))
  where
    Failed errs = Parsed . Failure . outOfNEVector $ errs

pattern Result :: a -> Parsed a
pattern Result res <- Parsed (Success res)
  where
    Result res = Parsed . Success $ res

{-# COMPLETE Failed, Result #-}

-- A handler similar to 'either'. Designed for cases where the 'Parsed' is nested
-- deeply inside other structures.
handleParsed ::
  (NonEmptyVector ParsingError -> b) ->
  (a -> b) ->
  Parsed a ->
  b
handleParsed onFail onSuccess (Parsed comp) =
  either (onFail . intoNEVector) onSuccess . validationToEither $ comp

-- A handler similar to 'fromMaybe'. Designed for cases where the 'Parsed' is
-- nexted deeply inside other structures.
fromParsed :: a -> Parsed a -> a
fromParsed def (Parsed comp) =
  fromRight def . validationToEither $ comp

-- Represents one parsing step. In particular, we track whether we're parsing a
-- record field or not by way of 'ctx'. You can safely ignore 'ctx' in practice
-- - it exists only to ensure some extra type safety and for better errors.
--
-- 'fmap' post-processes the result of a 'Step' using a pure function. This is
-- mostly needed as a prerequisite for 'Applicative'.
--
-- 'pure' produces a 'Step' that ignores its input and produces the given value
-- (without error). '<*>' combines two 'Step's by cloning the input, sending it
-- to both component 'Step's, collecting the results, and combining them with
-- ($). It's more useful when used with 'liftA2', which types as so:
--
-- liftA2 :: (b -> c -> d) -> Step ctx a b -> Step ctx a c -> Step ctx a d
--
-- 'id' is a 'Step' which does nothing (essentially, just passes the input
-- through). '>>>' links together two 'Step's in the direction of the tracks.
newtype Step (ctx :: Maybe Symbol) (a :: Type) (b :: Type) =
  Step (Kleisli (Either ParsingErrorType) a b)
  deriving (Functor, Applicative) via (Kleisli (Either ParsingErrorType) a)
  deriving Category via (Kleisli (Either ParsingErrorType))

-- Run a 'Step' which draws no values from a record.
parse :: a -> Step 'Nothing a b -> Parsed b
parse input (Step comp) = Parsed $ case runKleisli comp input of
  Left err  -> Failure . singleton . OtherParsingError $ err
  Right res -> Success res

-- Run a 'Step' which draws values from a record. Errors will report the record
-- and field name.
parseField :: forall (field :: Symbol) (a :: Type) (b :: Type) .
  (Typeable a, KnownSymbol field) =>
  a -> Step ('Just field) a b -> Parsed b
parseField input (Step comp) = Parsed $ case runKleisli comp input of
  Left err  -> let recordType = tyName @a
                   fieldName = pack . symbolVal $ Proxy @field
                   errorType = err
                   err' = RecordParsingError{..} in
                Failure . singleton $ err'
  Right res -> Success res

-- Helpers for building 'Step's

-- A 'Step' that projects a field out of a record. This Just Works (tm) with
-- record dot syntax enabled, and tracks where the projection came from.
--
-- Formerly 'withField'.
project :: forall (field :: Symbol) (r :: Type) (a :: Type) .
  (HasField field r a) => Step ('Just field) r a
project = Step . Kleisli $ Right . getField @field

-- Lift an existing function which can fail into a 'Step'.
liftEither :: (a -> Either ParsingErrorType b) -> Step ctx a b
liftEither = Step . Kleisli

-- Lift a pure function into a 'Step'.
liftPure :: (a -> b) -> Step ctx a b
liftPure f = Step . Kleisli $ pure . f

-- A 'Step' that ensures its numerical input is not a negative value.
--
-- Formerly 'notNegative'.
nonNegative :: forall (a :: Type) (ctx :: Maybe Symbol) .
  (Num a, Eq a, Typeable a) => Step ctx a a
nonNegative = Step . Kleisli $ \input -> case signum input of
  (-1) -> Left . UnexpectedNegative $ tyName @a
  _    -> Right input

-- A 'Step' that ensures a 'Text' is not empty, and enshrines that into the type
-- of its output.
--
-- Formerly 'textNotEmpty'.
nonEmptyText :: forall (ctx :: Maybe Symbol) . Step ctx Text NonEmptyText
nonEmptyText = Step . Kleisli $
  maybe (Left UnexpectedEmptyText) Right . preview nonEmpty

-- A 'Step' which ensures that a 'Maybe' value is present, erroring otherwise,
-- and enshrines that by having its output be outside of 'Maybe'.
--
-- Formerly 'extractJust'.
mandated :: forall (a :: Type) (ctx :: Maybe Symbol) .
  (Typeable a) => Step ctx (Maybe a) a
mandated = Step . Kleisli $ maybe (Left mkError) Right
  where
    mkError :: ParsingErrorType
    mkError = MandatoryValueMissing (tyName @a)

-- A 'Step' which either promotes a value out of 'Maybe', or replaces it with
-- the provided default.
--
-- Formerly 'extractMaybeWithDefault'.
defaulting :: a -> Step ctx (Maybe a) a
defaulting def = Step . Kleisli $ Right . fromMaybe def

-- A 'Step' which parses 'Text' into an 'Integral' instance, erroring if this fails.
--
-- Formerly 'toInt'.
integral :: forall (a :: Type) (ctx :: Maybe Symbol) .
  (Integral a, Bounded a, Typeable a) => Step ctx Text a
integral = liftEither go
  where
    go s = maybe (Left (UnexpectedTextValue (tyName @a) s)) integerToBounded . readMaybe . unpack $ s

integerToBounded :: forall a. (Integral a, Bounded a, Typeable a) => Integer -> Either ParsingErrorType a
integerToBounded n
    | n < toInteger (minBound @a) = Left . Other $ "The value is less than minBound of " <>  tyName @a
    | n > toInteger (maxBound @a) = Left . Other $ "The value is greater than maxBound of " <> tyName @a
    | otherwise                   = Right (fromIntegral n)
{-# INLINE integerToBounded #-}
{-# SPECIALISE integerToBounded :: Integer -> Either ParsingErrorType Int #-}
{-# SPECIALISE integerToBounded :: Integer -> Either ParsingErrorType Int8 #-}
{-# SPECIALISE integerToBounded :: Integer -> Either ParsingErrorType Int16 #-}
{-# SPECIALISE integerToBounded :: Integer -> Either ParsingErrorType Int32 #-}
{-# SPECIALISE integerToBounded :: Integer -> Either ParsingErrorType Int64 #-}

-- A 'Step' which converts a 'LocalTime' into a 'UTCTime', on the assumption
-- that the input is zoned to UTC.
toUTC :: Step ctx LocalTime UTCTime
toUTC = Step . Kleisli $ Right . localTimeToUTC utc

-- A 'Step' which converts a 'a' into a 'Secret a'
-- Use for sensitive data fields
secret :: Step ctx a (Secret a)
secret = Step . Kleisli $ Right . makeSecret


-- Helpers for transforming 'Step's

-- Promote a 'Step' which works on pure values to working on values inside
-- 'Maybe'.
--
-- Formerly 'insideJust'.
around :: Step ctx a b -> Step ctx (Maybe a) (Maybe b)
around (Step comp) = Step . Kleisli . go . runKleisli $ comp
  where
    go ::
      (a -> Either ParsingErrorType b) ->
      Maybe a ->
      Either ParsingErrorType (Maybe b)
    go f = \case
      Nothing -> pure Nothing
      Just x  -> Just <$> f x

-- Promote a 'Step' which works on pure values to working on values inside
-- 'Secret'.
aroundSecret :: Step ctx a b -> Step ctx (Secret a) (Secret b)
aroundSecret (Step comp) = Step . Kleisli . go . runKleisli $ comp
  where
    go ::
      (a -> Either ParsingErrorType b) ->
      Secret a ->
      Either ParsingErrorType (Secret b)
    go f secret' = makeSecret <$> f (unsafeExtractSecret secret')

-- A more capable 'liftA2', which allows the 'combining function' to fail in a
-- way that's different to the 'Step's being combined.
reconcile ::
  forall (a :: Type) (b :: Type) (c :: Type) (d :: Type) (ctx :: Maybe Symbol) .
  (b -> c -> Either ParsingErrorType d) ->
  Step ctx a b ->
  Step ctx a c ->
  Step ctx a d
reconcile f (Step comp1) (Step comp2) =
  Step . Kleisli . go . runKleisli $ liftA2 (,) comp1 comp2
    where
      go ::
        (a -> Either ParsingErrorType (b, c)) ->
        a ->
        Either ParsingErrorType d
      go g x = case g x of
        Left err     -> Left err
        Right (l, r) -> f l r

-- Helpers

intoNEVector :: NESeq a -> NonEmptyVector a
intoNEVector (h :<|| t) = unfoldr1 go h t
  where
    go :: Seq a -> Maybe (a, Seq a)
    go = \case
      Empty     -> Nothing
      x :<| acc -> Just (x, acc)

outOfNEVector :: NonEmptyVector a -> NESeq a
outOfNEVector = uncurry (:<||) . fmap (Seq.fromList . V.toList) . uncons

tyName :: forall (a :: Type) . (Typeable a) => Text
tyName = pack . tyConName . typeRepTyCon $ typeRep @a
