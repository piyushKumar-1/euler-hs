{-# OPTIONS -fno-warn-deprecations #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module EulerHS.Extra.AltValidation
  (
    -- * Extra Validation
    Transform(..)
  , mkValidator
  , Transformer
  , Validator
  , V
  , Errors
  , module X
  , withField
  , runParser
  , extractJust
  , extractMaybeWithDefault
  , guarded
  , decode
  , insideJust
  , parValidate
  ) where

import           EulerHS.Prelude hiding (or, pred)
import qualified Prelude as P

import           Data.Data hiding (typeRep)
import           Data.Generics.Product.Fields
import qualified Data.Text as T
import           Data.Validation
import           Data.Validation as X
import           GHC.TypeLits
import           Type.Reflection


data VErrorPayload = VErrorPayload
  { status        :: Text
  , status_id     :: Maybe Int
  , error_code    :: Maybe Text
  , error_message :: Maybe Text
  , error_field   :: Maybe Text
  }
  deriving (Eq, Show, Generic)

validationError :: VErrorPayload
validationError = VErrorPayload
  { status = "invalid_request_error"
  , status_id = Nothing
  , error_code = Just "INVALID_REQUEST"
  , error_message = Nothing
  , error_field = Nothing
  }

-- | Context contains the name of validated field
type Ctx = Text

type Errors = [VErrorPayload]
type V a = Validation [VErrorPayload] a

-- TODO: Looks like Profunctor. Does it hold laws?
-- | Represents Transformer from one type to another.

--- | This class represents transformation abilities between types.
class Transform a b where
  transform :: a -> Validation Errors b

type Transformer a b = a -> ReaderT Ctx (Either Errors) b

-- | Represents the value parameter validator.
type Validator a = Transformer a a

-- | Takes error message and predicate and returns validation function
-- using default 'VErrorPayload'
mkValidator :: Text -> (t -> Bool) -> Validator t
mkValidator msg pred v = ReaderT (\ctx -> if not $ pred v
  then Left [validationError { error_message =  Just msg, error_field = Just ctx }]
  else pure v)

-- | Takes error message and predicate and returns validation function
-- using custom error
mkCustomValidator :: VErrorPayload -> Text -> (t -> Bool) -> Validator t
mkCustomValidator err msg pred v = ReaderT (\ctx -> if not $ pred v
  then Left [err { error_message = Just msg, error_field = Just ctx }]
  else pure v)

-- | Guards computations by a validation rule
guarded :: Text -> Bool -> ReaderT Ctx (Either Errors) ()
guarded msg pred | pred      = ReaderT (\_   -> pure ())
                 | otherwise = ReaderT (\ctx -> Left [validationError {error_message = Just msg, error_field = Just ctx }])

-- | Guards computations by a validation rule with a custom error
guardedCustom :: VErrorPayload -> Bool -> ReaderT Ctx (Either Errors) ()
guardedCustom err pred | pred      = ReaderT (\_   -> pure ())
                 | otherwise = ReaderT (\ctx -> Left [err {error_field = Just ctx }])

-- | Trying to decode 'Text' into a target type
decode :: forall t . (Data t, Read t) => Transformer Text t
decode v = ReaderT (\ctx -> case (readMaybe $ toString v) of
  Just x -> Right x
  _      -> Left [ validationError { error_message = Just ("Can't decode value" <> v <> ", should be one of " <> showConstructors @t)
                       , error_field = Just ctx}])

-- | Trying to decode 'Text' into a target type, use custom error
decodeCustom :: forall t . (Data t, Read t) => VErrorPayload -> Transformer Text t
decodeCustom err v = ReaderT (\ctx -> case (readMaybe $ toString v) of
  Just x -> Right x
  _      -> Left [ err { error_message = Just ("Can't decode value" <> v <> ", should be one of " <> showConstructors @t)
                       , error_field = Just ctx}])

insideJust :: Transformer a b -> Transformer (Maybe a) (Maybe b)
insideJust _ Nothing    = pure Nothing
insideJust val (Just a) = Just <$> val a

-- | Trying to extract the argument from Maybe type
--   if value is Nothing then raise Failure
extractJust :: Transformer (Maybe a) a
extractJust r = ReaderT (\ctx -> maybe (Left [validationError { error_message = Just "mandatory value is not present", error_field = Just ctx }]) Right r)

extractMaybeWithDefault :: a -> Transformer (Maybe a) a
extractMaybeWithDefault d r = ReaderT (\_ -> maybe (Right d) Right r)

-- | Extract value and run validators on it
withField
  :: forall (f :: Symbol) v r a
   . (Generic r, HasField' f r v, KnownSymbol f)
  => r -> Transformer v a -> Validation Errors a
withField rec pav = fromEither $ runReaderT (pav $ getField @f rec) $ fieldName_ @f

-- | Run a custom parser
runParser
  :: forall a
   . ReaderT Ctx (Either Errors) a
  -> Text
  -> Validation Errors a
runParser p err = fromEither $ runReaderT p err

-- | Return text representation of constructors of a given type
showConstructors :: forall t . Data t => Text
showConstructors = T.pack $ show $ getConstructors @t

-- | Return list with constructors of a given type
getConstructors :: forall t . Data t => [Constr]
getConstructors = dataTypeConstrs (dataTypeOf (undefined :: t))

-- | Return given 'Symbol' as 'Text'
-- >>> fieldName @"userId"
-- "userId"
fieldName_ :: forall (f :: Symbol) . KnownSymbol f => Text
fieldName_ = T.pack $ ((filter (/='"'))) $ P.show $ typeRep @f

parValidate :: [Validator a] -> Validator a
parValidate vals a = ReaderT (\ctx -> toEither $ foldr (*>) (pure a) $ fmap (mapper ctx) vals)
  where
    mapper ctx val = fromEither $ runReaderT (val a) ctx
