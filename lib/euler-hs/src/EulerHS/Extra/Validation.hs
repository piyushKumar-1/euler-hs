{-# OPTIONS -fno-warn-deprecations #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module EulerHS.Extra.Validation
  ( 
    -- * Extra Validation
    Transform(..)
  , mkValidator
  , Transformer
  , Validator
  , Errors
  , module X
  , withField
  , extractJust
  , decode
  , insideJust
  , parValidate
  ) where

import EulerHS.Prelude hiding (pred, or)
import qualified Prelude as P

import Data.Generics.Product.Fields
import Data.Validation
import Data.Validation as X
import Data.Data hiding (typeRep)
import GHC.TypeLits
import Type.Reflection
import Control.Lens hiding (transform, cons)
import qualified Data.Text as T

type Ctx = Text
type Errors = [Text]

-- TODO: Looks like Profunctor. Does it hold laws?
-- | Represents Transformer from one type to another.
type Transformer a b = a -> ReaderT Ctx (Either Errors) b

-- | Represents the value parameter validator.
type Validator a = Transformer a a

-- | This class represents transformation abilities between types.
class Transform a b where
  transform :: a -> Validation [Text] b

-- | Takes error message and predicate and return validation function
mkValidator :: Text -> (t -> Bool) -> Validator t
mkValidator err pred v = ReaderT (\ctx -> if not $ pred v
  then Left [ctx <> " " <> err]
  else pure v)

-- | Trying to decode Text to target type
decode :: forall t . (Data t, Read t) => Transformer Text t
decode v = ReaderT (\ctx -> case (readMaybe $ toString v) of
  Just x -> Right x
  _      -> Left ["Can't decode " <> v <> " from field " <> ctx <> ", should be one of " <> showConstructors @t])

insideJust :: Transformer a b -> Transformer (Maybe a) (Maybe b)
insideJust val Nothing  = pure Nothing
insideJust val (Just a) = Just <$> val a

-- | Trying to extract the argument from Maybe type
--   if value is Nothing then raise Failure
extractJust :: Transformer (Maybe a) a
extractJust r = ReaderT (\ctx -> maybe (Left [ctx <> " not present"]) Right r)

-- | Extract value and run validators on it
withField
  :: forall (f :: Symbol) v r a
   . (Generic r, HasField' f r v, KnownSymbol f)
  => r -> Transformer v a -> Validation Errors a
withField rec pav = fromEither $ runReaderT (pav $ getField @f rec) $ fieldName_ @f

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
