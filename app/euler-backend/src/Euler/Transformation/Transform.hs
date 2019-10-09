{-# LANGUAGE AllowAmbiguousTypes #-}
module Euler.Transformation.Transform
  ( Transform(..)
  , mkValidator
  , decodeTo
  , fromMaybe'
  , takeField
  , (<?*>)
  , (<!*>)
  , (<<|>>)
  , showConstructors
  , getConstructors
  , 

  ) where

import EulerHS.Prelude hiding (pack, pred, or)
import qualified Prelude as P

import Data.Generics.Product.Fields
import Data.Validation
import Data.Functor.Alt
import GHC.TypeLits
import Type.Reflection
import Control.Lens hiding (transform, cons)
import qualified Data.Text as T
import Data.List.NonEmpty (cons)
import Data.Data hiding (typeRep)

class Transform a b where
  transform :: a -> Validation [Text] b


mkValidator :: Text -> (t -> Bool) -> NonEmpty (Text -> t -> Validation [Text] t)
mkValidator err pred = (\fn v -> if pred v then _Success # v else _Failure # [fn <> " " <> err]) :| []

decodeTo :: forall t. (Data t, Read t) => (Text, Validation [Text] Text) -> (Text, Validation [Text] t)
decodeTo (f, Failure e) = (f, Failure e)
decodeTo (f, Success v) = case (readMaybe $ toString v) of
  Just x -> (f, _Success # x)
  _ -> (f, _Failure # ["Can't decode " <> v <> " from field " <> f <> ", should be one of " <> (showConstructors @t)])

fromMaybe' :: forall (f :: Symbol) t r
  . ( HasField' f r (Maybe t), KnownSymbol f)
  => r -> (Text, Validation [Text] t)
fromMaybe' r = ((fieldName_ @f) , isPresent' (fieldName_ @f) $ getField @f r)

isPresent' :: Text -> Maybe t -> Validation [Text] t
isPresent' f v = maybe (_Failure # [f <> " not present"]) (_Success # ) v

takeField :: forall (f :: Symbol) v r
  .(Generic r, HasField' f r v, KnownSymbol f) 
  =>  r -> (Text, Validation [Text] v)
takeField r = (fieldName_ @f, Success $ getField @f r)

ifPresent :: (Text, Validation [Text] (Maybe v))
  -> NonEmpty((Text -> v -> Validation [Text] v)) --(Text -> v -> Validation [Text] v)
  -> Validation [Text] (Maybe v)
ifPresent (fName, Success (Just v)) vs = Just <$> (appValidators vs fName v)
ifPresent (_, Success Nothing) _ = Success Nothing
ifPresent (_, Failure e) _ = Failure e

-- for optional fields
infix 5 <?*>
(<?*>) :: (Text, Validation [Text] (Maybe v))
  -> NonEmpty((Text -> v -> Validation [Text] v)) --(Text -> v -> Validation [Text] v)
  -> Validation [Text] (Maybe v)
(<?*>) = ifPresent

ifTransformed :: (Text, Validation [Text] v)
  -> NonEmpty((Text -> v -> Validation [Text] v)) --(Text -> v -> Validation [Text] v)
  -> Validation [Text] v
ifTransformed (_, Failure e) _ = Failure e
ifTransformed (fName, Success a) vs = appValidators vs fName a

-- for mandatory fields
infix 5 <!*>
(<!*>) :: (Text, Validation [Text] v)
  -> NonEmpty((Text -> v -> Validation [Text] v)) --(Text -> v -> Validation [Text] v)
  -> Validation [Text] v
(<!*>) = ifTransformed

appValidators ::  NonEmpty((Text -> v -> Validation [Text] v)) -> Text -> v -> Validation [Text] v
appValidators validators fName v = foldr1 (<*) $ map (($ (fName, v)).uncurry) validators

showConstructors :: forall t. (Data t) => Text
showConstructors = T.pack $ show $ getConstructors @t

getConstructors :: forall t. (Data t) => [Constr]
getConstructors = dataTypeConstrs (dataTypeOf (undefined :: t))

fieldName_ :: forall (f :: Symbol).(KnownSymbol f) => Text
fieldName_ = T.pack $ ((filter (/='"'))) $ P.show $ typeRep @f

infixl 3 <<|>>
(<<|>>) :: Semigroup err => Validation err a -> Validation err a -> Validation err a
(<<|>>) = or
{-# INLINE (<<|>>) #-}

-- unlike (<!>) 'or' collect errors from both variants
or :: Semigroup err => Validation err a -> Validation err a -> Validation err a
or (Failure e1) (Failure e2) =
    Failure (e1 <> e2)
or (Failure _) (Success a) = Success a
or (Success a) _ = Success a
