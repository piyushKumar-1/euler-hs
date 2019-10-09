{-# OPTIONS -fno-warn-deprecations #-}
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

import EulerHS.Prelude hiding ( pred, or)
import qualified Prelude as P

import Data.Generics.Product.Fields
import Data.Validation
import GHC.TypeLits
import Type.Reflection
import Control.Lens hiding (transform, cons)
import qualified Data.Text as T
import Data.Data hiding (typeRep)

class Transform a b where
  transform :: a -> Validation [Text] b

-- takes error message and predicate and return validation function
mkValidator :: Text -> (t -> Bool) -> NonEmpty (Text -> t -> Validation [Text] t)
mkValidator err pred = (\fn v -> if pred v then _Success # v else _Failure # [fn <> " " <> err]) :| []

-- trying to decode Text to target type
decodeTo :: forall t. (Data t, Read t) => (Text, Validation [Text] Text) -> (Text, Validation [Text] t)
decodeTo (f, Failure e) = (f, Failure e)
decodeTo (f, Success v) = case (readMaybe $ toString v) of
  Just x -> (f, _Success # x)
  _ -> (f, _Failure # ["Can't decode " <> v <> " from field " <> f <> ", should be one of " <> (showConstructors @t)])

-- trying to extract the argument from Maybe type
-- if value is Nothing then raise Failure
fromMaybe' :: forall (f :: Symbol) t r
  . ( HasField' f r (Maybe t), KnownSymbol f)
  => r -> (Text, Validation [Text] t)
fromMaybe' r = ((fieldName_ @f) , isPresent' (fieldName_ @f) $ getField @f r)

isPresent' :: Text -> Maybe t -> Validation [Text] t
isPresent' f v = maybe (_Failure # [f <> " not present"]) (_Success # ) v

--extract value and put it in 'Success'
takeField :: forall (f :: Symbol) v r
  .(Generic r, HasField' f r v, KnownSymbol f) 
  =>  r -> (Text, Validation [Text] v)
takeField r = (fieldName_ @f, Success $ getField @f r)

-- applies validators to the extracted value
-- for optional fields
ifPresent :: (Text, Validation [Text] (Maybe v))
  -> NonEmpty((Text -> v -> Validation [Text] v)) --(Text -> v -> Validation [Text] v)
  -> Validation [Text] (Maybe v)
ifPresent (fName, Success (Just v)) vs = Just <$> (appValidators vs fName v)
ifPresent (_, Success Nothing) _ = Success Nothing
ifPresent (_, Failure e) _ = Failure e

-- applies validators to the extracted value
-- for optional fields
infix 5 <?*>
(<?*>) :: (Text, Validation [Text] (Maybe v))
  -> NonEmpty((Text -> v -> Validation [Text] v)) --(Text -> v -> Validation [Text] v)
  -> Validation [Text] (Maybe v)
(<?*>) = ifPresent

-- applies validators to the extracted value
-- for mandatory fields
ifTransformed :: (Text, Validation [Text] v)
  -> NonEmpty((Text -> v -> Validation [Text] v)) --(Text -> v -> Validation [Text] v)
  -> Validation [Text] v
ifTransformed (_, Failure e) _ = Failure e
ifTransformed (fName, Success a) vs = appValidators vs fName a

-- applies validators to the extracted value
-- for mandatory fields
infix 5 <!*>
(<!*>) :: (Text, Validation [Text] v)
  -> NonEmpty((Text -> v -> Validation [Text] v)) --(Text -> v -> Validation [Text] v)
  -> Validation [Text] v
(<!*>) = ifTransformed

appValidators ::  NonEmpty((Text -> v -> Validation [Text] v)) -> Text -> v -> Validation [Text] v
appValidators validators fName v = foldr1 (<*) $ map (($ (fName, v)).uncurry) validators

-- return text representation of constructors of a given type
showConstructors :: forall t. (Data t) => Text
showConstructors = T.pack $ show $ getConstructors @t

-- return list with constructors of a given type
getConstructors :: forall t. (Data t) => [Constr]
getConstructors = dataTypeConstrs (dataTypeOf (undefined :: t))

fieldName_ :: forall (f :: Symbol).(KnownSymbol f) => Text
fieldName_ = T.pack $ ((filter (/='"'))) $ P.show $ typeRep @f


-- It is used in situations when we try to create several variants at the same time
-- and there is a probability of failure of all variants at once.
-- In case of failure collects validation errors of all variants.
-- On the contrary <!> gives validation errors only from the first option 
or :: Semigroup err => Validation err a -> Validation err a -> Validation err a
or (Failure e1) (Failure e2) =
    Failure (e1 <> e2)
or (Failure _) (Success a) = Success a
or (Success a) _ = Success a

infixl 3 <<|>>
(<<|>>) :: Semigroup err => Validation err a -> Validation err a -> Validation err a
(<<|>>) = or
{-# INLINE (<<|>>) #-}


-- <!> (defined in Data.Validation)
-- it is used in situations when we try to create several variants at the same time
-- and there is a probability of failure of all variants at once.
-- In case of failure returns validation errors only from the first option.
-- On the contrary <<|>> collects validation errors of all variants. 
