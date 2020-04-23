-----------------------------------------------------------------------------
-- |
-- An excerpt from Relude library to build parsers for enumerations.
-- Have we got comparable analogues?

module WebService.Relude where

import EulerHS.Prelude
import qualified Data.Map.Strict as M


{- | @inverseMap f@ creates a function that is the inverse of a given function
@f@. It does so by constructing 'M.Map' internally for each value @f a@. The
implementation makes sure that the 'M.Map' is constructed only once and then
shared for every call.

__Memory usage note:__ don't inverse functions that have types like 'Int'
as their result. In this case the created 'M.Map' will have huge size.

The complexity of reversed mapping is \(\mathcal{O}(\log n)\).

__Performance note:__ make sure to specialize monomorphic type of your functions
that use 'inverseMap' to avoid 'M.Map' reconstruction.

One of the common 'inverseMap' use-case is inverting the 'show' or a 'show'-like
function.

>>> data Color = Red | Green | Blue deriving (Show, Enum, Bounded)
>>> parse = inverseMap show :: String -> Maybe Color
>>> parse "Red"
Just Red
>>> parse "Black"
Nothing

__Correctness note:__ 'inverseMap' expects /injective function/ as its argument,
i.e. the function must map distinct arguments to distinct values.

Typical usage of this function looks like this:

@
__data__ GhcVer
    = Ghc802
    | Ghc822
    | Ghc844
    | Ghc865
    | Ghc881
    __deriving__ ('Eq', 'Ord', 'Show', 'Enum', 'Bounded')

showGhcVer :: GhcVer -> 'Text'
showGhcVer = \\__case__
    Ghc802 -> "8.0.2"
    Ghc822 -> "8.2.2"
    Ghc844 -> "8.4.4"
    Ghc865 -> "8.6.5"
    Ghc881 -> "8.8.1"

parseGhcVer :: 'Text' -> 'Maybe' GhcVer
parseGhcVer = 'inverseMap' showGhcVer
@
-}
inverseMap
    :: forall a k .
       (Bounded a, Enum a, Ord k)
    => (a -> k)
    -> (k -> Maybe a)
inverseMap f = \k -> M.lookup k dict
  where
    dict :: M.Map k a
    dict = M.fromList $ map (mapToFst f) (universe @a)
{-# INLINE inverseMap #-}


{- | Returns all values of some 'Bounded' 'Enum' in ascending order.

>>> data TrafficLight = Red | Blue | Green deriving (Show, Enum, Bounded)
>>> universe :: [TrafficLight]
[Red,Blue,Green]
>>> universe :: [Bool]
[False,True]
-}
universe :: (Bounded a, Enum a) => [a]
universe = [minBound .. maxBound]
{-# INLINE universe #-}

{- | Apply a function, with the result in the fst slot,
and the value in the other.

A dual to 'mapToSnd'

>>> mapToFst (+1) 10
(11,10)
-}
mapToFst :: (a -> b) -> a -> (b, a)
mapToFst f a = (f a, a)
{-# INLINE mapToFst #-}