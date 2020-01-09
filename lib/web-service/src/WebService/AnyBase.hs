{-# LANGUAGE BangPatterns #-}
module WebService.AnyBase
  ( fromTo
  , hex2Flickr
  , flickr2Hex
  , bin
  , oct
  , dec
  , hex
  , flickr58
  ) where

import EulerHS.Prelude

import qualified Data.Text as T
import qualified Data.Map  as Map



bin :: String
bin = "01"

oct :: String
oct = "01234567"

dec :: String
dec = "0123456789"

hex :: String
hex = "0123456789abcdef"

flickr58 :: String
flickr58 = "123456789abcdefghijkmnopqrstuvwxyzABCDEFGHJKLMNPQRSTUVWXYZ"

toDec :: Map.Map Char Integer -> Integer -> String -> Maybe Integer
toDec srcAlphabetMap srcAlphabetBase n =
  let
    ns = sequence $ foldl' (\a x -> Map.lookup x srcAlphabetMap : a) [] n
    ppows = 1 : fmap (*srcAlphabetBase) ppows
  in  case ns of
        Nothing -> Nothing
        Just ns' -> pure $ foldl' (\a (p,nn) -> p * nn + a) 0 $ zip ppows ns'

fromDecToDst :: Map.Map Integer Char -> Integer -> Integer -> Maybe String
fromDecToDst dstAlphabetMap dstAlphabetBase decNumber = sequence r
  where
    r = go [] decNumber dstAlphabetBase
    go !l decNumber' dstAlphabetBase' =
      case quotRem decNumber' dstAlphabetBase' of
        (0,x) -> (Map.lookup x dstAlphabetMap) : l
        (nn,x) -> go ((Map.lookup x dstAlphabetMap):l) nn dstAlphabetBase'

fromTo :: String -> String -> String -> Maybe String
fromTo srcAlphabet dstAlphabet n = join $ fromDecToDst dstAlphabetMap dstAlphabetBase <$> decRep
  where
    srcAlphabetBase = toInteger $ length srcAlphabet
    dstAlphabetBase = toInteger $ length dstAlphabet
    srcAlphabetMap = Map.fromList $ zip srcAlphabet [0..]
    dstAlphabetMap = Map.fromList $ zip [0..] dstAlphabet
    decRep = toDec srcAlphabetMap srcAlphabetBase n

hex2Flickr :: T.Text -> Maybe T.Text
hex2Flickr n = fmap T.pack $
  fromTo hex flickr58 (T.unpack n)

flickr2Hex :: T.Text -> Maybe T.Text
flickr2Hex n = fmap T.pack $
    fromTo flickr58 hex (T.unpack n)
