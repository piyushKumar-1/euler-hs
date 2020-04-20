module Main where

import EulerHS.Prelude

import qualified Euler.OrderStatus as OrderStatus



main :: IO ()
main = do
  OrderStatus.execOrderStatusBench
