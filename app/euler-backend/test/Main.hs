module Main where

  import           Test.Hspec
  
  import qualified Euler.Tests.Transformation.TransactionSpec as TxnTransform
  
  main = hspec TxnTransform.spec