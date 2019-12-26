module Main (main) where

import Dashboard.Query.Types
import Data.Proxy (Proxy(..))
import Language.PureScript.Bridge
import Language.PureScript.Bridge.CodeGenSwitches
import Universum

utcTimeBridge :: BridgePart
utcTimeBridge =
  typeName ^== "Timestamp"
  >> TypeInfo "" "Dashboard.Query.ExtraTypes" "Timestamp" <$> psTypeParameters

pairBridge :: BridgePart
pairBridge =
  doCheck haskType isTuple
  >> doCheck typeParameters (\a -> length a == 2)
  >> (TypeInfo "" "Dashboard.Query.ExtraTypes" "Pair" <$> psTypeParameters)

tripleBridge :: BridgePart
tripleBridge =
  doCheck haskType isTuple
  >> doCheck typeParameters (\a -> length a == 3)
  >> (TypeInfo "" "Dashboard.Query.ExtraTypes" "Triple" <$> psTypeParameters)

-- Generic representations of the Haskell types to be converted to Purescript types
-- needed by the query client
clientTypes :: [SumType 'Haskell]
clientTypes = -- Query types
              [ mkSumType (Proxy :: Proxy Query)
              , mkSumType (Proxy :: Proxy Selection)
              , mkSumType (Proxy :: Proxy TableName)
              , mkSumType (Proxy :: Proxy Interval)
              , mkSumType (Proxy :: Proxy Filter)
              , mkSumType (Proxy :: Proxy GroupBy)
              , mkSumType (Proxy :: Proxy SelectOp)
              , mkSumType (Proxy :: Proxy SelectField)
              , mkSumType (Proxy :: Proxy Timestamp)
              , mkSumType (Proxy :: Proxy Milliseconds)
              , mkSumType (Proxy :: Proxy FieldName)
              , mkSumType (Proxy :: Proxy FilterOp)
              , mkSumType (Proxy :: Proxy Value)
              ]
              ++
              -- Query result types
              [ mkSumType (Proxy :: Proxy QueryResult)
              , mkSumType (Proxy :: Proxy QueryResultRow)
              ]
              ++
              -- Query error types
              [ mkSumType (Proxy :: Proxy QueryValidationError)
              , mkSumType (Proxy :: Proxy QueryErrorType)
              ]

genTypes :: String -> IO ()
genTypes writeTo = writePSTypesWith genOptions writeTo (buildBridge typeBridge) clientTypes
  where
    genOptions = defaultSwitch <> genForeign (ForeignOptions True)
    typeBridge = pairBridge <|> tripleBridge <|> utcTimeBridge <|> defaultBridge

main :: IO ()
main = genTypes "client/console/src"
