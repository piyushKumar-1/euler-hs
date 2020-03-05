{-# language TemplateHaskell #-}

module Euler.GenericLensTH where


import EulerHS.Prelude
import Data.List (intersect)
import Language.Haskell.TH
import Data.Generics.Product.Fields


prepareName :: String -> String
prepareName strName = "_" <> strName


makeGenericLens :: String -> Q [Dec]
makeGenericLens strName = do
  let sym   = pure $ LitT (StrTyLit strName)
  let name  = mkName $ prepareName strName

  type_ <-             [t| forall s a . HasField' $sym s a => Lens s s a a |]
  body  <- NormalB <$> [e| field' @ $sym                                   |]
  pure $
    [ SigD       name  type_
    , ValD (VarP name) body  []
    ]


makeGenericLenses :: Name -> Q [Dec]
makeGenericLenses name = do
    tInfo <- reify name

    let fieldNames = case tInfo of
          TyConI (DataD _ _ _ _ (extractNamesFromCons -> []   ) _) -> error $ unlines
            [ ""
            , "-----------------------------------"
            , "| Generic lens generator. Intersection of field names is an empty list"
            , "| We consider this as error as it does not look like something you expect"
            , "-----------------------------------"
            , show (ppr tInfo)
            , "-----------------------------------"
            , show tInfo
            , "-----------------------------------"
            , "| Only types with record-like data constructors allowed"
            , "-----------------------------------"
            , ""
            ]

          TyConI (DataD _ _ _ _ (extractNamesFromCons -> names) _) -> names

          _ -> error $ unlines
            [ ""
            , "-----------------------------------"
            , "| Generic lens generator. Unsupported structure:"
            , "-----------------------------------"
            , show (ppr tInfo)
            , "-----------------------------------"
            , show tInfo
            , "-----------------------------------"
            , "| Only types with record-like data constructors allowed"
            , "-----------------------------------"
            , ""
            ]


    lookedUpNames <- traverse ((\name' -> fmap (name', ) $ lookupValueName $ prepareName name')) fieldNames
    let uniqNames = fmap fst $ filter (isNothing . snd) lookedUpNames

    -- let duplNames = fmap fst $ filter (isJust    . snd) lookedUpNames
    -- liftIO $ print duplNames

    join <$> traverse makeGenericLens uniqNames

    where
      extractNamesFromCon :: Con -> [String]
      extractNamesFromCon (RecC _ vbps) = fmap (\(n, _, _) -> nameBase n) vbps
      extractNamesFromCon _             = []

      extractNamesFromCons :: [Con] -> [String]
      extractNamesFromCons []       = []
      extractNamesFromCons (fmap extractNamesFromCon -> c : cs) = foldr intersect c cs


