{-# language TemplateHaskell #-}

module Euler.GenericLensTH where


import EulerHS.Prelude
import Language.Haskell.TH
import Data.Generics.Product.Fields


prepareName :: String -> String
prepareName strName = "_" <> strName


makeLens :: String -> Q [Dec]
makeLens strName = do
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
          TyConI (DataD _ _ _ _ [RecC _ varbangtypes] _) -> extractName <$> varbangtypes
          ast -> error $ unlines
            [ ""
            , "-----------------------------------"
            , "| Generic lens generator. Unsupported structure: "
            , "-----------------------------------"
            , show (ppr ast)
            , "-----------------------------------"
            , "| Only record-like types allowed (with single data-constructor and named fields)"
            , "-----------------------------------"
            , ""
            ]


    lookedUpNames <- traverse ((\name -> fmap (name, ) $ lookupValueName $ prepareName name)) fieldNames
    let uniqNames = fmap fst $ filter (isNothing . snd) lookedUpNames

    -- let duplNames = fmap fst $ filter (isJust    . snd) lookedUpNames
    -- liftIO $ print duplNames

    join <$> traverse makeLens uniqNames

    where
      extractName (_name, _, _) = nameBase _name
