{
  eulerBuild
, withHoogle
}:
eulerBuild.mkHaskellOverlay
  (self: super: hself: hsuper: {
    ghc = hsuper.ghc // {
      withPackages = if withHoogle
                    then hsuper.ghc.withHoogle
                    else hsuper.ghc;
    };
    ghcWithPackages =
      hself.ghc.withPackages;
  })
