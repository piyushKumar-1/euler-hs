let
  drv = import ./default.nix { withHoogle = true; };
  pkgs = drv.pkgs;
in drv.euler-hs.env.overrideAttrs (attrs: {
  buildInputs =
    with pkgs.haskellPackages;
    [ cabal-install
      cabal2nix
      ghcid
      hindent
      hlint
      stylish-haskell
    ] ++ [
      zlib
    ] ++ attrs.buildInputs;
})
