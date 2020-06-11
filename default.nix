{
  withHoogle ? true
, haskellCompiler ? "ghc883"
}:
let
  inherit (import <nixpkgs> {}) fetchFromGitHub;
  # Date:   Mon May 18 19:30:42 2020 -0500
  nixpkgs = fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs-channels";
    rev = "0f5ce2fac0c726036ca69a5524c59a49e2973dd4";
    sha256 = "0nkk492aa7pr0d30vv1aw192wc16wpa1j02925pldc09s9m9i0r3";
  };

  eulerBuild = import ./nix/euler-build {
    inherit nixpkgs;
    inherit haskellCompiler;
  };

  ghc-overlay = eulerBuild.importOverlay ./nix/overlays/ghc.nix {
    inherit withHoogle;
  };
  beam-overlay = eulerBuild.importOverlay ./nix/overlays/beam.nix { };

  euler-hs-src = eulerBuild.allowedPaths {
    root =  ./.;
    paths = [
      ./euler-hs.cabal
      ./src
      ./test
      ./testDB
    ];
  };
  euler-hs-overlay = eulerBuild.importOverlay ./nix/overlays/euler-hs.nix {
    src = euler-hs-src;
  };

  allUsedOverlays = [ ghc-overlay beam-overlay euler-hs-overlay ];

  pkgs = import nixpkgs {
    overlays = allUsedOverlays;
  };
in {
  inherit pkgs;
  euler-hs = pkgs.eulerHaskellPackages.euler-hs;

  inherit ghc-overlay;
  inherit beam-overlay;
  inherit euler-hs-overlay;
  overlay = eulerBuild.composeOverlays allUsedOverlays;

  # TODO: (?) put in a separate repo together with ./nix/euler-build
  inherit eulerBuild;
}
