{
  nixpkgs
, haskellCompiler
, fastBuildParams ? { runTests = false;
                      enableProfiling = false;
                      buildDocs = false;
                      enableBenchmarks = false;
                      dontOptimize = false;
                    }
}:
let
  # Needed for lib and fetchFromGitHub
  pkgs = import nixpkgs { };
  inherit (pkgs) fetchFromGitHub;
  inherit (pkgs) lib;

  importOverlay = path: params:
    import path ({ inherit eulerBuild; } // params);

  mkHaskellOverlayWith = pkgsAttrName: userOverrides:
    self: super: {
      "${pkgsAttrName}" =
        (super."${pkgsAttrName}"
          or super.haskell.packages."${haskellCompiler}").override (oldArgs: {
            overrides = self.lib.composeExtensions (oldArgs.overrides or (_: _: {}))
              (userOverrides self super);
          }
      );
    };

  mkHaskellOverlay = mkHaskellOverlayWith "haskellPackages";
  mkEulerHaskellOverlay = mkHaskellOverlayWith "eulerHaskellPackages";

  nix-inclusive = fetchFromGitHub {
    owner = "manveru";
    repo = "nix-inclusive";
    rev = "bb435b7dce2b8a27d174543f0d768646d0d48fa3";
    sha256 = "0fg4dbk7x62z86ah3qw4dlaiippgd0ckcqgm89zz5sapna1v4d56";
  };
  inclusive = import "${nix-inclusive}/inclusive.nix" { inherit lib; };

  allowedPaths = { root, paths }: inclusive root paths;

  composeOverlays = lib.foldl' lib.composeExtensions (_: _: {});

  fastBuild = { drv, overrides ? {} }:
    let
      params = fastBuildParams // overrides;

      modifiers =
        with pkgs.haskell.lib;
        pkgs.lib.optional (!params.runTests) dontCheck ++ 
        pkgs.lib.optional (!params.enableProfiling) disableLibraryProfiling ++
        pkgs.lib.optional (!params.buildDocs) dontHaddock ++
        pkgs.lib.optional (!params.enableBenchmarks) dontBenchmark ++
        pkgs.lib.optional params.dontOptimize disableOptimization;
    in
      pkgs.lib.pipe drv modifiers;

  fastBuildExternal = fastBuild;

  mkShell = import ./mk-shell.nix;

  eulerBuild = {
    inherit (pkgs) fetchFromGitHub;
    inherit importOverlay;
    inherit composeOverlays;
    inherit mkHaskellOverlay;
    inherit mkEulerHaskellOverlay;
    inherit allowedPaths;

    inherit fastBuild;
    inherit fastBuildExternal;

    inherit mkShell;
  };

in eulerBuild
