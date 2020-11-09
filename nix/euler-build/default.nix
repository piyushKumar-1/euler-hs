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
    owner = "juspay";
    repo = "nix-inclusive";
    rev = "2ca1706029bfcf4bb7eaf17b4f32e49f436a148e";
    sha256 = "1y3vhqnbh5kg906fpw22h670ppl8238xwv0dx7zdcp22212zdjnx";
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

  eulerBuild = {
    inherit (pkgs) fetchFromGitHub;
    inherit importOverlay;
    inherit composeOverlays;
    inherit mkHaskellOverlay;
    inherit mkEulerHaskellOverlay;
    inherit allowedPaths;

    inherit fastBuild;
    inherit fastBuildExternal;
  };

in eulerBuild
