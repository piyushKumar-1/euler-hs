# Autogenerated from euler.yaml. Do not edit.

self: super:
let
  euler-hs-src = super.eulerBuild.allowedPaths {
    root = ./..;
    paths = [
      ../src
      ../test
      ../euler-hs.cabal
    ];
  };

  hedis-repo = builtins.fetchTarball {
    url = "https://github.com/juspay/hedis/archive/46ea0ea78e6d8d1a2b1a66e6f08078a37864ad80.tar.gz";
    sha256 = "1xql164afmjc44s5p8ngws8y6jmb071nqdyrh0v5kz0jbb315595";
  };
  hedis-path = hedis-repo;

in 
super.eulerBuild.mkEulerHaskellOverlay self super
  (hself: hsuper: {
    hedis = self.eulerBuild.fastBuildExternal {
      drv = super.haskell.lib.unmarkBroken (hself.callCabal2nix "hedis" hedis-path { });
    };
    
    euler-hs = self.eulerBuild.fastBuild {
      drv = hself.callCabal2nix "euler-hs" euler-hs-src { };
      overrides = {
        # We want to run tests for our packages most of the time
        runTests = true;
      };
    };
  })
