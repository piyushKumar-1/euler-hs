# Autogenerated from euler.yaml. Do not edit.
# hash: eb6974d3955718c8308d4088f587090e60100824d4fd11c068b1ab2906195e1e
# time: 2020-10-21 14:52:55.131823

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
in 
super.eulerBuild.mkEulerHaskellOverlay self super
  (hself: hsuper: {
    hedis = self.eulerBuild.fastBuildExternal {
      drv = hself.callCabal2nix "hedis" (builtins.fetchTarball {
        url = "https://github.com/juspay/hedis/archive/46ea0ea78e6d8d1a2b1a66e6f08078a37864ad80.tar.gz";
        sha256 = "1xql164afmjc44s5p8ngws8y6jmb071nqdyrh0v5kz0jbb315595";
      }) { };
    };
    
    euler-hs = self.eulerBuild.fastBuild {
      drv = hself.callCabal2nix "euler-hs" euler-hs-src { };
      overrides = {
        # We want to run tests for our packages most of the time
        runTests = true;
      };
    };
  })
