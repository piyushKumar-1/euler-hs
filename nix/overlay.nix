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
    servant = self.eulerBuild.fastBuildExternal {
      drv = super.haskell.lib.unmarkBroken (hself.callHackageDirect {
        pkg = "servant";
        ver = "0.18.2";
        sha256 = "0l2k895nxvw2ngr9201g3br6s9zab7mk5mhpjibyg8mxfbv75a8y";
      } { });
    };
    servant-server = self.eulerBuild.fastBuildExternal {
      drv = super.haskell.lib.unmarkBroken (hself.callHackageDirect {
        pkg = "servant-server";
        ver = "0.18.2";
        sha256 = "1kynxl7qg5z45bhi0k61sxn79xkgnq1z97ccqqs39wjyf45fj5yy";
      } { });
    };
    servant-mock = self.eulerBuild.fastBuildExternal {
      drv = super.haskell.lib.unmarkBroken (hself.callHackageDirect {
        pkg = "servant-mock";
        ver = "0.8.7";
        sha256 = "1r0f18npxh9k9ziyc0l216cjpjbm3j6gbzganbxsh7byrym19np0";
      } { });
    };
    servant-client = self.eulerBuild.fastBuildExternal {
      drv = super.haskell.lib.unmarkBroken (hself.callHackageDirect {
        pkg = "servant-client";
        ver = "0.18.2";
        sha256 = "0yip2s63ivrlrpficdipq60j2a6czg8agn18lpkkaxf3n55j4jr3";
      } { });
    };
    servant-client-core = self.eulerBuild.fastBuildExternal {
      drv = super.haskell.lib.unmarkBroken (hself.callHackageDirect {
        pkg = "servant-client-core";
        ver = "0.18.2";
        sha256 = "1hazxk1laklpm2c65zgkk2gn8mvlp682437071s04bqggk9b59sx";
      } { });
    };
    
    euler-hs = self.eulerBuild.fastBuild {
      drv = super.haskell.lib.addBuildTools (hself.callCabal2nix "euler-hs" euler-hs-src { }) (with self; [ redis ]);
      overrides = {
        # We want to run tests for our packages most of the time
        runTests = true;
      };
    };
  })
