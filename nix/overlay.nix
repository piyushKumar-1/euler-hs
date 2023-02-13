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
    url = "https://github.com/juspay/hedis/archive/f8b11e0512b864f165a4a086713b41a0e3d622ea.tar.gz";
    sha256 = "0rgv5r22mwf1qpjlms2x9kbaqgmfsly8kp6v1i3m936c1ii39dr5";
  };
  hedis-path = hedis-repo;

  cereal-repo = builtins.fetchTarball {
    url = "https://github.com/juspay/cereal/archive/213f145ccbd99e630ee832d2f5b22894c810d3cc.tar.gz";
    sha256 = "1w925sg7wsycq40ay124mbwnp9hhbn0gkl39d210vq6qki632k0n";
  };
  cereal-path = cereal-repo;

in
super.eulerBuild.mkEulerHaskellOverlay self super
  (hself: hsuper: {
    hedis = self.eulerBuild.fastBuildExternal {
      drv = super.haskell.lib.unmarkBroken (hself.callCabal2nix "hedis" hedis-path { });
    };
    record-dot-preprocessor = self.eulerBuild.fastBuildExternal {
      drv = super.haskell.lib.unmarkBroken (hself.callHackageDirect {
        pkg = "record-dot-preprocessor";
        ver = "0.2.14";
        sha256 = "11zg0wgiy2awsblmazzk5rm22sr11fm3gyj9zsh230339ckrha88";
      } { });
    };
    servant = self.eulerBuild.fastBuildExternal {
      drv = super.haskell.lib.unmarkBroken (hself.callHackageDirect {
        pkg = "servant";
        ver = "0.18.3";
        sha256 = "0dklk3i3bcg85rzrmiggkypkw7pwkh6m5dlrjipsw0njfmlhqfz6";
      } { });
      overrides = {
        enableProfiling = true;
      };
    };
    servant-server = self.eulerBuild.fastBuildExternal {
      drv = super.haskell.lib.unmarkBroken (hself.callHackageDirect {
        pkg = "servant-server";
        ver = "0.18.3";
        sha256 = "0388v7drjj0gph99kszxivi4qijbckys67bkbv6384g2zrdplvj7";
      } { });
      overrides = {
        enableProfiling = true;
      };
    };
    servant-mock = self.eulerBuild.fastBuildExternal {
      drv = self.haskell.lib.doJailbreak (super.haskell.lib.unmarkBroken (hself.callHackageDirect {
        pkg = "servant-mock";
        ver = "0.8.7";
        sha256 = "1r0f18npxh9k9ziyc0l216cjpjbm3j6gbzganbxsh7byrym19np0";
      } { }));
      overrides = {
        enableProfiling = true;
      };
    };
    servant-client = self.eulerBuild.fastBuildExternal {
      drv = super.haskell.lib.unmarkBroken (hself.callHackageDirect {
        pkg = "servant-client";
        ver = "0.18.3";
        sha256 = "1gz2a0h9xnc1hmfp5bv84pg7vi45b9rvjk1fgy51zizg8vdlagvg";
      } { });
      overrides = {
        enableProfiling = true;
      };
    };
    servant-client-core = self.eulerBuild.fastBuildExternal {
      drv = super.haskell.lib.unmarkBroken (hself.callHackageDirect {
        pkg = "servant-client-core";
        ver = "0.18.3";
        sha256 = "1x0f3kalzrwj2blgsmk269m37bb7sygw6lr7dbp0rk6jbrfrkjm1";
      } { });
      overrides = {
        enableProfiling = true;
      };
    };
    cereal = self.eulerBuild.fastBuildExternal {
      drv = super.haskell.lib.unmarkBroken (hself.callCabal2nix "cereal" cereal-path { });
      overrides = {
        enableProfiling = true;
      };
    };
    beam-mysql = self.eulerBuild.fastBuildExternal {
      drv = super.haskell.lib.unmarkBroken (hsuper.beam-mysql);
    };
    beam = self.eulerBuild.fastBuildExternal {
      drv = super.haskell.lib.unmarkBroken (hsuper.beam);
    };
    haskell-sequelize = self.eulerBuild.fastBuildExternal {
      drv = super.haskell.lib.unmarkBroken (hsuper.haskell-sequelize);
    };
    
    euler-hs = self.eulerBuild.fastBuild {
      drv = super.haskell.lib.addBuildTools (hself.callCabal2nix "euler-hs" euler-hs-src { }) (with self; [ redis ]);
      overrides = {
        # We want to run tests for our packages most of the time
        runTests = true;
      };
    };
  })
