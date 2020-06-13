{ eulerBuild }:
let
  inherit (eulerBuild) fetchFromGitHub;

  beam-repo = fetchFromGitHub {
    owner = "juspay";
    repo = "beam";
    rev = "b626c85f4d86b0960359d8b99613a9a0439667a2";
    sha256 = "1qhrwhirk1ksf4w7hncypdiazsgqw10dhsi28yrdsvll6hh935yp";
  };

  beam-core-path = "${beam-repo}/beam-core";
  beam-migrate-path = "${beam-repo}/beam-migrate";
  beam-postgres-path = "${beam-repo}/beam-postgres";
  beam-sqlite-path = "${beam-repo}/beam-sqlite";

  beam-mysql-repo = fetchFromGitHub {
    owner = "juspay";
    repo = "beam-mysql";
    rev = "7960580e041188459cfafef9c49aa6535ef4243e";
    sha256 = "1j96qgb3h1k325h2yin3jkj1105is58rdz9mci228wy65ak72dhv";
  };

  beam-mysql-path = beam-mysql-repo;
in
eulerBuild.mkEulerHaskellOverlay
  (self: super: hself: hsuper:
    let 
      # needed for ClassA error in beam-migrate
      haskell-src-exts_1_21_1 =
        eulerBuild.fastBuildExternal {
          drv = hself.callPackage ../packages/haskell-src-exts.nix { };
        };
    in {
      beam-migrate =
        eulerBuild.fastBuildExternal {
          drv = hself.callCabal2nix "beam-migrate" beam-migrate-path {
            haskell-src-exts = haskell-src-exts_1_21_1;
          };
        };

      # TODO: use this override (will cause a lot of rebuilds)
      # haskell-src-exts = hself.haskell-src-exts_1_21_1;

      # TODO: remove this override after enabled HSE above
      # needed for hspec-wai-json used in euler-api-order
      # dontCheck
      haskell-src-meta =
        eulerBuild.fastBuildExternal {
          drv = hsuper.haskell-src-meta.override {
            haskell-src-exts = haskell-src-exts_1_21_1;
          };
        };

      beam-core = eulerBuild.fastBuildExternal {
        drv = hself.callCabal2nix "beam-core" beam-core-path { };
      };
      beam-sqlite = eulerBuild.fastBuildExternal {
        drv = hself.callCabal2nix "beam-sqlite" beam-sqlite-path { };
      };

      # dontCheck
      beam-postgres = eulerBuild.fastBuildExternal {
        drv = hself.callCabal2nix "beam-postgres" beam-postgres-path { };
      };

      beam-mysql = eulerBuild.fastBuildExternal {
        drv = hself.callCabal2nix "beam-mysql" beam-mysql-path { };
      };
  })
