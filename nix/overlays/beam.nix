{ eulerBuild }:
let
  inherit (eulerBuild) fetchFromGitHub;

  beam-repo = fetchFromGitHub {
    owner = "juspay";
    repo = "beam";
    rev = "f6a35d5361e96c766ae977e9a07fa40c6622808e";
    sha256 = "09ajx46v8cnrfbdq3npkjspr9bjj72h8mmvg0hdfnlpnw9x3ykw9";
  };

  beam-core-path = "${beam-repo}/beam-core";
  beam-migrate-path = "${beam-repo}/beam-migrate";
  beam-postgres-path = "${beam-repo}/beam-postgres";
  beam-sqlite-path = "${beam-repo}/beam-sqlite";

  beam-mysql-repo = fetchFromGitHub {
    owner = "juspay";
    repo = "beam-mysql";
    rev = "ca7888c2573368fd691b327890f116881891ac4d";
    sha256 = "1114gnpyrlmrhk4csra4yd14rgmz105r3hv421mp96g9j0kfxkds";
  };

  beam-mysql-path = beam-mysql-repo;

  mysql-haskell-repo = fetchFromGitHub {
    owner = "juspay";
    repo = "mysql-haskell";
    rev = "788022d65538db422b02ecc0be138b862d2e5cee";
    sha256 = "030qq1hgh15zkwa6j6x568d248iyfaw5idj2hh2mvb7j8xd1l4lv";
  };

  mysql-haskell-path = mysql-haskell-repo;
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

      mysql-haskell = eulerBuild.fastBuildExternal {
        drv = hself.callCabal2nix "mysql-haskell" mysql-haskell-path { };
      };

      beam-mysql = eulerBuild.fastBuildExternal {
        drv = hself.callCabal2nix "beam-mysql" beam-mysql-path { };
      };
  })
