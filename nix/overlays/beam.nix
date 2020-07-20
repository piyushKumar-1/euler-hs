{ eulerBuild }:
let
  inherit (eulerBuild) fetchFromGitHub;

  beam-repo = fetchFromGitHub {
    owner = "juspay";
    repo = "beam";
    rev = "47f62de4cbfafe13891846ef795609b6f8050729";
    sha256 = "1w3iahns22ylccs0ka064ipajxkq52s2vcwsbw56zfrp0qnq0kn2";
  };

  beam-core-path = "${beam-repo}/beam-core";
  beam-migrate-path = "${beam-repo}/beam-migrate";
  beam-postgres-path = "${beam-repo}/beam-postgres";
  beam-sqlite-path = "${beam-repo}/beam-sqlite";

  beam-mysql-repo = fetchFromGitHub {
    owner = "juspay";
    repo = "beam-mysql";
    rev = "5d17599e02f30c220b679439ee0c1025e3086250";
    sha256 = "09n42cg99cn3kj00ns1c3ds690w0y59rlliyp32pqd6gbcqq7bxr";
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
