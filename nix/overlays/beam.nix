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
    rev = "ca7888c2573368fd691b327890f116881891ac4d";
    sha256 = "1wl8503ig1hl6pfa65wip62c16izv8dn3bdbd6v4kqq8xh5fdqr3";
  };

  beam-mysql-path = beam-mysql-repo;

  bytestring-lexing-repo = fetchFromGitHub {
    owner = "juspay";
    repo = "bytestring-lexing";
    rev = "e35ff137b0425c7b8abfe0eef235fa2bb6c819ce";
    sha256 = "1ah4jzqhiwqx78zwyg5x41dnvhimpa2p97zgs3199dvc1iznn5yp";
  };

  bytestring-lexing-path = bytestring-lexing-repo;

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

      # Needed for floating point fix in mysql-haskell
      bytestring-lexing = eulerBuild.fastBuildExternal {
        drv = hself.callCabal2nix "bytestring-lexing" bytestring-lexing-path { };
      };

      # Uses bytestring-lexing
      binary-parsers = eulerBuild.fastBuildExternal {
        drv = hsuper.binary-parsers;
      };
      # Uses binary-parsers
      wire-streams = eulerBuild.fastBuildExternal {
        drv = hsuper.wire-streams;
      };

      mysql-haskell = eulerBuild.fastBuildExternal {
        drv = hself.callCabal2nix "mysql-haskell" mysql-haskell-path { };
      };

      beam-mysql = eulerBuild.fastBuildExternal {
        drv = hself.callCabal2nix "beam-mysql" beam-mysql-path { };
      };
  })
