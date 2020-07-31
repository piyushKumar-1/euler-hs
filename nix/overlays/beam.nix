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
    rev = "65bb65b51e290e34586a98b0fe195509fafeafd7";
    sha256 = "15jjpc5y48cs7v80ghfnr21195i01qf1h6r6fqvjxsgw5mdb2gr0";
  };

  beam-mysql-path = beam-mysql-repo;

  bytestring-lexing-repo = fetchFromGitHub {
    owner = "juspay";
    repo = "bytestring-lexing";
    rev = "0a46db1139011736687cb50bbd3877d223bcb737";
    sha256 = "1jrwhlp8xs4m21xfr843278j3i7h4sxyjpq67l6lzc36pqan9zlz";
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
