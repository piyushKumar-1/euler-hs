{ eulerBuild }:
let
  inherit (eulerBuild) fetchFromGitHub;

  beam-repo = fetchFromGitHub {
    owner = "juspay";
    repo = "beam";
    rev = "f1264f1139f5b8e3315351d086870de9318dbc45";
    sha256 = "1pv7rj9qqq1gdnlq8sczhjw2fpfxy9mbqfbdnz51xabmrps7nrjr";
  };

  beam-core-path = "${beam-repo}/beam-core";
  beam-migrate-path = "${beam-repo}/beam-migrate";
  beam-postgres-path = "${beam-repo}/beam-postgres";
  beam-sqlite-path = "${beam-repo}/beam-sqlite";

  beam-mysql-repo = fetchFromGitHub {
    owner = "juspay";
    repo = "beam-mysql";
    rev = "3382f1b07ee77883ff68af2a1e16776b46dc7b18";
    sha256 = "0428d0aa81jsjlbss258xdhcafdr441wg201v1g2nk6bapwknk2d";
  };

  beam-mysql-path = beam-mysql-repo;
in
eulerBuild.mkHaskellOverlay
  (self: super: hself: hsuper: rec {
    beam-migrate = with self.haskell.lib;
      disableLibraryProfiling
        (hself.callCabal2nix "beam-migrate" beam-migrate-path {
          haskell-src-exts = haskell-src-exts_1_21_1;
        });

    # needed for ClassA error in beam-migrate
    haskell-src-exts_1_21_1 =
      hself.callPackage ../packages/haskell-src-exts.nix { };
    # TODO: use this override (will cause a lot of rebuilds)
    # haskell-src-exts = hself.haskell-src-exts_1_21_1;

    # TODO: remove this override after enabled HSE above
    # needed for hspec-wai-json used in euler-api-order
    haskell-src-meta = self.haskell.lib.dontCheck
      (hsuper.haskell-src-meta.override {
        haskell-src-exts = haskell-src-exts_1_21_1;
      });

    beam-core = self.haskell.lib.disableLibraryProfiling
      (hself.callCabal2nix "beam-core" beam-core-path { });
    beam-sqlite =
      self.haskell.lib.disableLibraryProfiling
        (hself.callCabal2nix "beam-sqlite" beam-sqlite-path { });

    beam-postgres = with self.haskell.lib;
      dontCheck (disableLibraryProfiling
        (hself.callCabal2nix "beam-postgres" beam-postgres-path { }));

    beam-mysql = self.haskell.lib.disableLibraryProfiling
      (hself.callCabal2nix "beam-mysql" beam-mysql-path { });
  })
