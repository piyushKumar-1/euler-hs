{
  withHoogle ? true
}:
let
  inherit (import <nixpkgs> {}) fetchFromGitHub;
  nixpkgs = fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs-channels";
    rev = "0f5ce2fac0c726036ca69a5524c59a49e2973dd4";
    sha256 = "0nkk492aa7pr0d30vv1aw192wc16wpa1j02925pldc09s9m9i0r3";
  };

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

  hedis-repo = fetchFromGitHub {
    owner = "juspay";
    repo = "hedis";
    rev = "4ea54f16c0057acc99a9f0e9b63ea51ea4bf420e";
    sha256 = "094r4pxkc3h6w2vy4lha1zfdz29qihvkx2wi3mb7g1m3a6c7xp4h";
  };

  hedis-path = hedis-repo;

  config = {
    packageOverrides = pkgs: rec {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = self: super: rec {
          ghc =
            super.ghc // { withPackages = if withHoogle then super.ghc.withHoogle else super.ghc ; };
          ghcWithPackages =
            self.ghc.withPackages;

          # Overrides for broken packages in nix

          hedis = with pkgs.haskell.lib;
            dontCheck (disableLibraryProfiling
              (self.callCabal2nix "hedis" "${hedis-path}" { }));

          beam-migrate = with pkgs.haskell.lib;
            (disableLibraryProfiling
              (self.callCabal2nix "beam-migrate" beam-migrate-path { })
            ).override { haskell-src-exts = haskell-src-exts_1_21_1; };

          # needed for ClassA error in beam-migrate
          haskell-src-exts_1_21_1 = self.callPackage ./nix/haskell-src-exts.nix { };

          beam-core = pkgs.haskell.lib.disableLibraryProfiling
            (self.callCabal2nix "beam-core" "${beam-core-path}" { });
          beam-sqlite =
            pkgs.haskell.lib.disableLibraryProfiling
              (self.callCabal2nix "beam-sqlite" beam-sqlite-path { });

          beam-postgres = with pkgs.haskell.lib;
            dontCheck (disableLibraryProfiling
              (self.callCabal2nix "beam-postgres" "${beam-postgres-path}" { }));

          beam-mysql = pkgs.haskell.lib.disableLibraryProfiling
            (self.callCabal2nix "beam-mysql" "${beam-mysql-path}" { });

          euler-hs = with pkgs.haskell.lib;
            dontCheck
              (disableLibraryProfiling
                (self.callCabal2nix "euler-hs" ./. { }));

          # TODO: remove dontCheck above and remove this package
          # after fixing call untyped API test
          euler-hs-with-tests = pkgs.haskell.lib.disableLibraryProfiling
            (self.callCabal2nix "euler-hs" ./. { });
        };
      };
    };
  };
  pkgs =
    import nixpkgs { inherit config; };
in {
  pkgs = pkgs;
  euler-hs = pkgs.haskellPackages.euler-hs;
}
