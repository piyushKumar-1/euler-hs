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

  # TODO: change this to juspay tomorrow
  beam-repo = fetchFromGitHub {
    owner = "tathougies";
    repo = "beam";
    rev = "44d7cf4752853fb33d49a008c3e7d4a9a4bd1ee7";
    sha256 = "0gz1fch9awr2p08yiflmck0qyhprwpzmrslj2v6fp2lhnjbxkl59";
  };

  beam-core-path = "${beam-repo}/beam-core";
  beam-migrate-path = "${beam-repo}/beam-migrate";
  beam-postgres-path = "${beam-repo}/beam-postgres";
  beam-sqlite-path = "${beam-repo}/beam-sqlite";

  beam-mysql-repo = fetchFromGitHub {
    owner = "juspay";
    repo = "beam-mysql";
    rev = "a0435c4883fbe5a249b3f68b2e639a449aa5849b";
    sha256 = "0dp2dbnrg88mbhgwfph4gzv9ixgbq7ivkdlj536cypl0q9jwzq8n";
  };

  beam-mysql-path = beam-mysql-repo;

  config = {
    packageOverrides = pkgs: rec {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = self: super: rec {
          ghc =
            super.ghc // { withPackages = if withHoogle then super.ghc.withHoogle else super.ghc ; };
          ghcWithPackages =
            self.ghc.withPackages;
          # Overrides for broken packages in nix

          # TODO: point to juspay
          hedis =
            self.callPackage ./nix/hedis.nix { };
          # TODO: remove
          crc16 =
            self.callPackage ./nix/crc16.nix { };

          # TODO: bump in juspay and make upstream PR
          beam-migrate =
            (pkgs.haskell.lib.appendPatch
              (pkgs.haskell.lib.disableLibraryProfiling
                (self.callCabal2nix "beam-migrate" beam-migrate-path { }))
              ./nix/0001-Bump-bounds-for-beam-migrate-on-dependent-haskell-sr.patch
            ).override { haskell-src-exts = haskell-src-exts_1_21_1; };

          haskell-src-exts_1_21_1 = self.callPackage ./nix/haskell-src-exts.nix { };

          beam-sqlite =
            pkgs.haskell.lib.appendPatch
              (pkgs.haskell.lib.disableLibraryProfiling
                (self.callCabal2nix "beam-sqlite" beam-sqlite-path { }))
              ./nix/0001-Bump-hashable-bound-for-mysql-sqlite.patch;

          beam-core = pkgs.haskell.lib.disableLibraryProfiling
            (self.callCabal2nix "beam-core" "${beam-core-path}" { });
          beam-postgres =
            pkgs.haskell.lib.dontCheck
              (pkgs.haskell.lib.disableLibraryProfiling
                (self.callCabal2nix "beam-postgres" "${beam-postgres-path}" { }));
          beam-mysql = pkgs.haskell.lib.disableLibraryProfiling
            (self.callCabal2nix "beam-mysql" "${beam-mysql-path}" { });

          euler-hs =
            self.callCabal2nix "euler-hs" ./. { };
        };
      };
    };
  };
  pkgs =
    import nixpkgs { inherit config; };
in {
  pkgs = pkgs;
  euler-hs = with pkgs; haskell.lib.disableLibraryProfiling haskellPackages.euler-hs;
}
