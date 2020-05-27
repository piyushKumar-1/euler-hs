{
  withHoogle ? true
}:
let
  inherit (import <nixpkgs> {}) fetchFromGitHub;
  nixpkgs = fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    rev = "19.09";
    sha256 = "0mhqhq21y5vrr1f30qd2bvydv4bbbslvyzclhw0kdxmkgg3z4c92";
  };

  beam-mysql-repo = fetchFromGitHub {
    owner = "juspay";
    repo = "beam-mysql";
    rev = "3d03598b11be40929ee95984a83c3d9ed4be8536";
    sha256 = "0jhziprq0knxsbakg4r45db5w9sh7xyxqaldc4l04qfms1v1s80k";
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
          amazonka =
            self.callPackage ./nix/amazonka.nix { };
          amazonka-core =
            self.callPackage ./nix/amazonka-core.nix { };
          amazonka-kms =
            self.callPackage ./nix/amazonka-kms.nix { };
          amazonka-test =
            self.callPackage ./nix/amazonka-test.nix { };
          universum =
            self.callPackage ./nix/universum.nix { };
          servant-xml =
            self.callPackage ./nix/servant-xml.nix { };
          xmlbf-xeno =
            self.callPackage ./nix/xmlbf-xeno.nix { };
          beam-postgres =
            self.callPackage ./nix/beam-postgres.nix { };
          cryptostore =
            self.callPackage ./nix/cryptostore.nix { };
          hedis =
            self.callPackage ./nix/hedis.nix { };
          crc16 =
            self.callPackage ./nix/crc16.nix { };

          beam-mysql = pkgs.haskell.lib.disableLibraryProfiling
            (self.callCabal2nix "beam-mysql" "${beam-mysql-path}" { });

          euler-hs =
            self.callCabal2nix "euler-hs" ./lib/euler-hs { };
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
