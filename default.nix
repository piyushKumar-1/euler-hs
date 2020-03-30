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
          beam-mysql =
            self.callPackage ./nix/beam-mysql.nix { };
          cryptostore =
            self.callPackage ./nix/cryptostore.nix { };
          hedis =
            self.callPackage ./nix/hedis.nix { };
          crc16 =
            self.callPackage ./nix/crc16.nix { };
          euler-backend-storage =
            self.callPackage ./nix/euler-backend-storage.nix { };
          # Our own packages
          euler-hs =
            self.callCabal2nix "euler-hs" ./lib/euler-hs { };
          web-service =
            self.callCabal2nix "web-service" ./lib/web-service { };
          euler-backend =
            self.callCabal2nix "euler-backend" ./app/euler-backend { };
          #credit-platform =
          #  self.callCabal2nix "credit-platform" ./app/credit-platform { };
        };
      };
    };
  };
  pkgs =
    import nixpkgs { inherit config; };
in {
  pkgs = pkgs;
  euler-hs = pkgs.haskellPackages.euler-hs;
  web-service = pkgs.haskellPackages.web-service;
 # credit-platform = pkgs.haskellPackages.credit-platform;
  euler-backend = pkgs.haskellPackages.euler-backend;
}
