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
          universum =
            self.callPackage ./nix/universum.nix { };
          beam-postgres =
            self.callPackage ./nix/beam-postgres.nix { };
          # We need a different upstream for purescript-bridge for now
          purescript-bridge =
            self.callPackage ./nix/purescript-bridge.nix { };
          # Our own packages
          euler-hs =
            self.callCabal2nixWithOptions "euler-hs" ./lib/euler-hs "--no-check --no-haddock" { };
          euler-backend =
            self.callCabal2nix "euler-backend" ./app/euler-backend { };
          credit-platform =
            self.callCabal2nix "credit-platform" ./app/credit-platform { };
          dashboard =
            self.callCabal2nix "dashboard" ./lib/dashboard { };
          analytics-dashboard =
            self.callCabal2nix "analytics-dashboard" ./app/analytics-dashboard { };
        };
      };
    };
  };
  pkgs =
    import nixpkgs { inherit config; };
in {
  pkgs = pkgs;
  euler-hs = pkgs.haskellPackages.euler-hs;
  credit-platform = pkgs.haskellPackages.credit-platform;
  euler-backend = pkgs.haskellPackages.euler-backend;
  dashboard = pkgs.haskellPackages.dashboard;
  analytics-dashboard = pkgs.haskellPackages.analytics-dashboard;
}
