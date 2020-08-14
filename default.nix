{
  haskellCompiler ? "ghc883"
}:
let
  inherit (builtins) fromJSON readFile;

  # Date:   Mon May 18 19:30:42 2020 -0500
  nixpkgs = fetchTarball (fromJSON (readFile ./nix/nixpkgs.json));

  # To avoid importing nixpkgs here
  makeOverridable = f: origArgs:
    let
      origRes = f origArgs;
    in
      origRes // { override = newArgs: f (origArgs // newArgs); };

  eulerBuild = makeOverridable (import ./nix/euler-build) {
    inherit nixpkgs;
    inherit haskellCompiler;
  };

  beam-overlay = eulerBuild.importOverlay ./nix/overlays/beam.nix { };

  sequelize-repo = fetchGit {
    url = "git@bitbucket.org:juspay/haskell-sequelize.git";
    ref = "eulerBuild";
    # use last commit fom ref
    # rev = "";
  };
  sequelize-path = sequelize-repo;
  sequelize-drv = import sequelize-path { };


  euler-hs-src = eulerBuild.allowedPaths {
    root =  ./.;
    paths = [
      ./euler-hs.cabal
      ./src
      ./test
      ./testDB
    ];
  };
  euler-hs-overlay = eulerBuild.importOverlay ./nix/overlays/euler-hs.nix {
    src = euler-hs-src;
  };

  # for both dev and CI
  code-tools-overlay = eulerBuild.importOverlay ./nix/overlays/code-tools.nix { };
  # for dev only
  devtools-overlay = import ./nix/overlays/devtools.nix { };

  allUsedOverlays = [
    code-tools-overlay
    beam-overlay
    euler-hs-overlay
    sequelize-drv.overlay
  ];

  pkgs = import nixpkgs {
    overlays = allUsedOverlays;
  };

  haskellPackagesTools =
    with pkgs.haskellPackages;
    [
      hlint
      cabal-fmt
      nixfmt
      stylish-haskell
    ];
  tools = [];

  mkShell = eulerBuild.mkShell {
    drvPath = ./default.nix;
    drvName = "euler-hs";
    inherit haskellPackagesTools;
    inherit tools;
  };
in {
  inherit pkgs;
  euler-hs = pkgs.eulerHaskellPackages.euler-hs;

  inherit beam-overlay;
  inherit euler-hs-overlay;
  overlay = eulerBuild.composeOverlays allUsedOverlays;

  inherit code-tools-overlay;
  inherit devtools-overlay;
  inherit mkShell;

  # TODO: (?) put in a separate repo together with ./nix/euler-build
  inherit eulerBuild;

  # TODO: (?) move inside eulerBuild
  inherit nixpkgs;
}
