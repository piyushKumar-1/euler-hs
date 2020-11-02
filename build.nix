{ devShell ? false }:
let
  nix-inclusive = builtins.fetchTarball "https://github.com/juspay/nix-inclusive/archive/6d7062921b2ea3911d2f95a8bd7afdf723d01b9a.tar.gz";
  filter = import "${nix-inclusive}/inclusive.nix" { lib = (import <nixpkgs> {}).lib; };
  path =
    filter ./. [
      ./src
      ./test
      ./euler-hs.cabal
      ./flake.nix
      ./flake.lock
      ./nix/overlay.nix
    ];
  attr = if devShell then "devShell" else "defaultPackage";
in
(builtins.getFlake (toString (builtins.unsafeDiscardStringContext path))).${attr}.${builtins.currentSystem}
