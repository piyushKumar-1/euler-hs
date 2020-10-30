{ devShell ? false }:
let
  nix-inclusive = builtins.fetchTarball "https://github.com/manveru/nix-inclusive/archive/dd80ddcc6850d130b591fbc9b0aacacfbb3cf47c.tar.gz";
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
(builtins.getFlake (toString (builtins.unsafeDiscardStringContext path))).${attr}.x86_64-linux
