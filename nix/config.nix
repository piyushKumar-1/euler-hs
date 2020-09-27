{ nixpkgs }:
{
  flakeName = "euler-hs";
  defaultPackageName = "euler-hs";
  exportPackages = [
    "euler-hs"
  ];

  shellTools =
    with nixpkgs; [
      cabal-fmt
    ];
  # shellAttrs = {
  #   withHoogle = false;
  # };
}