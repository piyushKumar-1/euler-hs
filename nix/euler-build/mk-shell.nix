{ drvPath, drvName, haskellPackagesTools, tools }:
let
  drvArgs = args:
    builtins.removeAttrs
      args
      (builtins.attrNames (builtins.functionArgs mkShell));

  mkShell =
    args@{
      withHoogle ? true
    , ...
    }:
    let
      drv = import drvPath (drvArgs args);
      pkgs = drv.pkgs;
    in
    pkgs.eulerHaskellPackages.shellFor {
      packages = p: [ p."${drvName}" ];
      inherit withHoogle;
      buildInputs = haskellPackagesTools ++ tools;
    };
in
  mkShell
